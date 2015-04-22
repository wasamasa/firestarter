;;; firestarter.el --- Execute (shell) commands on save

;; Copyright (C) 2015 Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: Vasilij Schneidermann <v.schneidermann@gmail.com>
;; URL: https://github.com/wasamasa/firestarter
;; Version: 0.0.2
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This global minor mode allows you to run (shell) commands on save.

;; See the README for more info:
;; https://github.com/wasamasa/firestarter

;;; Code:

(require 'format-spec)

(defgroup firestarter nil
  "Execute shell commands on save."
  :group 'convenience
  :prefix "firestarter-")

(defcustom firestarter-lighter " 🔥"
  "Lighter for `firestarter-mode'."
  :type 'string
  :group 'firestarter)

(defvar firestarter nil
  "Command to run on file save.
A string value is interpreted as shell command and passed to an
asynchronous subprocess.  A symbol value is interpreted as
command and executed interactively.  A list value is interpreted
as code and evaluated.")
(make-variable-buffer-local 'firestarter)

(defcustom firestarter-default-type 'silent
  "Default shell command reporting type.
It may be one of the following values:

nil, 'silent: Don't report anything at all.

'success: Report on successful execution (return code equals zero).

'failure: Report on failed execution (return code equals non-zero).

t, 'finished: Report after either outcome once the subprocess quit."
  :type '(choice (const :tag "Silent" silent)
                 (const :tag "Success" success)
                 (const :tag "Failure" failure)
                 (const :tag "Finished" finished))
  :group 'firestarter)

(defvar firestarter-type nil
  "Current shell command reporting type.
See `firestarter-default-type' for valid values.")
(make-variable-buffer-local 'firestarter-type)

(defvar firestarter-process nil
  "Process associated with current buffer.")
(make-variable-buffer-local 'firestarter-process)

(defvar firestarter-process-output ""
  "Output associated with `firestarter-process'.")
(make-variable-buffer-local 'firestarter-process-output)

(defvar firestarter-process-busy nil
  "Non-nil if `firestarter-process' is still running.
When nil, `firestarter-process' either wasn't started yet or did
quit already.")
(make-variable-buffer-local 'firestarter-process-busy)

(defcustom firestarter-buffer-name "*firestarter*"
  "Buffer name of the reporting buffer for shell commands."
  :type 'string
  :group 'firestarter)

(defun firestarter-command (command &optional type)
  "Execute COMMAND in a shell.
Optionally, override the reporting type as documented in
`firestarter-default-type' with TYPE."
  (if firestarter-process-busy
      (error "Process already running")
    (setq firestarter-process
          (start-process "firestarter" nil
                         shell-file-name shell-command-switch
                         (firestarter-format command)))
    (setq firestarter-process-output ""
          firestarter-process-busy t)
    ;; if type is given, override firestarter-type, otherwise set it to
    ;; the default if it's unset
    (setq firestarter-type
          (or type firestarter-type firestarter-default-type))
    ;; KLUDGE process output filters aren't run with the buffer they
    ;; originate from selected, that's why the buffer associated with
    ;; the process is stored in a process property
    (process-put firestarter-process 'buffer (current-buffer))
    (set-process-filter firestarter-process 'firestarter-filter)
    (set-process-sentinel firestarter-process 'firestarter-sentinel)))

(defun firestarter-format (string)
  "Apply format codes on STRING.
Available format codes are:

%b: Buffer name.  Equals the file name for buffers linked with
 files.  Beware that this is merely convention and buffers can be
 renamed to conform to their unique name constraint!

%p: Full path of the file associated with the buffer.  Decomposes
 into a directory and file name part.  If there is no file
 association, the value is an empty string.  As the following
 format codes are directly derived from this value, the same
 caveat applies to them as well.

%d: Directory name of the file associated with the buffer.
 Equals the full path without the file name.

%f: File name of the file associated with the buffer.  Decomposes
 into a file stem and a file extension.

%s: File stem of the file associated with the buffer.  Equals the
 file name without its extension.

%e: File extension of the file associated with the buffer.
 Equals the file name without its stem.  Includes the period if
 an extension is present, otherwise the value is an empty
 string."
  (let* ((buffer (buffer-name))
         (path (or (buffer-file-name) ""))
         (file (file-name-nondirectory (or path "")))
         (stem (file-name-sans-extension file))
         (extension (file-name-extension file t)))
    (format-spec string (format-spec-make ?b buffer ?p path ?f file
                                          ?s stem ?e extension))))

(defun firestarter-filter (process output)
  "Special process filter.
It retrieves the associated buffer for PROCESS, then extends its
value of `firestarter-process-output' with OUTPUT.  The reason
for using a string instead of the default process filter and a
buffer is due to buffers not being subject to garbage collection.
Asides from that it is not possible to have a buffer-local
buffer, a buffer-local string however is no problem."
  (let ((buffer (process-get process 'buffer)))
    (with-current-buffer buffer
      (setq firestarter-process-output
            (concat firestarter-process-output output)))))

(defun firestarter-sentinel (process _type)
  "Special process sentinel.
It retrieves the status of PROCESS, then sets up and displays the
reporting buffer according to `firestarter-type'."
  (let ((buffer (process-get process 'buffer)))
    (with-current-buffer buffer
      (let ((status (process-status process)))
        (when (memq status '(exit signal))
          (setq firestarter-process-busy nil)
          (unless (or (eq firestarter-type 'silent) (not firestarter-type))
            (let ((return-code (process-exit-status process)))
              (firestarter-setup-buffer process return-code)
              (when (or (and (eq firestarter-type 'success) (= return-code 0))
                        (and (eq firestarter-type 'failure) (/= return-code 0))
                        (memq firestarter-type '(finished t)))
                (display-buffer "*firestarter*")))))))))

(defun firestarter-setup-buffer (process return-code)
  "Setup the reporting buffer.
Retrieve the associated buffer for PROCESS, then format its
output and incorporate RETURN-CODE into the report."
  (let ((buffer (process-get process 'buffer))
        (target (get-buffer-create "*firestarter*"))
        (output firestarter-process-output))
    (with-current-buffer target
      (let ((inhibit-read-only t))
        (view-mode)
        (goto-char (point-max))
        (insert (propertize
                 (format "%s (%d):" (buffer-name buffer) return-code)
                 'face 'highlight)
                "\n\n" output "\n"
                (propertize "---" 'face 'shadow) "\n\n")))))

(defun firestarter ()
  "Hook function run after save.
It dispatches upon the value type of `firestarter'."
  (interactive)
  (when firestarter
    (cond
     ((stringp firestarter)
      (firestarter-command firestarter))
     ((symbolp firestarter)
      (call-interactively firestarter))
     ((listp firestarter)
      (eval firestarter))
     (t (error "Invalid value for `firestarter': %s" firestarter)))))

(defun firestarter-update-processes ()
  "Hook function for updating the process->buffer associations.
It is run to accomodate for the case of buffer renaming which
would invalidate the corresponding process attribute."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when firestarter-process
        (process-put firestarter-process 'buffer buffer)))))

(defun firestarter-abort ()
  "Abort the currently active firestarter process."
  (interactive)
  (when firestarter-process
    (delete-process firestarter-process)))

;;;###autoload
(define-minor-mode firestarter-mode
  "Toggle `firestarter-mode'.
When activated, run a command as specified in the buffer-local
`firestarter' variable on every file save."
  :lighter firestarter-lighter
  :global t
  (if firestarter-mode
      (progn
        (add-hook 'after-save-hook 'firestarter)
        (add-hook 'buffer-list-update-hook 'firestarter-update-processes))
    (remove-hook 'after-save-hook 'firestarter)
    (remove-hook 'buffer-list-update-hook 'firestarter-update-processes)))

(provide 'firestarter)
;;; firestarter.el ends here
