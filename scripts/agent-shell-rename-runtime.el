;;; agent-shell-rename-runtime.el --- Read-only rename diagnostics -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this file manually and run agent-shell-rename-runtime-report.
;; No agent packages are loaded and no shell state is changed.

;;; Code:

(require 'map)
(require 'subr-x)

(defconst agent-shell-rename-runtime--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Checkout containing this diagnostic file.")

(defun agent-shell-rename-runtime--source (symbol)
  "Return SYMBOL's loaded source file, excluding autoload definitions."
  (when (and (fboundp symbol) (not (autoloadp (symbol-function symbol))))
    (symbol-file symbol 'defun)))

(defun agent-shell-rename-runtime--revision (directory)
  "Describe the Git revision on disk in DIRECTORY."
  (if (not (and (file-exists-p (expand-file-name ".git" directory))
                (executable-find "git")))
      "unavailable"
    (let ((default-directory directory))
      (with-temp-buffer
        (if (not (zerop (process-file "git" nil t nil "rev-parse" "HEAD")))
            "unavailable"
          (let ((revision (string-trim (buffer-string))))
            (erase-buffer)
            (if (not (zerop (process-file "git" nil t nil "status" "--porcelain")))
                (concat revision " (working tree status unavailable)")
              (concat revision (if (zerop (buffer-size)) " (clean)" " (modified)")))))))))

(defun agent-shell-rename-runtime--from-p (symbol directory)
  "Return non-nil when SYMBOL is loaded from DIRECTORY."
  (when-let* ((source (agent-shell-rename-runtime--source symbol)))
    (file-in-directory-p (file-truename source) (file-truename directory))))

(defun agent-shell-rename-runtime-report (&optional shell-maker-directory)
  "Report loaded package paths and rename support without changing shell state.
SHELL-MAKER-DIRECTORY defaults to the sibling shell-maker checkout.
Interactively, print the report to the echo area and *Messages*.
The returned string can also be printed from an isolated batch process."
  (interactive)
  (let* ((shell-maker (or shell-maker-directory
                          (expand-file-name "../shell-maker" agent-shell-rename-runtime--root)))
         (shell (cond
                 ((and (derived-mode-p 'agent-shell-mode)
                       (local-variable-p 'agent-shell--state))
                  (current-buffer))
                 ((and (local-variable-p 'agent-shell-viewport--shell-buffer)
                       (buffer-live-p (symbol-value 'agent-shell-viewport--shell-buffer)))
                  (symbol-value 'agent-shell-viewport--shell-buffer))))
         (report
          (concat
           (format "Emacs: %s
agent-shell: %s
viewport: %s
steering: %s
prompt: %s
shell-maker: %s
ACP: %s
"
                   emacs-version
                   (or (agent-shell-rename-runtime--source 'agent-shell-submit) "not loaded")
                   (or (agent-shell-rename-runtime--source 'agent-shell-viewport--shell-buffer) "not loaded")
                   (or (agent-shell-rename-runtime--source 'agent-shell-experimental--send-steering) "not loaded")
                   (or (agent-shell-rename-runtime--source 'agent-shell--prompt-input) "not loaded")
                   (or (agent-shell-rename-runtime--source 'shell-maker--process) "not loaded")
                   (or (agent-shell-rename-runtime--source 'acp-send-request) "not loaded"))
           (format "Paths match checkouts: %s
agent-shell checkout: %s
shell-maker checkout: %s
"
                   (if (and (agent-shell-rename-runtime--from-p 'agent-shell-submit agent-shell-rename-runtime--root)
                            (agent-shell-rename-runtime--from-p 'agent-shell-viewport--shell-buffer agent-shell-rename-runtime--root)
                            (agent-shell-rename-runtime--from-p 'agent-shell-experimental--send-steering agent-shell-rename-runtime--root)
                            (agent-shell-rename-runtime--from-p 'agent-shell--prompt-input agent-shell-rename-runtime--root)
                            (agent-shell-rename-runtime--from-p 'shell-maker--process shell-maker))
                       "yes" "no")
                   (agent-shell-rename-runtime--revision agent-shell-rename-runtime--root)
                   (agent-shell-rename-runtime--revision shell-maker))
           "Checkout revisions describe files on disk; loaded definitions may be older.\n"
           (format "Current C-<return> binding: %S\n" (key-binding (kbd "C-<return>")))
           (if (not shell)
               "No shell selected; run this command from the affected agent-shell buffer.\n"
             (with-current-buffer shell
               (let* ((state (buffer-local-value 'agent-shell--state shell))
                      (viewport (map-elt state :viewport-buffer))
                      (stored-name
                       (and (agent-shell-rename-runtime--source 'shell-maker-buffer-name)
                            (boundp 'shell-maker--config)
                            (funcall 'shell-maker-buffer-name
                                     (buffer-local-value 'shell-maker--config shell)))))
                 (format "Shell: %s
Stored shell name: %s
Name synchronized: %s
Rename hook installed: %s
State owns this buffer: %s
Viewport owns this shell: %s
Process live: %s
"
                         (buffer-name) stored-name
                         (if (equal stored-name (buffer-name)) "yes" "no")
                         (if (memq 'shell-maker--sync-buffer-name
                                   (buffer-local-value 'buffer-list-update-hook shell)) "yes" "no")
                         (if (eq shell (map-elt state :buffer)) "yes" "no")
                         (if (buffer-live-p viewport)
                             (if (and (local-variable-p 'agent-shell-viewport--shell-buffer viewport)
                                      (eq shell (buffer-local-value 'agent-shell-viewport--shell-buffer viewport)))
                                 "yes" "no")
                           "no viewport")
                         (if (process-live-p (get-buffer-process shell)) "yes" "no"))))))))
    (when (called-interactively-p 'interactive)
      (message "%s" report))
    report))

(provide 'agent-shell-rename-runtime)

;;; agent-shell-rename-runtime.el ends here
