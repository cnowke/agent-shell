;;; agent-shell-rename-acceptance.el --- Paired rename acceptance runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Run through scripts/test-buffer-renames.

;;; Code:

(require 'ert)
(require 'lisp-mode)
(require 'package)
(require 'subr-x)

(defvar agent-shell-rename-tests--steering-command)
(defvar agent-shell-rename-runtime--agent-functions)
(declare-function agent-shell-rename-runtime--from-p "agent-shell-rename-runtime" (symbol directory))

(defun agent-shell-rename-acceptance--assert-source (symbol directory)
  "Require SYMBOL's loaded definition to come from DIRECTORY."
  (let ((source (symbol-file symbol 'defun)))
    (unless (agent-shell-rename-runtime--from-p symbol directory)
      (error "%s loaded from %s; expected %s" symbol source directory))
    (princ (format "%s: %s\n" symbol source))))

(defun agent-shell-rename-acceptance--revision (directory)
  "Print DIRECTORY's Git revision and working tree status."
  (unless (file-exists-p (expand-file-name ".git" directory))
    (error "Expected a Git checkout at %s" directory))
  (with-temp-buffer
    (unless (zerop (process-file "git" nil t nil "-C" directory "rev-parse" "HEAD"))
      (error "Not a Git checkout: %s" directory))
    (princ (format "Checkout %s: %s" directory (buffer-string)))
    (erase-buffer)
    (unless (zerop (process-file "git" nil t nil "-C" directory "status" "--porcelain"))
      (error "Cannot inspect working tree: %s" directory))
    (unless (zerop (buffer-size))
      (princ (format "Working tree changes:\n%s" (buffer-string))))))

(defun agent-shell-rename-acceptance--forms (file predicate)
  "Find forms in FILE satisfying PREDICATE without evaluating the file."
  (let ((matches nil))
    (with-temp-buffer
      (insert-file-contents file)
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (goto-char (point-min))
      (while (progn (forward-comment (point-max)) (not (eobp)))
        (let ((pending (list (read (current-buffer)))))
          (while pending
            (let ((form (pop pending)))
              (when (consp form)
                (if (funcall predicate form)
                    (push form matches)
                  (unless (memq (car form) '(quote function))
                    (push (cdr form) pending)
                    (push (car form) pending)))))))))
    (nreverse matches)))

(defun agent-shell-rename-acceptance--binding (file)
  "Return FILE's Agent Shell C-<return> command, loading any local definition."
  (let* ((bindings
          (agent-shell-rename-acceptance--forms
           file (lambda (form)
                  (and (eq (car form) 'define-key)
                       (eq (nth 1 form) 'agent-shell-mode-map)
                       (equal (nth 2 form) '(kbd "C-<return>"))))))
         (command (pcase (nth 3 (car bindings))
                    ((or `(function ,command) `(quote ,command))
                     (and (symbolp command) command))))
         (definitions
          (and (symbolp command) command
               (agent-shell-rename-acceptance--forms
                file (lambda (form)
                       (and (eq (car form) 'defun)
                            (eq (nth 1 form) command)))))))
    (unless (and (= (length bindings) 1) (= (length (car bindings)) 4) command)
      (error "Expected one literal agent-shell C-<return> binding without REMOVE in %s" file))
    (when (> (length definitions) 1)
      (error "Multiple definitions of %s in %s" command file))
    (when definitions
      (eval (car definitions) t))
    (unless (commandp command)
      (error "C-<return> does not name an interactive command: %s" command))
    command))

(defun agent-shell-rename-acceptance-run ()
  "Run all required rename workflows against both configured checkouts."
  (unless noninteractive
    (user-error "Run scripts/test-buffer-renames in a separate Emacs process"))
  (let* ((root (file-truename (getenv "AGENT_SHELL_RENAME_ROOT")))
         (shell-maker (file-truename (getenv "AGENT_SHELL_RENAME_SHELL_MAKER")))
         (acp (getenv "AGENT_SHELL_RENAME_ACP"))
         (init (getenv "AGENT_SHELL_RENAME_INIT")))
    (require 'agent-shell-rename-runtime
             (expand-file-name "scripts/agent-shell-rename-runtime.el" root))
    (package-initialize)
    (setq load-prefer-newer t)
    (unless (string-empty-p acp)
      (add-to-list 'load-path (expand-file-name acp)))
    (add-to-list 'load-path shell-maker)
    (add-to-list 'load-path root)
    (require 'agent-shell)
    (require 'acp-fakes)
    (princ (format "Emacs: %s\n" emacs-version))
    (dolist (entry agent-shell-rename-runtime--agent-functions)
      (agent-shell-rename-acceptance--assert-source (cdr entry) root))
    (agent-shell-rename-acceptance--assert-source 'shell-maker--process shell-maker)
    (princ (format "ACP: %s\nACP fake server: %s\n"
                   (symbol-file 'acp-send-request) (symbol-file 'acp-fakes-make-client)))
    (agent-shell-rename-acceptance--revision root)
    (agent-shell-rename-acceptance--revision shell-maker)
    (unless (string-empty-p acp)
      (agent-shell-rename-acceptance--assert-source 'acp-send-request acp)
      (agent-shell-rename-acceptance--assert-source 'acp-fakes-make-client acp)
      (when (file-exists-p (expand-file-name ".git" acp))
        (agent-shell-rename-acceptance--revision (expand-file-name acp))))
    (load (expand-file-name "tests/shell-maker-rename-tests.el" shell-maker) nil t)
    (load (expand-file-name "tests/agent-shell-rename-tests.el" root) nil t)
    (if (string-empty-p init)
        (princ "C-<return>: agent-shell-submit-override (fixture binding; use --init to check yours)\n")
      (let ((file (expand-file-name init)))
        (setq agent-shell-rename-tests--steering-command
              (agent-shell-rename-acceptance--binding file))
        (princ (format "C-<return>: %s, extracted from %s\n"
                       agent-shell-rename-tests--steering-command file))))
    (dolist (test '(shell-maker-rename-raw-buffer-keeps-process-test
                    shell-maker-rename-streaming-output-test
                    agent-shell-rename-raw-steering-test
                    agent-shell-rename-viewport-observers-see-current-process-test
                    agent-shell-rename-acceptance-submit-stream-test
                    agent-shell-rename-acceptance-steering-test
                    agent-shell-rename-acceptance-viewport-test
                    agent-shell-rename-acceptance-interrupt-test
                    agent-shell-rename-acceptance-restart-reload-test
                    agent-shell-rename-tooling-reads-command-without-running-init-test
                    agent-shell-rename-tooling-rejects-truncated-init-test
                    agent-shell-rename-tooling-large-init-test
                    agent-shell-rename-tooling-existing-command-test
                    agent-shell-rename-tooling-rejects-unusable-binding-test
                    agent-shell-rename-tooling-runtime-ignores-autoloads-test
                    agent-shell-rename-tooling-runtime-leaves-stale-shell-untouched-test))
      (unless (ert-test-boundp test)
        (error "Required acceptance test is missing: %s" test)))
    (let ((stats (ert-run-tests-batch "^\\(agent-shell-rename-\\|shell-maker-rename-\\)")))
      (when (> (ert-stats-skipped stats) 0)
        (princ (format "FAIL: %d required tests were skipped\n" (ert-stats-skipped stats))))
      (let ((passed (and (zerop (ert-stats-completed-unexpected stats))
                         (zerop (ert-stats-skipped stats)))))
        (princ (format "%s: %d rename tests, %d unexpected results, %d skips\n"
                       (if passed "PASS" "FAIL") (ert-stats-total stats)
                       (ert-stats-completed-unexpected stats) (ert-stats-skipped stats)))
        (kill-emacs (if passed 0 1))))))

(provide 'agent-shell-rename-acceptance)

;;; agent-shell-rename-acceptance.el ends here
