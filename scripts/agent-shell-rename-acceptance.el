;;; agent-shell-rename-acceptance.el --- Paired rename acceptance runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Run through scripts/test-buffer-renames.

;;; Code:

(require 'ert)
(require 'package)
(require 'subr-x)

(defvar agent-shell-rename-tests--steering-command)

(defun agent-shell-rename-acceptance--assert-source (symbol directory)
  "Require SYMBOL's loaded definition to come from DIRECTORY."
  (let ((source (symbol-file symbol 'defun)))
    (unless (and source
                 (not (autoloadp (symbol-function symbol)))
                 (file-in-directory-p (file-truename source)
                                      (file-truename directory)))
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
      (goto-char (point-min))
      (letrec ((visit (lambda (form)
                        (when (consp form)
                          (if (funcall predicate form)
                              (push form matches)
                            (unless (memq (car form) '(quote function))
                              (funcall visit (car form))
                              (funcall visit (cdr form))))))))
        (condition-case nil
            (while t (funcall visit (read (current-buffer))))
          (end-of-file nil))))
    (nreverse matches)))

(defun agent-shell-rename-acceptance--binding (file)
  "Load only FILE's Agent Shell C-<return> command definition."
  (let* ((bindings
          (agent-shell-rename-acceptance--forms
           file (lambda (form)
                  (and (eq (car form) 'define-key)
                       (eq (nth 1 form) 'agent-shell-mode-map)
                       (equal (nth 2 form) '(kbd "C-<return>"))))))
         (command (nth 1 (nth 3 (car bindings))))
         (definitions
          (and (symbolp command) command
               (agent-shell-rename-acceptance--forms
                file (lambda (form)
                       (and (eq (car form) 'defun)
                            (eq (nth 1 form) command)))))))
    (unless (and (= (length bindings) 1)
                 (memq (car-safe (nth 3 (car bindings))) '(function quote))
                 (= (length definitions) 1))
      (error "Expected one agent-shell C-<return> binding and its defun in %s" file))
    (eval (car definitions) t)
    (unless (commandp command)
      (error "C-<return> does not name an interactive command: %s" command))
    (setq agent-shell-rename-tests--steering-command command)
    (princ (format "C-<return>: %s, extracted from %s\n" command file))))

(defun agent-shell-rename-acceptance-run ()
  "Run all required rename workflows against both configured checkouts."
  (unless noninteractive
    (user-error "Run scripts/test-buffer-renames in a separate Emacs process"))
  (let* ((root (file-truename (getenv "AGENT_SHELL_RENAME_ROOT")))
         (shell-maker (file-truename (getenv "AGENT_SHELL_RENAME_SHELL_MAKER")))
         (acp (getenv "AGENT_SHELL_RENAME_ACP"))
         (init (getenv "AGENT_SHELL_RENAME_INIT")))
    (package-initialize)
    (setq load-prefer-newer t)
    (unless (string-empty-p acp)
      (add-to-list 'load-path (expand-file-name acp)))
    (add-to-list 'load-path shell-maker)
    (add-to-list 'load-path root)
    (require 'agent-shell)
    (require 'acp-fakes)
    (princ (format "Emacs: %s\n" emacs-version))
    (agent-shell-rename-acceptance--assert-source 'agent-shell-submit root)
    (agent-shell-rename-acceptance--assert-source 'agent-shell-viewport--shell-buffer root)
    (agent-shell-rename-acceptance--assert-source 'agent-shell-experimental--send-steering root)
    (agent-shell-rename-acceptance--assert-source 'agent-shell--prompt-input root)
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
      (agent-shell-rename-acceptance--binding (expand-file-name init)))
    (dolist (test '(shell-maker-rename-raw-buffer-keeps-process-test
                    shell-maker-rename-streaming-output-test
                    agent-shell-rename-raw-steering-test
                    agent-shell-rename-viewport-observers-see-current-process-test
                    agent-shell-rename-acceptance-submit-stream-test
                    agent-shell-rename-acceptance-steering-test
                    agent-shell-rename-acceptance-viewport-test
                    agent-shell-rename-acceptance-interrupt-test
                    agent-shell-rename-acceptance-restart-reload-test))
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
