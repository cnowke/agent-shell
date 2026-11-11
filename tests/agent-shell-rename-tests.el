;;; agent-shell-rename-tests.el --- Tests for buffer renaming -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'agent-shell)

(defvar agent-shell-rename-tests--shells nil)

(defun agent-shell-rename-tests--shell (&optional start-function)
  "Create and track a shell without starting an ACP client.
Use START-FUNCTION to select the shell startup function."
  (let ((shell (cl-letf (((symbol-function 'agent-shell--handle) #'ignore))
                 (funcall (or start-function #'agent-shell--start)
                          :config (agent-shell-make-agent-config
                                   :identifier 'rename-test
                                   :buffer-name "Test"
                                   :mode-line-name "Test"
                                   :shell-prompt "Agent> "
                                   :shell-prompt-regexp "^Agent> "
                                   :client-maker (lambda (_buffer) '((:command . "cat"))))
                          :session-strategy 'new :new-session t :no-focus t))))
    (push shell agent-shell-rename-tests--shells)
    shell))

(defmacro agent-shell-rename-tests--with-shells (count &rest body)
  "Run BODY with COUNT shells in a temporary directory."
  (declare (indent 1) (debug t))
  `(let* ((directory (make-temp-file "agent-shell-rename-" t))
          (default-directory (file-name-as-directory directory))
          (shell-maker-root-path default-directory)
          (agent-shell-header-style nil)
          (agent-shell-show-welcome-message nil)
          (agent-shell-chat-mode-enabled nil)
          (agent-shell-prefer-viewport-interaction nil)
          (agent-shell-viewport--clean-up nil)
          (agent-shell-rename-tests--shells nil))
     (unwind-protect
         (progn
           (dotimes (_ ,count)
             (agent-shell-rename-tests--shell))
           (setq agent-shell-rename-tests--shells
                 (nreverse agent-shell-rename-tests--shells))
           ,@body)
       (dolist (buffer agent-shell-rename-tests--shells)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (let ((kill-buffer-query-functions nil))
               (set-buffer-modified-p nil)
               (kill-buffer buffer)))))
       (delete-directory directory t))))

(ert-deftest agent-shell-rename-preserves-custom-binding-test ()
  "Starting another shell preserves a customized rename command binding."
  (let ((agent-shell-mode-map (copy-keymap agent-shell-mode-map)))
    (keymap-set agent-shell-mode-map "<remap> <rename-buffer>" #'ignore)
    (agent-shell-rename-tests--with-shells 1
      (with-current-buffer (car agent-shell-rename-tests--shells)
        (should (eq #'ignore (command-remapping #'rename-buffer)))))))

(ert-deftest agent-shell-rename-test-cleanup-keeps-unrelated-buffer-test ()
  "The fixture cleans up its shells without killing unrelated buffers."
  (let ((unrelated nil)
        (shell nil)
        (viewport nil)
        (process nil))
    (unwind-protect
        (progn
          (agent-shell-rename-tests--with-shells 1
            (setq shell (car agent-shell-rename-tests--shells)
                  viewport (agent-shell-viewport--buffer :shell-buffer shell)
                  process (get-buffer-process shell))
            (setq unrelated (generate-new-buffer " *unrelated-buffer*"))
            (with-current-buffer unrelated (insert "keep this content")))
          (should-not (buffer-live-p shell))
          (should-not (buffer-live-p viewport))
          (should-not (process-live-p process))
          (should (buffer-live-p unrelated))
          (should (equal "keep this content"
                         (with-current-buffer unrelated (buffer-string)))))
      (when (buffer-live-p unrelated)
        (kill-buffer unrelated)))))

(ert-deftest agent-shell-rename-existing-only-without-shell-test ()
  "Checking for an existing viewport does not require an initialized shell."
  (with-temp-buffer
    (should-not (agent-shell-viewport--buffer
                 :shell-buffer (current-buffer) :existing-only t))))

(ert-deftest agent-shell-rename-keeps-viewport-draft-test ()
  "The supported setter keeps the same viewport and its unsent draft."
  (agent-shell-rename-tests--with-shells 1
    (let* ((shell (car agent-shell-rename-tests--shells))
           (viewport (agent-shell-viewport--buffer :shell-buffer shell)))
      (with-current-buffer viewport (insert "draft to preserve"))
      (shell-maker-set-buffer-name shell " *renamed-shell*")
      (should (eq shell (agent-shell-viewport--shell-buffer viewport)))
      (should (eq viewport (agent-shell-viewport--buffer :shell-buffer shell)))
      (should (equal (concat (buffer-name shell) agent-shell-viewport--suffix)
                     (buffer-name viewport)))
      (should (equal (with-current-buffer viewport (buffer-string))
                     "draft to preserve")))))

(ert-deftest agent-shell-rename-raw-shell-keeps-viewport-test ()
  "A direct shell rename keeps its viewport attached and updates its label."
  (agent-shell-rename-tests--with-shells 1
    (let* ((shell (car agent-shell-rename-tests--shells))
           (viewport (agent-shell-viewport--buffer :shell-buffer shell)))
      (with-current-buffer shell (rename-buffer " *raw-renamed-shell*" t))
      (should (eq shell (agent-shell-viewport--shell-buffer viewport)))
      (should (eq viewport (agent-shell-viewport--buffer :shell-buffer shell)))
      (should (equal (concat (buffer-name shell) agent-shell-viewport--suffix)
                     (buffer-name viewport))))))

(ert-deftest agent-shell-rename-reused-name-keeps-target-test ()
  "Reusing a shell's old name cannot retarget its viewport."
  (agent-shell-rename-tests--with-shells 2
    (let* ((first (car agent-shell-rename-tests--shells))
           (second (cadr agent-shell-rename-tests--shells))
           (old-name (buffer-name first))
           (viewport (agent-shell-viewport--buffer :shell-buffer first)))
      (shell-maker-set-buffer-name first " *renamed-shell*")
      (shell-maker-set-buffer-name second old-name)
      (should (eq first (agent-shell-viewport--shell-buffer viewport)))
      (with-current-buffer viewport
        (should (eq first (agent-shell--shell-buffer :no-create t)))
        (should (eq first (agent-shell--current-shell)))))))

(ert-deftest agent-shell-rename-viewport-name-and-mode-changes-test ()
  "Viewport associations survive arbitrary names and major-mode switches."
  (agent-shell-rename-tests--with-shells 1
    (let* ((shell (car agent-shell-rename-tests--shells))
           (viewport (agent-shell-viewport--buffer :shell-buffer shell)))
      (with-current-buffer viewport
        (rename-buffer " *arbitrary-viewport-name*" t)
        (agent-shell-viewport-view-mode)
        (should (eq shell (agent-shell-viewport--shell-buffer)))
        (agent-shell-viewport-edit-mode)
        (should (eq shell (agent-shell--current-shell))))
      (should (eq viewport (agent-shell-viewport--buffer :shell-buffer shell))))))

(ert-deftest agent-shell-rename-occupied-viewport-name-test ()
  "An unrelated buffer with the viewport's name remains untouched."
  (agent-shell-rename-tests--with-shells 1
    (let* ((shell (car agent-shell-rename-tests--shells))
           (occupied (generate-new-buffer
                      (concat (buffer-name shell) agent-shell-viewport--suffix))))
      (unwind-protect
          (progn
            (with-current-buffer occupied (insert "unrelated contents"))
            (let ((viewport (agent-shell-viewport--buffer :shell-buffer shell)))
              (should-not (eq occupied viewport))
              (should (equal "unrelated contents"
                             (with-current-buffer occupied (buffer-string))))
              (should (eq shell (agent-shell-viewport--shell-buffer viewport)))
              (should (eq viewport (agent-shell-viewport--buffer
                                    :shell-buffer shell :existing-only t)))))
        (kill-buffer occupied)))))

(ert-deftest agent-shell-rename-collisions-and-repetition-test ()
  "Collisions and repeated renames preserve both processes and drafts."
  (agent-shell-rename-tests--with-shells 2
    (let* ((first (car agent-shell-rename-tests--shells))
           (second (cadr agent-shell-rename-tests--shells))
           (first-view (agent-shell-viewport--buffer :shell-buffer first))
           (second-view (agent-shell-viewport--buffer :shell-buffer second))
           (process (get-buffer-process first))
           (occupied (generate-new-buffer " *collision* [viewport]")))
      (with-current-buffer first-view (insert "first draft"))
      (with-current-buffer second-view (insert "second draft"))
      (unwind-protect
          (progn
            (dotimes (_ 8)
              (dolist (shell (list first second))
                (shell-maker-set-buffer-name shell " *collision*")
                (with-current-buffer shell
                  (should (equal (buffer-name) shell-maker--buffer-name-override))
                  (should (process-live-p (shell-maker--process))))))
            (should (eq process (get-buffer-process first)))
            (should (eq first (agent-shell-viewport--shell-buffer first-view)))
            (should (eq second (agent-shell-viewport--shell-buffer second-view)))
            (should (equal "first draft" (with-current-buffer first-view (buffer-string))))
            (should (equal "second draft" (with-current-buffer second-view (buffer-string)))))
        (kill-buffer occupied)))))

(ert-deftest agent-shell-rename-dead-shell-does-not-fall-back-test ()
  "An orphaned viewport cannot select another shell from the same project."
  (agent-shell-rename-tests--with-shells 2
    (let* ((shell (car agent-shell-rename-tests--shells))
           (second (cadr agent-shell-rename-tests--shells))
           (old-name (buffer-name shell))
           (viewport (agent-shell-viewport--buffer :shell-buffer shell)))
      (unwind-protect
          (progn
            (with-current-buffer shell
              (remove-hook 'kill-buffer-hook #'agent-shell--clean-up t))
            (kill-buffer shell)
            (shell-maker-set-buffer-name second old-name)
            (with-current-buffer viewport
              (should-not (agent-shell-viewport--shell-buffer))
              (should-not (agent-shell--current-shell))
              (should-not (agent-shell--shell-buffer :no-error t))
              (should-not (agent-shell--shell-buffer :no-error t :no-create t))
              (should-error (agent-shell--shell-buffer) :type 'user-error)
              (should-error (agent-shell--shell-buffer :no-create t) :type 'user-error))
            (should-error (agent-shell--shell-buffer :viewport-buffer viewport :no-create t)
                          :type 'user-error))
        (when (buffer-live-p viewport)
          (kill-buffer viewport))))))

(ert-deftest agent-shell-rename-killed-viewport-can-be-recreated-test ()
  "A killed viewport is replaced without changing the shell process."
  (agent-shell-rename-tests--with-shells 1
    (let* ((shell (car agent-shell-rename-tests--shells))
           (process (get-buffer-process shell))
           (viewport (agent-shell-viewport--buffer :shell-buffer shell)))
      (kill-buffer viewport)
      (should-not (agent-shell-viewport--buffer :shell-buffer shell :existing-only t))
      (let ((replacement (agent-shell-viewport--buffer :shell-buffer shell)))
        (should-not (eq viewport replacement))
        (should (eq shell (agent-shell-viewport--shell-buffer replacement))))
      (should (eq process (get-buffer-process shell))))))

(ert-deftest agent-shell-rename-shell-cleanup-finds-viewport-test ()
  "Shell cleanup kills its viewport even when its name has changed."
  (agent-shell-rename-tests--with-shells 1
    (let* ((shell (car agent-shell-rename-tests--shells))
           (viewport (agent-shell-viewport--buffer :shell-buffer shell)))
      (with-current-buffer viewport (rename-buffer " *custom-viewport*" t))
      (kill-buffer shell)
      (should-not (buffer-live-p viewport)))))

(ert-deftest agent-shell-rename-command-from-viewport-test ()
  "The rename command selects the correct shell when used in a viewport."
  (agent-shell-rename-tests--with-shells 2
    (let* ((shell (car agent-shell-rename-tests--shells))
           (other (cadr agent-shell-rename-tests--shells))
           (other-name (buffer-name other))
           (viewport (agent-shell-viewport--buffer :shell-buffer shell)))
      (with-current-buffer shell
        (should (eq #'agent-shell-rename-buffer (command-remapping #'rename-buffer))))
      (with-current-buffer viewport
        (insert "keep draft")
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) "  *command-renamed*  ")))
          (call-interactively #'agent-shell-rename-buffer)))
      (should (equal (buffer-name shell) "*command-renamed*"))
      (should (equal (buffer-name other) other-name))
      (should (equal "keep draft" (with-current-buffer viewport (buffer-string))))
      (should (eq viewport (agent-shell-viewport--buffer :shell-buffer shell))))))

(ert-deftest agent-shell-rename-viewport-cleanup-targets-associated-shell-test ()
  "Closing a renamed viewport only offers to kill its associated shell."
  (dolist (accept '(nil t))
    (let ((shutdown-clients nil)
          (prompts 0))
      (cl-letf (((symbol-function 'acp-shutdown)
                 (lambda (&rest arguments)
                   (push (plist-get arguments :client) shutdown-clients)))
                ((symbol-function 'y-or-n-p)
                 (lambda (_prompt)
                   (setq prompts (1+ prompts))
                   accept)))
        (agent-shell-rename-tests--with-shells 2
          (let* ((shell (car agent-shell-rename-tests--shells))
                 (other (cadr agent-shell-rename-tests--shells))
                 (other-process (get-buffer-process other))
                 (viewport (agent-shell-viewport--buffer :shell-buffer shell)))
            (shell-maker-set-buffer-name shell " *cleanup-renamed*")
            (with-current-buffer shell
              (map-put! agent-shell--state :client 'rename-test-client))
            (save-window-excursion
              (switch-to-buffer viewport)
              (rename-buffer " *renamed-viewport-to-close*" t)
              (let ((agent-shell-viewport--clean-up t))
                (kill-buffer viewport)))
            (should (= prompts 1))
            (should-not (buffer-live-p viewport))
            (should (eq (buffer-live-p shell) (not accept)))
            (should (equal shutdown-clients (when accept '(rename-test-client))))
            (should (buffer-live-p other))
            (should (eq other-process (get-buffer-process other)))))))))

(ert-deftest agent-shell-rename-invalid-name-preserves-session-test ()
  "Rejected names leave the process, viewport, and draft intact."
  (agent-shell-rename-tests--with-shells 2
    (let* ((shell (car agent-shell-rename-tests--shells))
           (other (cadr agent-shell-rename-tests--shells))
           (name (buffer-name shell))
           (process (get-buffer-process shell))
           (viewport (agent-shell-viewport--buffer :shell-buffer shell)))
      (with-current-buffer viewport (insert "draft"))
      (with-current-buffer shell
        (should-error (agent-shell-rename-buffer :name "   ") :type 'user-error)
        (should-error (rename-buffer (buffer-name other)))
        (should (eq process (shell-maker--process))))
      (should (equal name (buffer-name shell)))
      (should (eq viewport (agent-shell-viewport--buffer :shell-buffer shell)))
      (should (equal "draft" (with-current-buffer viewport (buffer-string)))))))

(ert-deftest agent-shell-rename-restart-and-reload-test ()
  "Restart/reload retain the renamed name while replacing the shell process."
  (dolist (operation '(agent-shell-restart agent-shell-reload))
    (agent-shell-rename-tests--with-shells 1
      (let* ((shell (car agent-shell-rename-tests--shells))
             (process (get-buffer-process shell))
             (viewport (agent-shell-viewport--buffer :shell-buffer shell))
             (start-function (symbol-function 'agent-shell--start))
             (replacement nil)
             (requested-session nil))
        (with-current-buffer shell
          (map-put! agent-shell--state :session '((:id . "rename-test-session")))
          (shell-maker-set-buffer-name shell " *restart-renamed*")
          (cl-letf (((symbol-function 'agent-shell--start)
                     (lambda (&rest arguments)
                       (setq requested-session (plist-get arguments :session-id))
                       (setq replacement (agent-shell-rename-tests--shell start-function))))
                    ((symbol-function 'agent-shell--display-buffer) #'ignore))
            (funcall operation)))
        (should-not (buffer-live-p shell))
        (should-not (buffer-live-p viewport))
        (should-not (process-live-p process))
        (should (equal " *restart-renamed*" (buffer-name replacement)))
        (should (equal requested-session
                       (when (eq operation 'agent-shell-reload) "rename-test-session")))
        (with-current-buffer replacement
          (should (eq (get-buffer-process replacement) (shell-maker--process))))))))

(provide 'agent-shell-rename-tests)

;;; agent-shell-rename-tests.el ends here
