;;; agent-shell-rename-tests.el --- Tests for buffer renaming -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'agent-shell)
(require 'acp-fakes)

(defvar agent-shell-rename-tests--shells nil)
(defvar agent-shell-rename-tests--steering-command #'agent-shell-submit-override)

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

(defun agent-shell-rename-tests--steer (rename-function rename-after-submit)
  "Steer through a rename using RENAME-FUNCTION.
When RENAME-AFTER-SUBMIT is non-nil, rename while the reply is pending."
  (agent-shell-rename-tests--with-shells 1
    (let* ((shell (car agent-shell-rename-tests--shells))
           (original-name (buffer-name shell))
           (process (get-buffer-process shell))
           (request nil)
           (unrelated nil)
           (agent-shell-persistent-prompt-enabled t)
           (agent-shell-busy-submit-override-function #'agent-shell-prompt-steer))
      (unwind-protect
          (with-current-buffer shell
            (map-put! (map-elt agent-shell--state :session) :id "rename-steering-session")
            (map-put! agent-shell--state :supports-steering t)
            (setq shell-maker--busy t)
            (unless rename-after-submit
              (funcall rename-function))
            (goto-char (point-max))
            (insert "change direction")
            (cl-letf (((symbol-function 'acp-send-request)
                       (lambda (&rest arguments)
                         (setq request arguments))))
              (call-interactively #'agent-shell-submit-override))
            (should (equal "_session/steering"
                           (map-elt (plist-get request :request) :method)))
            (should (equal "rename-steering-session"
                           (map-nested-elt (plist-get request :request) '(:params sessionId))))
            (should (equal [((type . "text") (text . "change direction"))]
                           (map-nested-elt (plist-get request :request) '(:params prompt))))
            (should (eq shell (plist-get request :buffer)))
            (should-not (agent-shell--prompt-input))
            (when rename-after-submit
              (funcall rename-function))
            (setq unrelated (get-buffer-create original-name))
            (with-current-buffer unrelated (insert "unrelated contents"))
            (goto-char (point-max))
            (insert "next draft")
            (with-current-buffer (plist-get request :buffer)
              (funcall (plist-get request :on-success) '((outcome . "injected"))))
            (should (string-match-p (regexp-quote "[steer] change direction")
                                    (buffer-string)))
            (should (equal "next draft" (agent-shell--prompt-input)))
            (should (eq process (shell-maker--process)))
            (should (process-live-p process))
            (should (equal "unrelated contents"
                           (with-current-buffer unrelated (buffer-string)))))
        (when (buffer-live-p unrelated)
          (kill-buffer unrelated))))))

(ert-deftest agent-shell-rename-steering-test ()
  "Steering and pending replies survive the supported rename command."
  (dolist (rename-after-submit '(nil t))
    (ert-info ((format "Rename after submission: %s" rename-after-submit))
      (agent-shell-rename-tests--steer
       (lambda () (agent-shell-rename-buffer :name " *renamed-steering*"))
       rename-after-submit))))

(ert-deftest agent-shell-rename-raw-steering-test ()
  "Steering survives raw renames when shell-maker supports them."
  (skip-unless (fboundp 'shell-maker--sync-buffer-name))
  (dolist (rename-after-submit '(nil t))
    (ert-info ((format "Rename after submission: %s" rename-after-submit))
      (agent-shell-rename-tests--steer
       (lambda () (rename-buffer " *raw-renamed-steering*" t))
       rename-after-submit))))

(defun agent-shell-rename-tests--message (direction kind object)
  "Make a recorded ACP message with DIRECTION, KIND, and OBJECT."
  `((:direction . ,direction) (:kind . ,kind) (:object . ,object)))

(defun agent-shell-rename-tests--chunk (text)
  "Make a recorded agent response chunk containing TEXT."
  (agent-shell-rename-tests--message
   'incoming 'notification
   `((method . "session/update")
     (params (sessionId . "rename-acceptance")
             (update (sessionUpdate . "agent_message_chunk")
                     (content (type . "text") (text . ,text)))))))

(defun agent-shell-rename-tests--barrier (method)
  "Make a recorded barrier named METHOD to hold back incoming traffic."
  (agent-shell-rename-tests--message 'outgoing 'notification `((method . ,method))))

(defun agent-shell-rename-tests--traffic (action &optional resume)
  "Record two turns with ACTION during the first, resuming when RESUME.
ACTION is `steer', `cancel', or nil for an ordinary streamed response."
  (append
   (list
    (agent-shell-rename-tests--message 'outgoing 'request '((id . 1) (method . "initialize")))
    (agent-shell-rename-tests--message
     'incoming 'response
     '((id . 1) (result (protocolVersion . 1)
                        (agentCapabilities (loadSession . t))
                        (_meta (steering (supported . t))))))
    (agent-shell-rename-tests--message
     'outgoing 'request `((id . 2) (method . ,(if resume "session/load" "session/new"))))
    (agent-shell-rename-tests--message
     'incoming 'response '((id . 2) (result (sessionId . "rename-acceptance")))))
   (list
    (agent-shell-rename-tests--message 'outgoing 'request '((id . 10) (method . "session/prompt")))
    (agent-shell-rename-tests--chunk "before rename; "))
   (pcase action
     ('steer
      (list
       (agent-shell-rename-tests--message 'outgoing 'request '((id . 11) (method . "_session/steering")))
       (agent-shell-rename-tests--barrier "rename-test/steer-reply")
       (agent-shell-rename-tests--message 'incoming 'response '((id . 11) (result (outcome . "injected"))))))
     ('cancel
      (list (agent-shell-rename-tests--barrier "session/cancel")))
     (_ (list (agent-shell-rename-tests--barrier "rename-test/continue"))))
   (unless (eq action 'cancel)
     (list (agent-shell-rename-tests--chunk "after rename")
           (agent-shell-rename-tests--barrier "rename-test/finish")))
   (list
    (agent-shell-rename-tests--message
     'incoming 'response `((id . 10) (result (stopReason . ,(if (eq action 'cancel) "cancelled" "end_turn")))))
    (agent-shell-rename-tests--message 'outgoing 'request '((id . 12) (method . "session/prompt")))
    (agent-shell-rename-tests--barrier "rename-test/next-finish")
    (agent-shell-rename-tests--chunk "next response")
    (agent-shell-rename-tests--message 'incoming 'response '((id . 12) (result (stopReason . "end_turn")))))))

(defun agent-shell-rename-tests--wait (predicate)
  "Wait up to five seconds for PREDICATE, then assert it holds."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01)
      (sit-for 0))
    (should (funcall predicate))))

(defun agent-shell-rename-tests--client-shell (traffic-maker)
  "Start a shell using TRAFFIC-MAKER as its fake agent server."
  (let ((shell
         (agent-shell--start
          :config (agent-shell-make-agent-config
                   :identifier 'rename-acceptance
                   :buffer-name "Acceptance" :mode-line-name "Acceptance"
                   :shell-prompt "Agent> " :shell-prompt-regexp "^Agent> "
                   :client-maker
                   (lambda (buffer)
                     (unless (memq buffer agent-shell-rename-tests--shells)
                       (push buffer agent-shell-rename-tests--shells))
                     (let ((client (acp-fakes-make-client (funcall traffic-maker))))
                       (map-put! client :context-buffer buffer)
                       client)))
          :session-strategy 'new :new-session t :no-focus t)))
    (agent-shell-rename-tests--wait
     (lambda () (map-elt (buffer-local-value 'agent-shell--state shell) :init-finished)))
    shell))

(defun agent-shell-rename-tests--advance (client method)
  "Release CLIENT's recorded barrier, which must be named METHOD."
  (should (equal method (map-nested-elt (acp-fakes-barrier client) '(:object method))))
  (should (acp-fakes-skip-barrier client)))

(defun agent-shell-rename-tests--submit (text &optional command)
  "Type TEXT into the current shell and invoke COMMAND.
COMMAND defaults to the public `agent-shell-submit' command."
  (goto-char (point-max))
  (insert text)
  (call-interactively (or command #'agent-shell-submit)))

(defun agent-shell-rename-tests--assert-request (client id text)
  "Check CLIENT's pending request ID contains TEXT for the right shell."
  (let ((pending (map-nested-elt client `(:pending-requests ,id))))
    (should (eq (current-buffer) (map-elt pending :buffer)))
    (should (equal "rename-acceptance"
                   (map-nested-elt pending '(:request :params sessionId))))
    (should (equal (vector `((type . "text") (text . ,text)))
                   (map-nested-elt pending '(:request :params prompt))))))

(defun agent-shell-rename-tests--next-turn (client &optional command)
  "Submit and finish another turn through CLIENT after the rename.
Use COMMAND to submit, defaulting to `agent-shell-submit'."
  (should-not (shell-maker-busy))
  (agent-shell-rename-tests--submit "next prompt" command)
  (agent-shell-rename-tests--assert-request client 12 "next prompt")
  (agent-shell-rename-tests--advance client "rename-test/next-finish")
  (agent-shell-rename-tests--wait
   (lambda () (and (not (shell-maker-busy))
                   (string-match-p "next response" (buffer-string)))))
  (should (acp-fakes-exhausted-p client))
  (should-not (map-elt client :pending-requests))
  (should-not (map-elt agent-shell--state :active-requests))
  (should (agent-shell--prompt-input-start))
  (should (string-match-p "next response" (buffer-string))))

(ert-deftest agent-shell-rename-acceptance-submit-stream-test ()
  "Submission, streaming, and another turn survive idle and busy renames."
  (skip-unless (fboundp 'shell-maker--sync-buffer-name))
  (agent-shell-rename-tests--with-shells 0
    (with-current-buffer (agent-shell-rename-tests--client-shell
                          (lambda () (agent-shell-rename-tests--traffic nil)))
      (let ((client (map-elt agent-shell--state :client))
            (process (shell-maker--process)))
        (agent-shell-rename-buffer :name " *acceptance-idle*")
        (agent-shell-rename-tests--submit "hello")
        (should (shell-maker-busy))
        (agent-shell-rename-tests--assert-request client 10 "hello")
        (agent-shell-rename-tests--wait
         (lambda () (string-match-p "before rename; " (buffer-string))))
        (rename-buffer " *acceptance-streaming*" t)
        (goto-char (point-max))
        (insert "draft during streaming")
        (agent-shell-rename-tests--advance client "rename-test/continue")
        (agent-shell-rename-tests--wait
         (lambda () (string-match-p "before rename; after rename" (buffer-string))))
        (agent-shell-rename-tests--advance client "rename-test/finish")
        (should (equal "draft during streaming" (agent-shell--prompt-input)))
        (should (eq process (shell-maker--process)))
        (agent-shell--clear-prompt-input)
        (agent-shell-rename-tests--next-turn client)))))

(ert-deftest agent-shell-rename-acceptance-steering-test ()
  "C-<return> steers correctly before and across an in-flight rename."
  (skip-unless (fboundp 'shell-maker--sync-buffer-name))
  (dolist (rename-after-submit '(nil t))
    (ert-info ((format "Rename after steering submission: %s" rename-after-submit))
      (let ((agent-shell-mode-map (copy-keymap agent-shell-mode-map)))
        (define-key agent-shell-mode-map (kbd "C-<return>")
                    agent-shell-rename-tests--steering-command)
        (agent-shell-rename-tests--with-shells 0
          (let* ((shell (agent-shell-rename-tests--client-shell
                         (lambda () (agent-shell-rename-tests--traffic 'steer))))
                 (other (agent-shell-rename-tests--client-shell
                         (lambda () (agent-shell-rename-tests--traffic nil))))
                 (old-name (buffer-name shell))
                 (other-text (with-current-buffer other (buffer-string))))
            (with-current-buffer shell
              (let ((client (map-elt agent-shell--state :client))
                    (process (shell-maker--process)))
                (agent-shell-rename-tests--submit "hello")
                (unless rename-after-submit (rename-buffer " *acceptance-steer*" t))
                (goto-char (point-max))
                (insert "change direction")
                (call-interactively (key-binding (kbd "C-<return>")))
                (agent-shell-rename-tests--assert-request client 11 "change direction")
                (should-not (agent-shell--prompt-input))
                (when rename-after-submit (rename-buffer " *acceptance-steer*" t))
                (shell-maker-set-buffer-name other old-name)
                (goto-char (point-max))
                (insert "next draft")
                (agent-shell-rename-tests--advance client "rename-test/steer-reply")
                (should (string-match-p (regexp-quote "[steer] change direction") (buffer-string)))
                (should (equal "next draft" (agent-shell--prompt-input)))
                (agent-shell-rename-tests--advance client "rename-test/finish")
                (should (eq process (shell-maker--process)))
                (should (equal "next draft" (agent-shell--prompt-input)))
                (agent-shell--clear-prompt-input)
                (agent-shell-rename-tests--next-turn client (key-binding (kbd "C-<return>")))))
            (should (equal other-text (with-current-buffer other (buffer-string))))
            (should-not (map-elt (map-elt (buffer-local-value 'agent-shell--state other) :client)
                                 :pending-requests))))))))

(ert-deftest agent-shell-rename-acceptance-viewport-test ()
  "Renamed viewports retain drafts and queue into their associated shell."
  (skip-unless (fboundp 'shell-maker--sync-buffer-name))
  (agent-shell-rename-tests--with-shells 0
    (let* ((shell (agent-shell-rename-tests--client-shell
                   (lambda () (agent-shell-rename-tests--traffic nil))))
           (viewport (agent-shell-viewport--buffer :shell-buffer shell))
           (client (map-elt (buffer-local-value 'agent-shell--state shell) :client)))
      (with-current-buffer viewport (insert "queued from viewport"))
      (with-current-buffer shell
        (agent-shell-rename-tests--submit "hello")
        (rename-buffer " *acceptance-viewport-shell*" t))
      (with-current-buffer viewport
        (rename-buffer " *acceptance-independent-viewport*" t)
        (should (equal "queued from viewport" (buffer-string)))
        (should (eq shell (agent-shell--current-shell)))
        (agent-shell-viewport-compose-send t)
        (should (string-empty-p (string-trim (buffer-string)))))
      (with-current-buffer shell
        (should (equal '("queued from viewport") (map-elt agent-shell--state :pending-prompts)))
        (goto-char (point-max))
        (insert "shell draft")
        (agent-shell-rename-tests--advance client "rename-test/continue")
        (agent-shell-rename-tests--advance client "rename-test/finish")
        (agent-shell-rename-tests--wait
         (lambda () (map-nested-elt client '(:pending-requests 12))))
        (agent-shell-rename-tests--assert-request client 12 "queued from viewport")
        (should (equal "shell draft" (agent-shell--prompt-input)))
        (agent-shell-rename-tests--advance client "rename-test/next-finish")
        (should (acp-fakes-exhausted-p client))
        (should-not (shell-maker-busy))
        (should (equal "shell draft" (agent-shell--prompt-input))))
      (should (eq viewport (agent-shell-viewport--buffer :shell-buffer shell))))))

(ert-deftest agent-shell-rename-acceptance-interrupt-test ()
  "Interrupting a renamed shell finishes the turn and permits another."
  (skip-unless (fboundp 'shell-maker--sync-buffer-name))
  (agent-shell-rename-tests--with-shells 0
    (with-current-buffer (agent-shell-rename-tests--client-shell
                          (lambda () (agent-shell-rename-tests--traffic 'cancel)))
      (let ((client (map-elt agent-shell--state :client))
            (process (shell-maker--process))
            (agent-shell-confirm-interrupt nil))
        (agent-shell-rename-tests--submit "hello")
        (rename-buffer " *acceptance-interrupt*" t)
        (call-interactively #'agent-shell-interrupt)
        (agent-shell-rename-tests--wait (lambda () (not (shell-maker-busy))))
        (should-not (map-elt agent-shell--state :active-requests))
        (should (eq process (shell-maker--process)))
        (agent-shell-rename-tests--next-turn client)))))

(ert-deftest agent-shell-rename-acceptance-restart-reload-test ()
  "Actual restart and reload retain a renamed shell's name and usability."
  (dolist (operation '(agent-shell-restart agent-shell-reload))
    (agent-shell-rename-tests--with-shells 0
      (let* ((resume nil)
             (shell (agent-shell-rename-tests--client-shell
                     (lambda () (agent-shell-rename-tests--traffic nil resume))))
             (process (get-buffer-process shell))
             (client (map-elt (buffer-local-value 'agent-shell--state shell) :client))
             (viewport (agent-shell-viewport--buffer :shell-buffer shell))
             (name nil))
        (with-current-buffer shell
          (agent-shell-rename-buffer :name " *acceptance-restart*")
          (setq name (buffer-name))
          (setq resume (eq operation 'agent-shell-reload))
          (save-window-excursion (call-interactively operation)))
        (let ((replacement (get-buffer name)))
          (should (buffer-live-p replacement))
          (should-not (buffer-live-p shell))
          (should-not (buffer-live-p viewport))
          (should-not (process-live-p process))
          (should-not (process-live-p (map-elt client :process)))
          (agent-shell-rename-tests--wait
           (lambda () (map-elt (buffer-local-value 'agent-shell--state replacement) :init-finished)))
          (with-current-buffer replacement
            (should (equal "rename-acceptance" (map-nested-elt agent-shell--state '(:session :id))))
            (agent-shell-rename-tests--submit "hello")
            (agent-shell-rename-tests--advance (map-elt agent-shell--state :client) "rename-test/continue")
            (agent-shell-rename-tests--advance (map-elt agent-shell--state :client) "rename-test/finish")
            (agent-shell-rename-tests--next-turn (map-elt agent-shell--state :client))))))))

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

(ert-deftest agent-shell-rename-viewport-observers-see-current-process-test ()
  "Viewport rename observers find the same process with shell-maker support."
  (skip-unless (fboundp 'shell-maker--sync-buffer-name))
  (agent-shell-rename-tests--with-shells 1
    (let* ((shell (car agent-shell-rename-tests--shells))
           (viewport (agent-shell-viewport--buffer :shell-buffer shell))
           (process (get-buffer-process shell))
           (old-name (buffer-name shell))
           (observed 'not-called)
           (observer (lambda ()
                       (setq observed (with-current-buffer shell
                                        (shell-maker--process))))))
      (unwind-protect
          (progn
            (with-current-buffer viewport
              (add-hook 'buffer-list-update-hook observer nil t))
            (with-current-buffer shell (rename-buffer " *observer-renamed*" t))
            (should (eq process observed))
            (should-not (get-buffer old-name)))
        (when (buffer-live-p viewport)
          (with-current-buffer viewport
            (remove-hook 'buffer-list-update-hook observer t)))
        (when-let* ((ghost (get-buffer old-name)))
          (kill-buffer ghost))))))

(ert-deftest agent-shell-rename-reused-shell-keeps-viewport-test ()
  "Reinitializing an existing shell retains its viewport and unsent draft."
  (agent-shell-rename-tests--with-shells 1
    (let* ((shell (car agent-shell-rename-tests--shells))
           (viewport (agent-shell-viewport--buffer :shell-buffer shell))
           (process (get-buffer-process shell))
           (config (map-elt (buffer-local-value 'agent-shell--state shell) :agent-config)))
      (unwind-protect
          (progn
            (with-current-buffer viewport (insert "preserve draft"))
            (cl-letf (((symbol-function 'agent-shell--handle) #'ignore))
              (should (eq shell (agent-shell--start :config config
                                                    :session-strategy 'new :no-focus t))))
            (should (eq viewport (agent-shell-viewport--buffer :shell-buffer shell)))
            (should (eq shell (agent-shell-viewport--shell-buffer viewport)))
            (should (eq process (get-buffer-process shell)))
            (should (equal "preserve draft" (with-current-buffer viewport (buffer-string)))))
        (when (buffer-live-p viewport)
          (kill-buffer viewport))))))

(ert-deftest agent-shell-rename-reuse-failure-cleans-viewport-test ()
  "Failed reinitialization cleans up the existing viewport with its shell."
  (agent-shell-rename-tests--with-shells 1
    (let* ((shell (car agent-shell-rename-tests--shells))
           (viewport (agent-shell-viewport--buffer :shell-buffer shell))
           (process (get-buffer-process shell))
           (config (map-copy (map-elt (buffer-local-value 'agent-shell--state shell)
                                      :agent-config))))
      (unwind-protect
          (progn
            (map-put! config :client-maker (lambda (_buffer) nil))
            (should-error (agent-shell--start :config config :session-strategy 'new :no-focus t))
            (should-not (buffer-live-p shell))
            (should-not (buffer-live-p viewport))
            (accept-process-output process 1)
            (should-not (process-live-p process)))
        (when (buffer-live-p viewport)
          (kill-buffer viewport))))))

(ert-deftest agent-shell-rename-viewport-other-major-mode-test ()
  "Changing viewport mode preserves its draft until shell cleanup."
  (agent-shell-rename-tests--with-shells 1
    (let* ((shell (car agent-shell-rename-tests--shells))
           (viewport (agent-shell-viewport--buffer :shell-buffer shell)))
      (unwind-protect
          (progn
            (with-current-buffer viewport
              (insert "preserve draft")
              (text-mode)
              (rename-buffer " *renamed-text-draft*" t))
            (should-not (agent-shell-viewport--buffer :shell-buffer shell :existing-only t))
            (should-error (agent-shell-viewport--buffer :shell-buffer shell) :type 'user-error)
            (should (equal "preserve draft" (with-current-buffer viewport (buffer-string))))
            (kill-buffer shell)
            (should-not (buffer-live-p viewport)))
        (when (buffer-live-p viewport)
          (with-current-buffer viewport
            (agent-shell-viewport-edit-mode)))))))

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
