;;; agent-shell-fakes-tests.el --- Tests for replay helpers -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'agent-shell-fakes)

(ert-deftest agent-shell-fakes-synthetic-exchange-resolves-request-test ()
  "A generated exchange resolves a fake client's request with its result."
  (with-temp-buffer
    (let* ((messages (agent-shell-fakes--synth-exchange
                      7 "session/new" '((cwd . "/tmp/")) '((sessionId . "s1"))))
           (client (acp-fakes-make-client messages))
           (result nil))
      (funcall (map-elt client :request-sender)
               :client client
               :request '((:method . "session/new") (:params (cwd . "/tmp/")))
               :buffer (current-buffer)
               :on-success (lambda (response) (setq result response)))
      (should (equal '((sessionId . "s1")) result))
      (should (acp-fakes-exhausted-p client))
      (should (equal '((cwd . "/tmp/"))
                     (map-nested-elt (car messages) '(:object params)))))))

(ert-deftest agent-shell-fakes-partial-capture-gets-handshake-test ()
  "A partial capture gets a handshake with fresh IDs and the same session."
  (let* ((capture '(((:direction . outgoing) (:kind . request)
                     (:object (id . 41) (method . "session/prompt")
                              (params (sessionId . "existing-session") (prompt . []))))))
         (messages (agent-shell-fakes--synth-prelude capture))
         (requests (seq-filter (lambda (message)
                                 (eq (map-elt message :direction) 'outgoing))
                               messages))
         (session-response (seq-find (lambda (message)
                                       (map-nested-elt message '(:object result sessionId)))
                                     messages)))
    (should (agent-shell-fakes--complete-capture-p messages))
    (should (equal '("initialize" "session/new" "session/list" "session/prompt")
                   (seq-map (lambda (request)
                              (map-nested-elt request '(:object method)))
                            requests)))
    (should (seq-every-p (lambda (request)
                           (> (map-nested-elt request '(:object id)) 41))
                         (seq-take requests 3)))
    (should (equal "existing-session"
                   (map-nested-elt session-response '(:object result sessionId))))
    (should (eq (car capture) (car (last messages))))))

(provide 'agent-shell-fakes-tests)

;;; agent-shell-fakes-tests.el ends here
