;;; agent-shell-tool-output-tests.el --- Terminal output tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-openai)

(defun agent-shell-tool-output-tests--notify (state update)
  "Adapt and dispatch terminal UPDATE using STATE."
  (agent-shell--on-notification
   :state state
   :acp-notification
   (agent-shell--adapt-notification
    :state state
    :acp-notification
    `((method . "session/update")
      (params (update (sessionUpdate . "tool_call_update")
                      (toolCallId . "command") ,@update))))))

(defun agent-shell-tool-output-tests--call-with-shell (function)
  "Call FUNCTION with an isolated shell state and a real fragment buffer."
  (with-temp-buffer
    (comint-mode)
    (setq major-mode 'agent-shell-mode)
    (setq-local agent-shell--state
                (agent-shell--make-state
                 :buffer (current-buffer)
                 :agent-config (agent-shell-openai-make-codex-config)))
    (setq-local shell-maker--config (agent-shell--make-shell-maker-config :prompt "Test> "))
    (setq-local comint-last-output-start (copy-marker (point)))
    (insert (propertize "Test> " 'field 'prompt))
    (setq-local comint-last-prompt (cons (copy-marker 1) (copy-marker (point))))
    (let ((agent-shell--stream-render-interval 30)
          (agent-shell-tool-use-expand-by-default nil)
          (agent-shell-activity-group-expand-by-default t)
          (process (make-pipe-process :name "agent-shell-stream-output-test"
                                      :buffer (current-buffer) :noquery t
                                      :sentinel #'ignore)))
      (unwind-protect
          (cl-letf (((symbol-function 'shell-maker--process) (lambda () process))
                    ((symbol-function 'agent-shell--append-transcript) #'ignore))
            (funcall function agent-shell--state))
        (agent-shell--cancel-stream-render-timer agent-shell--state)
        (delete-process process)))))

(ert-deftest agent-shell-tool-output-shares-previous-chunks-test ()
  "Appending a delta retains earlier chunks without copying their strings."
  (agent-shell-tool-output-tests--call-with-shell
   (lambda (state)
     (let* ((text (make-string 1000000 ?x))
            (first (agent-shell--adapt-notification
                    :state state
                    :acp-notification
                    `((method . "session/update")
                      (params (update (sessionUpdate . "tool_call_update")
                                      (toolCallId . "one")
                                      (_meta (terminal_output_delta (data . ,text))))))))
            (second (agent-shell--adapt-notification
                     :state state
                     :acp-notification
                     '((method . "session/update")
                       (params (update (sessionUpdate . "tool_call_update")
                                       (toolCallId . "one")
                                       (_meta (terminal_output_delta (data . "tail")))))))))
       (should (eq (seq-first (map-nested-elt first '(params update :output-chunks))) text))
       (should (eq (seq-rest (map-nested-elt second '(params update :output-chunks)))
                   (map-nested-elt first '(params update :output-chunks))))
       (should (= (map-nested-elt second '(params update :output-length)) 1000004))
       (should-not (map-nested-elt second '(params update content)))))))

(ert-deftest agent-shell-tool-output-batches-and-appends-test ()
  "Batch raw chunks and append only new text to the live fragment."
  (agent-shell-tool-output-tests--call-with-shell
   (lambda (state)
     (let ((append-body (symbol-function 'agent-shell-ui--append-body))
           (replace-body (symbol-function 'agent-shell-ui--replace-body))
           appended replaced)
       (cl-letf (((symbol-function 'agent-shell-ui--append-body)
                  (lambda (&rest args)
                    (push (nth 1 args) appended)
                    (apply append-body args)))
                 ((symbol-function 'agent-shell-ui--replace-body)
                  (lambda (&rest args)
                    (push (nth 1 args) replaced)
                    (apply replace-body args))))
         (agent-shell-tool-output-tests--notify
          state '((status . "in_progress") (kind . "execute") (title . "Command")
                  (_meta (terminal_output_delta (data . "first")))))
         (let ((timer (map-elt state :stream-render-timer)))
           (agent-shell-tool-output-tests--notify
            state '((_meta (terminal_output_delta (data . " second\n")))))
           (should (eq timer (map-elt state :stream-render-timer))))
         (should-not (string-search "first" (buffer-string)))
         (agent-shell--flush-stream-render state)
         (should (string-search "first second\n" (buffer-string)))
         (agent-shell-tool-output-tests--notify
          state '((_meta (terminal_output_delta (data . "third")))))
         (agent-shell--flush-stream-render state)
         (should (equal appended '("third")))
         (should-not replaced)
         (should (string-search "first second\nthird" (buffer-string))))))))

(ert-deftest agent-shell-tool-output-bounds-preview-and-preserves-transcript-test ()
  "Bound the live body, then retain complete output on completion or failure."
  (dolist (status '("completed" "failed"))
    (agent-shell-tool-output-tests--call-with-shell
     (lambda (state)
       (let* ((chunk (make-string 4096 ?x))
              (text (apply #'concat (make-list 12 chunk)))
              transcript)
         (dotimes (_ 12)
           (agent-shell-tool-output-tests--notify
            state `((status . "in_progress") (kind . "execute") (title . "Command")
                    (_meta (terminal_output_delta (data . ,chunk))))))
         (agent-shell--flush-stream-render state)
         (should (< (buffer-size) (+ agent-shell--tool-output-preview-length 1000)))
         (should (string-search "Output continues;" (buffer-string)))
         (cl-letf (((symbol-function 'agent-shell--append-transcript)
                    (lambda (&rest args) (setq transcript (plist-get args :text)))))
           (agent-shell-tool-output-tests--notify state `((status . ,status))))
         (should (string-search text transcript))
         (should-not (string-search "Output continues;" (buffer-string)))
         (should (string-search "characters omitted" (buffer-string)))
         (should-not (map-elt state :stream-render-pending))
         (let ((body (buffer-string)))
           (agent-shell-tool-output-tests--notify state '((title . "Command")))
           (agent-shell--flush-stream-render state)
           (should (equal body (buffer-string)))))))))

(ert-deftest agent-shell-tool-output-completion-flushes-pending-preview-test ()
  "A completion flushes its queued preview before replacing the final body."
  (agent-shell-tool-output-tests--call-with-shell
   (lambda (state)
     (agent-shell-tool-output-tests--notify
      state '((status . "in_progress") (kind . "execute") (title . "Command")
              (_meta (terminal_output_delta (data . "first\n")))))
     (should (map-elt state :stream-render-pending))
     (agent-shell-tool-output-tests--notify
      state '((status . "completed")
              (_meta (terminal_output_delta (data . "last\n")))))
     (should (string-search "first\nlast" (buffer-string)))
     (should-not (map-elt state :stream-render-pending))
     (should-not (map-elt state :stream-render-timer)))))

(ert-deftest agent-shell-tool-output-interruption-preserves-output-test ()
  "An interrupted stream keeps its full output without a final tool update."
  (dolist (event '(turn-complete error clean-up))
    (agent-shell-tool-output-tests--call-with-shell
     (lambda (state)
       (let* ((chunk (concat (make-string 100 ?x) "\n"))
              (text (apply #'concat (make-list 400 chunk)))
              transcript)
         (agent-shell-tool-output-tests--notify
          state `((status . "in_progress") (kind . "execute") (title . "Command")
                  (_meta (terminal_output_delta (data . ,text)))))
         (should (map-elt state :stream-render-pending))
         (cl-letf (((symbol-function 'agent-shell--append-transcript)
                    (lambda (&rest args) (setq transcript (plist-get args :text)))))
           (agent-shell--emit-event :event event :data '((:stop-reason . "cancelled"))))
         (should (string-search text transcript))
         (should (string-search (string-trim-right text) (buffer-string)))
         (should-not (string-search "Output continues;" (buffer-string)))
         (should (equal (map-nested-elt state '(:tool-calls "command" :status)) "in_progress"))
         (should-not agent-shell-openai--terminal-output-snapshots)
         (should-not (map-elt state :stream-render-pending))
         (should-not (map-elt state :stream-render-timer)))))))

(ert-deftest agent-shell-tool-output-snapshot-resets-live-preview-test ()
  "A full snapshot replaces earlier output before subsequent deltas append."
  (agent-shell-tool-output-tests--call-with-shell
   (lambda (state)
     (agent-shell-tool-output-tests--notify
      state '((status . "in_progress") (kind . "execute") (title . "Command")
              (_meta (terminal_output_delta (data . "discarded\n")))))
     (agent-shell-tool-output-tests--notify
      state '((rawOutput (formatted_output . "snapshot\n"))))
     (agent-shell-tool-output-tests--notify
      state '((_meta (terminal_output_delta (data . "suffix\n")))))
     (agent-shell--flush-stream-render state)
     (should-not (string-search "discarded" (buffer-string)))
     (should (string-search "snapshot\nsuffix" (buffer-string)))
     (agent-shell-tool-output-tests--notify state '((status . "completed")))
     (should (string-search "snapshot\nsuffix" (buffer-string))))))

(provide 'agent-shell-tool-output-tests)
;;; agent-shell-tool-output-tests.el ends here
