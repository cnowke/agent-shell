;;; agent-shell-tool-output-tests.el --- Terminal output tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-openai)

(defun agent-shell-tool-output-tests--notify (state update &optional tool-call-id)
  "Adapt and dispatch terminal UPDATE using STATE and TOOL-CALL-ID."
  (agent-shell--on-notification
   :state state
   :acp-notification
   (agent-shell--adapt-notification
    :state state
    :acp-notification
    `((method . "session/update")
      (params (update (sessionUpdate . "tool_call_update")
                      (toolCallId . ,(or tool-call-id "command")) ,@update))))))

(defun agent-shell-tool-output-tests--body (state &optional tool-call-id)
  "Return the raw body of STATE's TOOL-CALL-ID fragment."
  (let* ((block (agent-shell-ui--cached-block
                 (format "%s-%s" (map-elt state :request-count) (or tool-call-id "command"))))
         (body (and block (agent-shell-ui--body-range
                           (map-elt block :start) (map-elt block :end)))))
    (if body
        (buffer-substring-no-properties (map-elt body :start) (map-elt body :end))
      "")))

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
         (should (string-search "first" (buffer-string)))
         (should-not (map-elt state :stream-render-timer))
         (agent-shell-tool-output-tests--notify
          state '((_meta (terminal_output_delta (data . " second")))))
         (let ((timer (map-elt state :stream-render-timer)))
           (should (timerp timer))
           (agent-shell-tool-output-tests--notify
            state '((_meta (terminal_output_delta (data . "\n")))))
           (should (eq timer (map-elt state :stream-render-timer))))
         (should-not (string-search "first second" (buffer-string)))
         (agent-shell--flush-stream-render state)
         (should (string-search "first second\n" (buffer-string)))
         (agent-shell-tool-output-tests--notify
          state '((_meta (terminal_output_delta (data . "third")))))
         (agent-shell--flush-stream-render state)
         (should (equal appended '("third" " second\n")))
         (should-not replaced)
         (should (string-search "first second\nthird" (buffer-string))))))))

(ert-deftest agent-shell-tool-output-stream-updates-labels-only-when-changed-test ()
  "Repeated metadata batches output; changed labels preserve the live body."
  (agent-shell-tool-output-tests--call-with-shell
   (lambda (state)
     (let ((refresh (symbol-function 'agent-shell--refresh-activity-group-header))
           (refreshes 0))
       (cl-letf (((symbol-function 'agent-shell--refresh-activity-group-header)
                  (lambda (&rest args)
                    (setq refreshes (1+ refreshes))
                    (apply refresh args))))
         (dotimes (_ 20)
           (agent-shell-tool-output-tests--notify
            state '((status . "in_progress") (title . "Command")
                    (_meta (terminal_output_delta (data . "line\n"))))))
         (should (= refreshes 1))
         (should (map-elt state :stream-render-pending))
         (agent-shell--flush-stream-render state)
         (should (= refreshes 1))
         (let ((body (agent-shell-tool-output-tests--body state)))
           (agent-shell-tool-output-tests--notify state '((title . "Renamed")))
           (should (equal body (agent-shell-tool-output-tests--body state)))
           (should (string-search "Renamed" (buffer-string))))
         (should (= refreshes 2))
         (agent-shell-tool-output-tests--notify state '((status . "completed")))
         (should (= refreshes 3)))))))

(ert-deftest agent-shell-tool-output-completion-preserves-transcript-test ()
  "Append full live output and retain it on completion or failure."
  (dolist (status '("completed" "failed"))
    (agent-shell-tool-output-tests--call-with-shell
     (lambda (state)
       (let* ((agent-shell--tool-output-max-line-length 4096)
              (chunk (make-string 1024 ?x))
              (text (apply #'concat (make-list 12 chunk)))
              transcript)
         (dotimes (_ 12)
           (agent-shell-tool-output-tests--notify
            state `((status . "in_progress") (kind . "execute") (title . "Command")
                    (_meta (terminal_output_delta (data . ,chunk))))))
         (agent-shell--flush-stream-render state)
         (should (string-search text (buffer-string)))
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
     (agent-shell-tool-output-tests--notify
      state '((_meta (terminal_output_delta (data . "middle\n")))))
     (should (map-elt state :stream-render-pending))
     (agent-shell-tool-output-tests--notify
      state '((status . "completed")
              (_meta (terminal_output_delta (data . "last\n")))))
     (should (string-search "first\nmiddle\nlast" (buffer-string)))
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
          state '((status . "in_progress") (title . "Command")
                  (_meta (terminal_output_delta (data . "prefix\n")))))
         (agent-shell-tool-output-tests--notify
          state `((status . "in_progress") (kind . "execute") (title . "Command")
                  (_meta (terminal_output_delta (data . ,text)))))
         (should (map-elt state :stream-render-pending))
         (cl-letf (((symbol-function 'agent-shell--append-transcript)
                    (lambda (&rest args) (setq transcript (plist-get args :text))))
                   ((symbol-function 'agent-shell--shutdown) #'ignore))
           (if (eq event 'clean-up)
               (agent-shell--clean-up)
             (agent-shell--emit-event :event event :data '((:stop-reason . "cancelled")))))
         (should (string-search text transcript))
         (should (string-search (string-trim-right text) (buffer-string)))
         (should-not (string-search "Output continues;" (buffer-string)))
         (should (equal (map-nested-elt state '(:tool-calls "command" :status)) "in_progress"))
         (if (eq event 'clean-up)
             (should-not agent-shell-openai--terminal-output-snapshots)
           (should-not (map-nested-elt agent-shell-openai--terminal-output-snapshots
                                       '("command" :streaming))))
         (should-not (map-elt state :stream-render-pending))
         (should-not (map-elt state :stream-render-timer)))))))

(ert-deftest agent-shell-tool-output-request-settlement-finalizes-output-test ()
  "Real prompt callbacks preserve output on success, cancellation, and failure."
  (dolist (outcome '("end_turn" "cancelled" error))
    (agent-shell-tool-output-tests--call-with-shell
     (lambda (state)
       (let ((text (apply #'concat (make-list 400 (concat (make-string 100 ?x) "\n"))))
             (agent-shell-show-busy-indicator nil)
             (agent-shell-show-usage-at-turn-end nil)
             (agent-shell-activity-group-expand-by-default 'latest)
             (transcript "")
             callbacks)
         (map-put! state :session '((:id . "test-session") (:title . "Test")))
         (cl-letf (((symbol-function 'agent-shell--send-request)
                    (lambda (&rest args) (setq callbacks args)))
                   ((symbol-function 'agent-shell--finish-output) #'ignore)
                   ((symbol-function 'agent-shell--start-idle-timer) #'ignore)
                   ((symbol-function 'agent-shell--append-transcript)
                    (lambda (&rest args)
                      (setq transcript (concat transcript (plist-get args :text))))))
           (agent-shell--send-command :prompt "Test" :shell-buffer (current-buffer))
           (agent-shell-tool-output-tests--notify
            state '((status . "in_progress") (title . "Command")
                    (_meta (terminal_output_delta (data . "first\n")))))
           (agent-shell-tool-output-tests--notify
            state `((_meta (terminal_output_delta (data . ,text)))))
           (should (map-elt state :stream-render-pending))
           (if (eq outcome 'error)
               (funcall (plist-get callbacks :on-failure) '((code . 123) (message . "Test failure")) nil)
             (funcall (plist-get callbacks :on-success) `((stopReason . ,outcome))))
           (should (string-search (string-trim-right text) (agent-shell-tool-output-tests--body state)))
           (should (string-search text transcript))
           (should (equal (map-nested-elt state '(:tool-calls "command" :status)) "in_progress"))
           (should-not (map-nested-elt state '(:tool-calls "command" :output-chunks)))
           (should-not (map-nested-elt agent-shell-openai--terminal-output-snapshots '("command" :streaming)))
           (should-not (map-elt state :expanded-activity-group))
           (should-not (map-elt state :stream-render-pending))
           (should-not (map-elt state :stream-render-timer))))))))

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

(ert-deftest agent-shell-tool-output-snapshot-metadata-does-not-finalize-preview-test ()
  "Metadata after a full snapshot must not turn it into a streaming preview."
  (agent-shell-tool-output-tests--call-with-shell
   (lambda (state)
     (agent-shell-tool-output-tests--notify
      state '((status . "in_progress") (title . "Command")
              (_meta (terminal_output_delta (data . "first\n")))))
     (agent-shell-tool-output-tests--notify state '((rawOutput (formatted_output . "snapshot\n"))))
     (agent-shell-tool-output-tests--notify state '((title . "Renamed")))
     (let ((body (agent-shell-tool-output-tests--body state))
           (assembler (symbol-function 'agent-shell--tool-output-chunks-markdown))
           (assemblies 0))
       (cl-letf (((symbol-function 'agent-shell--tool-output-chunks-markdown)
                  (lambda (chunks)
                    (setq assemblies (1+ assemblies))
                    (funcall assembler chunks))))
         (agent-shell--emit-event :event 'turn-complete))
       (should (equal body (agent-shell-tool-output-tests--body state)))
       (should (zerop assemblies))
       (should-not (map-nested-elt state '(:tool-calls "command" :output-chunks)))))))

(ert-deftest agent-shell-tool-output-preview-preserves-whitespace-test ()
  "Preserve leading spaces and newlines when creating or resetting a preview."
  (dolist (text '("\n  first\n\n\n" "  first" "\n" " \t\n\n" ""))
    (agent-shell-tool-output-tests--call-with-shell
     (lambda (state)
       (dolist (replacement '(nil t))
         (when replacement
           (agent-shell-tool-output-tests--notify state '((rawOutput (formatted_output . "")))))
         (agent-shell-tool-output-tests--notify
          state `((status . "in_progress") (title . "Command")
                  (_meta (terminal_output_delta (data . ,text)))))
         (agent-shell--flush-stream-render state)
         (should (equal (agent-shell-tool-output-tests--body state) text)))))))

(ert-deftest agent-shell-tool-output-preview-keeps-command-test ()
  "The executing command remains inspectable while its output streams."
  (agent-shell-tool-output-tests--call-with-shell
   (lambda (state)
     (agent-shell-tool-output-tests--notify
      state '((status . "in_progress") (title . "Run")
              (rawInput (command . "printf audit"))
              (_meta (terminal_output_delta (data . "first\n")))))
     (agent-shell--flush-stream-render state)
     (should (string-search "printf audit" (agent-shell-tool-output-tests--body state)))
     (agent-shell-tool-output-tests--notify
      state '((_meta (terminal_output_delta (data . "second\n")))))
     (agent-shell--flush-stream-render state)
     (should (string-search "printf audit" (agent-shell-tool-output-tests--body state)))
     (should (string-search "first\nsecond\n" (agent-shell-tool-output-tests--body state))))))

(ert-deftest agent-shell-tool-output-completion-keeps-markup-literal-test ()
  "Embedded fences cannot expose terminal logs to Markdown rendering."
  (agent-shell-tool-output-tests--call-with-shell
   (lambda (state)
     (let ((agent-shell-tool-use-expand-by-default t)
           (text "first\n```\n# untouched\n\n*literal*\n"))
       (agent-shell-tool-output-tests--notify
        state `((status . "in_progress") (title . "Command")
                (_meta (terminal_output_delta (data . ,text)))))
       (agent-shell--flush-stream-render state)
       (should (equal (agent-shell-tool-output-tests--body state) text))
       (agent-shell-tool-output-tests--notify state '((status . "completed")))
       (should (string-search (string-trim-right text)
                              (agent-shell-tool-output-tests--body state)))))))

(ert-deftest agent-shell-tool-output-finalization-keeps-events-and-folds-test ()
  "Finalizing output preserves turn-end folds and emits no protocol update."
  (dolist (event '(turn-complete error clean-up))
    (agent-shell-tool-output-tests--call-with-shell
     (lambda (state)
       (let ((agent-shell-activity-group-expand-by-default 'latest)
             (updates 0))
         (agent-shell-subscribe-to
          :shell-buffer (current-buffer) :event 'tool-call-update
          :on-event (lambda (_) (setq updates (1+ updates))))
         (agent-shell-tool-output-tests--notify
          state '((status . "in_progress") (title . "Command")
                  (_meta (terminal_output_delta (data . "output\n")))))
         (agent-shell--flush-stream-render state)
         (agent-shell--collapse-expanded-activity-group state)
         (agent-shell--emit-event :event event :data '((:stop-reason . "cancelled")))
         (should (= updates 1))
         (should-not (map-elt state :expanded-activity-group)))))))

(ert-deftest agent-shell-tool-output-background-continuation-test ()
  "A background command retains output across the end of its prompt turn."
  (agent-shell-tool-output-tests--call-with-shell
   (lambda (state)
     (agent-shell-tool-output-tests--notify
      state '((status . "in_progress") (title . "Command")
              (_meta (terminal_output_delta (data . "before\n")))))
     (agent-shell--emit-event :event 'turn-complete :data '((:stop-reason . "end_turn")))
     (should (string-search
              "before"
              (agent-shell--tool-call-update-output-markdown
               `((content . ,(map-nested-elt state '(:tool-calls "command" :content)))))))
     (agent-shell-tool-output-tests--notify
      state '((_meta (terminal_output_delta (data . "after\n")))))
     (agent-shell-tool-output-tests--notify state '((status . "completed")))
     (should (string-search "before\nafter" (agent-shell-tool-output-tests--body state))))))

(ert-deftest agent-shell-tool-output-restore-interrupted-command-test ()
  "Restore all captured output when history has no final tool update."
  (agent-shell-tool-output-tests--call-with-shell
   (lambda (state)
     (let ((text (apply #'concat (make-list 400 (concat (make-string 100 ?x) "\n")))))
       (map-put! state :supports-session-load t)
       (map-put! state :pending-restore (agent-shell--make-pending-restore))
       (agent-shell-tool-output-tests--notify
        state `((status . "in_progress") (title . "Command")
                (_meta (terminal_output_delta (data . ,text)))))
       (cl-letf (((symbol-function 'agent-shell--create-bootstrapping-placeholders) #'ignore))
         (agent-shell--render-pending-restore state))
       (should (string-search (string-trim-right text)
                              (agent-shell-tool-output-tests--body state)))
       (should-not (string-search "Output continues;" (buffer-string)))))))

(ert-deftest agent-shell-tool-output-truncated-restore-retains-visible-prefix-test ()
  "Finalize the visible prefix even when a later snapshot or completion is omitted."
  (dolist (middle '(((status . "completed"))
                    ((status . "completed") (rawOutput (formatted_output . "omitted snapshot\n")))
                    ((rawOutput (formatted_output . "omitted snapshot\n")))))
    (agent-shell-tool-output-tests--call-with-shell
     (lambda (state)
       (let ((text (apply #'concat (make-list 400 (concat (make-string 100 ?x) "\n"))))
             (agent-shell-session-restore-verbosity 'first-last))
         (map-put! state :supports-session-load t)
         (map-put! state :pending-restore (agent-shell--make-pending-restore))
         (dolist (update
                  `(((sessionUpdate . "user_message_chunk")
                     (content (type . "text") (text . "First turn")))
                    ((sessionUpdate . "tool_call_update") (toolCallId . "command")
                     (title . "Command") (status . "in_progress")
                     (_meta (terminal_output_delta (data . ,text))))
                    ((sessionUpdate . "user_message_chunk")
                     (content (type . "text") (text . "Omitted turn")))
                    ((sessionUpdate . "tool_call_update") (toolCallId . "command") ,@middle)
                    ((sessionUpdate . "user_message_chunk")
                     (content (type . "text") (text . "Last turn")))
                    ((sessionUpdate . "agent_message_chunk")
                     (content (type . "text") (text . "Last response")))))
           (agent-shell--on-notification
            :state state
            :acp-notification
            (agent-shell--adapt-notification
             :state state
             :acp-notification `((method . "session/update") (params (update . ,update))))))
         (cl-letf (((symbol-function 'agent-shell--create-bootstrapping-placeholders) #'ignore))
           (agent-shell--render-pending-restore state))
         (should (string-search (string-trim-right text) (agent-shell-tool-output-tests--body state)))
         (should-not (string-search "Output continues;" (buffer-string)))
         (should-not (string-search "omitted snapshot" (buffer-string)))
         (should (string-search
                  (string-trim-right text)
                  (agent-shell--tool-call-update-output-markdown
                   `((content . ,(map-nested-elt state '(:tool-calls "command" :content)))))))
         (should-not (map-nested-elt state '(:tool-calls "command" :output-chunks)))
         (if (map-elt middle 'status)
             (should-not agent-shell-openai--terminal-output-snapshots)
           (agent-shell-tool-output-tests--notify
            state '((_meta (terminal_output_delta (data . "resumed\n")))))
           (agent-shell--flush-stream-render state)
           (should (equal (agent-shell-tool-output-tests--body state)
                          "omitted snapshot\nresumed\n"))))))))

(ert-deftest agent-shell-tool-output-restore-metadata-without-rendered-preview-test ()
  "A selected metadata-only update reconstructs an earlier omitted stream."
  (agent-shell-tool-output-tests--call-with-shell
   (lambda (state)
     (let ((agent-shell-session-restore-verbosity 'last))
       (map-put! state :supports-session-load t)
       (map-put! state :pending-restore (agent-shell--make-pending-restore))
       (dolist (update
                '(((sessionUpdate . "user_message_chunk")
                   (content (type . "text") (text . "Omitted turn")))
                  ((sessionUpdate . "tool_call_update") (toolCallId . "command")
                   (status . "in_progress")
                   (_meta (terminal_output_delta (data . "earlier output\n"))))
                  ((sessionUpdate . "user_message_chunk")
                   (content (type . "text") (text . "Selected turn")))
                  ((sessionUpdate . "tool_call_update") (toolCallId . "command")
                   (status . "in_progress") (title . "Renamed"))))
         (agent-shell--on-notification
          :state state
          :acp-notification
          (agent-shell--adapt-notification
           :state state
           :acp-notification `((method . "session/update") (params (update . ,update))))))
       (cl-letf (((symbol-function 'agent-shell--create-bootstrapping-placeholders) #'ignore))
         (agent-shell--render-pending-restore state))
       (should (string-search "earlier output" (agent-shell-tool-output-tests--body state)))
       (should (string-search "Renamed" (buffer-string)))
       (should-not (map-nested-elt state '(:tool-calls "command" :output-chunks)))))))

(ert-deftest agent-shell-tool-output-all-chunk-boundaries-test ()
  "Every partition of a short Unicode log yields the same literal preview."
  (let ((text " \nλ\t\n"))
    (dolist (interval '(nil 30))
      (dotimes (mask (expt 2 (1- (length text))))
        (agent-shell-tool-output-tests--call-with-shell
         (lambda (state)
           (let ((agent-shell--stream-render-interval interval)
                 (start 0)
                 (prefix "")
                 chunks)
             (dotimes (index (1- (length text)))
               (unless (zerop (logand mask (ash 1 index)))
                 (push (substring text start (1+ index)) chunks)
                 (setq start (1+ index))))
             (push (substring text start) chunks)
             (dolist (chunk (nreverse chunks))
               (setq prefix (concat prefix chunk))
               (agent-shell-tool-output-tests--notify
                state `((status . "in_progress") (title . "Command")
                        (_meta (terminal_output_delta (data . ,chunk)))))
               (agent-shell-tool-output-tests--notify
                state '((_meta (terminal_output_delta (data . "")))))
               (agent-shell-tool-output-tests--notify state '((title . "Command")))
               (agent-shell--flush-stream-render state)
               (ert-info ((format "Interval %S, partition %d, prefix %S" interval mask prefix))
                 (should (equal (agent-shell-tool-output-tests--body state) prefix)))))))))))

(ert-deftest agent-shell-tool-output-interleaved-streams-test ()
  "Interleaved tools and an agent reply keep separate bodies and groups."
  (agent-shell-tool-output-tests--call-with-shell
   (lambda (state)
     (agent-shell-tool-output-tests--notify
      state '((status . "in_progress") (title . "First")
              (_meta (terminal_output_delta (data . "first start\n")))) "first")
     (agent-shell-tool-output-tests--notify
      state '((status . "in_progress") (title . "Second")
              (_meta (terminal_output_delta (data . "second output\n")))) "second")
     (agent-shell--on-notification
      :state state
      :acp-notification '((method . "session/update")
                          (params (update (sessionUpdate . "agent_message_chunk")
                                          (messageId . "reply")
                                          (content (type . "text") (text . "Agent reply"))))))
     (agent-shell-tool-output-tests--notify
      state '((title . "First renamed")
              (_meta (terminal_output_delta (data . "first end\n")))) "first")
     (agent-shell-tool-output-tests--notify state '((status . "completed")) "second")
     (agent-shell-tool-output-tests--notify state '((status . "completed")) "first")
     (should (string-search "first start\nfirst end" (agent-shell-tool-output-tests--body state "first")))
     (should-not (string-search "second output" (agent-shell-tool-output-tests--body state "first")))
     (should (string-search "second output" (agent-shell-tool-output-tests--body state "second")))
     (should (string-search "Agent reply" (buffer-string)))
     (should (string-search "First renamed" (buffer-string)))
     (should (= (map-elt state :activity-group-count) 1))
     (should-not (map-elt state :stream-render-pending)))))

(ert-deftest agent-shell-tool-output-viewport-and-fold-matrix-test ()
  "Shell and viewport agree across prompt, tool, and group fold settings."
  (dolist (persistent '(nil t))
    (dolist (expanded '(nil t))
      (dolist (groups '(nil t latest))
        (agent-shell-tool-output-tests--call-with-shell
         (lambda (state)
           (let ((agent-shell-persistent-prompt-enabled persistent)
                 (agent-shell-tool-use-expand-by-default expanded)
                 (agent-shell-activity-group-expand-by-default groups)
                 (shell-buffer (current-buffer))
                 (text "\n  first\n```\n# literal\n\n"))
             (with-temp-buffer
               (setq major-mode 'agent-shell-viewport-view-mode)
               (let ((viewport (current-buffer)))
                 (cl-letf (((symbol-function 'agent-shell-viewport--buffer)
                            (lambda (&rest _) viewport))
                           ((symbol-function 'shell-maker--should-auto-scroll-p) #'ignore))
                   (with-current-buffer shell-buffer
                     (agent-shell-tool-output-tests--notify
                      state `((status . "in_progress") (title . "Command")
                              (_meta (terminal_output_delta (data . ,text)))))
                     (agent-shell--flush-stream-render state)
                     (should (equal (agent-shell-tool-output-tests--body state) text))
                     (should (equal (with-current-buffer viewport
                                      (agent-shell-tool-output-tests--body state)) text))
                     (agent-shell-tool-output-tests--notify
                      state '((_meta (terminal_output_delta (data . "last\n")))))
                     (agent-shell-tool-output-tests--notify state '((status . "completed")))
                     (should (equal (agent-shell-tool-output-tests--body state)
                                    (with-current-buffer viewport
                                      (agent-shell-tool-output-tests--body state))))
                     (should (string-search (concat text "last")
                                            (agent-shell-tool-output-tests--body state))))))))))))))

(ert-deftest agent-shell-tool-output-timer-renders-preview-test ()
  "The stream timer renders terminal output without waiting for another delta."
  (agent-shell-tool-output-tests--call-with-shell
   (lambda (state)
     (let ((agent-shell--stream-render-interval 0.01))
       (agent-shell-tool-output-tests--notify
        state '((status . "in_progress") (title . "Command")
                (_meta (terminal_output_delta (data . "timer ")))))
       (agent-shell-tool-output-tests--notify
        state '((_meta (terminal_output_delta (data . "output\n")))))
       (should (map-elt state :stream-render-pending))
       (with-timeout (2 (ert-fail "Terminal preview timer did not run"))
         (while (map-elt state :stream-render-pending)
           (accept-process-output nil 0.005)))
       (should (equal (agent-shell-tool-output-tests--body state) "timer output\n"))
       (should-not (map-elt state :stream-render-timer))
       (agent-shell-tool-output-tests--notify state '((status . "completed")))
       (should-not (map-elt state :stream-render-pending))))))

(ert-deftest agent-shell-tool-output-session-boundary-flushes-test ()
  "Session changes and teardown leave no preview queued for a later session."
  (dolist (event '(session-selected init-session clean-up))
    (agent-shell-tool-output-tests--call-with-shell
     (lambda (state)
       (agent-shell-tool-output-tests--notify
        state '((status . "in_progress") (title . "Command")
                (_meta (terminal_output_delta (data . "output\n")))))
       (agent-shell-tool-output-tests--notify
        state '((_meta (terminal_output_delta (data . "queued\n")))))
       (should (map-elt state :stream-render-pending))
       (agent-shell--emit-event :event event)
       (should-not (map-elt state :stream-render-pending))
       (should-not (map-elt state :stream-render-timer))
       (when (memq event '(session-selected clean-up))
         (should-not agent-shell-openai--terminal-output-snapshots))))))

(ert-deftest agent-shell-tool-output-defers-full-assembly-test ()
  "Live streams and metadata updates assemble complete Markdown only once."
  (agent-shell-tool-output-tests--call-with-shell
   (lambda (state)
     (let ((assembler (symbol-function 'agent-shell--tool-output-chunks-markdown))
           (agent-shell--stream-render-interval nil)
           (assemblies 0)
           (chunk (concat (make-string 100 ?x) "\n")))
       (cl-letf (((symbol-function 'agent-shell--tool-output-chunks-markdown)
                  (lambda (chunks)
                    (setq assemblies (1+ assemblies))
                    (funcall assembler chunks))))
         (dotimes (_ 500)
           (agent-shell-tool-output-tests--notify
            state `((status . "in_progress") (title . "Command")
                    (_meta (terminal_output_delta (data . ,chunk))))))
         (dotimes (_ 10)
           (agent-shell-tool-output-tests--notify state '((title . "Command"))))
         (should (= assemblies 0))
         (agent-shell-tool-output-tests--notify state '((status . "completed")))
         (should (= assemblies 1))
         (agent-shell-tool-output-tests--notify state '((title . "Command")))
         (agent-shell--emit-event :event 'turn-complete)
         (should (= assemblies 1))
         (should-not agent-shell-openai--terminal-output-snapshots))))))

(ert-deftest agent-shell-tool-output-completion-event-retains-content-test ()
  "Subscribers receive the full completed output in the saved tool content."
  (agent-shell-tool-output-tests--call-with-shell
   (lambda (state)
     (let (completed)
       (agent-shell-subscribe-to
        :shell-buffer (current-buffer) :event 'tool-call-update
        :on-event (lambda (event)
                    (when (equal (map-nested-elt event '(:data :tool-call :status)) "completed")
                      (setq completed (map-nested-elt event '(:data :tool-call :content))))))
       (agent-shell-tool-output-tests--notify
        state '((status . "in_progress") (title . "Command")
                (_meta (terminal_output_delta (data . "first\n")))))
       (agent-shell-tool-output-tests--notify
        state '((status . "completed") (_meta (terminal_output_delta (data . "last\n")))))
       (should (equal (agent-shell--tool-call-update-output-markdown `((content . ,completed)))
                      "```\nfirst\nlast\n```"))))))

(ert-deftest agent-shell-tool-output-late-input-details-test ()
  "Input details arriving after output appear without clipping completed logs."
  (dolist (status '("in_progress" "snapshot" "completed" "finalized"))
    (agent-shell-tool-output-tests--call-with-shell
     (lambda (state)
       (agent-shell-tool-output-tests--notify
        state '((status . "in_progress") (title . "Run")
                (_meta (terminal_output_delta (data . "first line\nsecond line\n")))))
       (agent-shell--flush-stream-render state)
       (when (equal status "completed")
         (agent-shell-tool-output-tests--notify state '((status . "completed"))))
       (when (equal status "snapshot")
         (agent-shell-tool-output-tests--notify
          state '((rawOutput (formatted_output . "first line\nsecond line\n")))))
       (when (equal status "finalized")
         (agent-shell--emit-event :event 'turn-complete))
       (agent-shell-tool-output-tests--notify state '((rawInput (command . "printf audit"))))
       (agent-shell--flush-stream-render state)
       (should (string-search "printf audit" (agent-shell-tool-output-tests--body state)))
       (unless (equal status "in_progress")
         (should (string-search "first line\nsecond line" (agent-shell-tool-output-tests--body state)))
         (should-not (string-search "Output continues;" (buffer-string))))))))

(ert-deftest agent-shell-tool-output-repeated-input-stays-incremental-test ()
  "Repeated input metadata does not replace the growing live output body."
  (agent-shell-tool-output-tests--call-with-shell
   (lambda (state)
     (let ((replace-body (symbol-function 'agent-shell-ui--replace-body))
           (agent-shell--stream-render-interval nil)
           (replacements 0))
       (cl-letf (((symbol-function 'agent-shell-ui--replace-body)
                  (lambda (&rest args)
                    (setq replacements (1+ replacements))
                    (apply replace-body args))))
         (dotimes (_ 20)
           (agent-shell-tool-output-tests--notify
            state '((status . "in_progress") (title . "Run") (rawInput (command . "printf audit"))
                    (_meta (terminal_output_delta (data . "line\n"))))))
         (should (= replacements 0))
         (should (string-search (apply #'concat (make-list 20 "line\n"))
                                (agent-shell-tool-output-tests--body state))))))))

(ert-deftest agent-shell-tool-output-late-completed-delta-test ()
  "Output arriving after completion updates the full body and transcript."
  (agent-shell-tool-output-tests--call-with-shell
   (lambda (state)
     (let (transcript)
       (agent-shell-tool-output-tests--notify
        state '((status . "in_progress") (title . "Command")
                (_meta (terminal_output_delta (data . "first line\n")))))
       (agent-shell-tool-output-tests--notify state '((status . "completed")))
       (cl-letf (((symbol-function 'agent-shell--append-transcript)
                  (lambda (&rest args) (setq transcript (plist-get args :text)))))
         (agent-shell-tool-output-tests--notify
          state '((_meta (terminal_output_delta (data . "last line\n"))))))
       (should (string-search "first line\nlast line" (agent-shell-tool-output-tests--body state)))
       (should (string-search "first line\nlast line" transcript))
       (should-not (string-search "Output continues;" (buffer-string)))
       (should-not (map-nested-elt agent-shell-openai--terminal-output-snapshots
                                   '("command" :streaming)))))))

(provide 'agent-shell-tool-output-tests)
;;; agent-shell-tool-output-tests.el ends here
