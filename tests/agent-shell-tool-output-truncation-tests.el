;;; agent-shell-tool-output-truncation-tests.el --- Bounded terminal previews -*- lexical-binding: t; -*-

(require 'ert)
(unless (fboundp 'agent-shell-tool-output-tests--call-with-shell)
  (load (expand-file-name "agent-shell-tool-output-tests.el"
                          (file-name-directory (or load-file-name buffer-file-name))) nil t))

(ert-deftest agent-shell-tool-output-preview-limit-boundaries-test ()
  "Cap at character boundaries, show the notice once, and save the full log."
  (dolist (interval '(nil 30))
    (agent-shell-tool-output-tests--call-with-shell
     (lambda (state)
       (let ((agent-shell--stream-render-interval interval)
             (agent-shell--tool-output-preview-length 8)
             (output "")
             capped transcript)
         (dolist (chunk '("αβ" "" "\n" "γδεζη" "tail" "" "\nmore\n"))
           (setq output (concat output chunk))
           (agent-shell-tool-output-tests--notify
            state `((status . "in_progress") (title . "Command")
                    (_meta (terminal_output_delta (data . ,chunk)))))
           (agent-shell--flush-stream-render state)
           (let ((body (agent-shell-tool-output-tests--body state)))
             (should (string-prefix-p (substring output 0 (min 8 (length output))) body))
             (should (< (length body) 200))
             (when capped (should (equal body capped)))
             (when (> (length output) 8)
               (should (= (length (split-string body "Output continues;")) 2))
               (setq capped body))))
         (cl-letf (((symbol-function 'agent-shell--append-transcript)
                    (lambda (&rest args) (setq transcript (plist-get args :text)))))
           (agent-shell-tool-output-tests--notify state '((status . "completed"))))
         (should (string-search (string-trim-right output) transcript))
         (should-not (string-search "Output continues;" (buffer-string))))))))

(ert-deftest agent-shell-tool-output-preview-large-log-test ()
  "Bound a growing preview, including input resets, and retain the full log."
  (dolist (status '("completed" "failed"))
    (agent-shell-tool-output-tests--call-with-shell
     (lambda (state)
       (let* ((chunk (make-string 4096 ?x))
              (text (apply #'concat (make-list 20 chunk)))
              capped transcript)
         (dotimes (index 20)
           (agent-shell-tool-output-tests--notify
            state `((status . "in_progress") (title . "Command")
                    (_meta (terminal_output_delta (data . ,chunk)))))
           (agent-shell--flush-stream-render state)
           (let ((body (agent-shell-tool-output-tests--body state)))
             (should (< (length body) (+ agent-shell--tool-output-preview-length 200)))
             (when capped (should (equal body capped)))
             (when (= index 8) (setq capped body))))
         (should (= (map-nested-elt agent-shell-openai--terminal-output-snapshots
                                    '("command" :length)) (length text)))
         (agent-shell-tool-output-tests--notify state '((rawInput (command . "printf audit"))))
         (agent-shell--flush-stream-render state)
         (let ((body (agent-shell-tool-output-tests--body state)))
           (should (string-search "printf audit" body))
           (should (= (length (split-string body "Output continues;")) 2))
           (should (< (length body) (+ agent-shell--tool-output-preview-length 250))))
         (cl-letf (((symbol-function 'agent-shell--append-transcript)
                    (lambda (&rest args) (setq transcript (plist-get args :text)))))
           (agent-shell-tool-output-tests--notify state `((status . ,status))))
         (should (string-search text transcript))
         (should-not (string-search "Output continues;" (buffer-string)))
         (should (string-search "characters omitted" (buffer-string))))))))

(provide 'agent-shell-tool-output-truncation-tests)
;;; agent-shell-tool-output-truncation-tests.el ends here
