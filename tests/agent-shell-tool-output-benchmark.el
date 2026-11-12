;;; agent-shell-tool-output-benchmark.el --- Terminal stream benchmark -*- lexical-binding: t; -*-

;;; Commentary:
;; Initialize package dependencies, add the checkout to load-path, load this
;; file, and call `agent-shell-tool-output-benchmark-run' from batch Emacs.
;; For example, :kib 2048 :sessions 7 :flush-every 16 :history-turns 120
;; streams 2 MiB per shell across seven labeled conversations.
;; With sibling dependency checkouts, run:
;; emacs -Q --batch -L . -L ../acp -L ../shell-maker \
;;   -l tests/agent-shell-tool-output-benchmark.el \
;;   --eval '(agent-shell-tool-output-benchmark-run :kib 2048)'

;;; Code:

(require 'agent-shell)
(require 'agent-shell-chat-mode)
(require 'profiler)
(unless (fboundp 'agent-shell-tool-output-tests--call-with-shell)
  (load (expand-file-name "agent-shell-tool-output-tests.el"
                          (file-name-directory (or load-file-name buffer-file-name))) nil t))

(defun agent-shell-tool-output-benchmark--with-shells (count history function &optional states)
  "Call FUNCTION with COUNT isolated shells containing HISTORY prior turns.
STATES accumulates their states while the temporary buffers remain alive."
  (if (zerop count)
      (funcall function (reverse states))
    (agent-shell-tool-output-tests--call-with-shell
     (lambda (state)
       (when (> history 0)
         (let ((prompt (copy-marker (point-min) t)))
           (goto-char (point-min))
           (dotimes (_ history)
             (insert (propertize "Test> " 'font-lock-face
                                 '(comint-highlight-prompt comint-highlight-prompt)
                                 'field 'output)
                     "Earlier question\n"
                     (propertize "<shell-maker-end-of-prompt>" 'invisible t 'shell-maker--marker t)
                     "\nEarlier response.\n\n"))
           (set-marker (car comint-last-prompt) prompt)
           (set-marker prompt nil)
           (goto-char (point-max)))
         (agent-shell-chat-mode 1))
       (unwind-protect
           (agent-shell-tool-output-benchmark--with-shells
            (1- count) history function (cons state states))
         (when (bound-and-true-p agent-shell-chat-mode)
           (agent-shell-chat-mode -1)))))))

(cl-defun agent-shell-tool-output-benchmark-run
    (&key (kib 2048) (sessions 1) (flush-every 16) (history-turns 0) profile-file)
  "Stream KIB KiB per shell across SESSIONS isolated command buffers.
Flush every FLUSH-EVERY chunks; zero defers the entire burst until the end.
HISTORY-TURNS enables chat labels over that many preceding prompt turns.
PROFILE-FILE, when non-nil, receives a CPU profile for `profiler-find-profile'.
For example, :kib 512 :sessions 7 exercises 3.5 MiB of aggregate output.
Print and return timings, GC statistics, and buffer/transcript sizes."
  (when (and profile-file (profiler-running-p))
    (error "An existing profiler is running"))
  (agent-shell-tool-output-benchmark--with-shells
   sessions history-turns
   (lambda (states)
     (let* ((chunk (substring (apply #'concat (make-list 64 "ordinary terminal output\n"))
                              0 1024))
            (gc-cons-threshold (* 64 1024 1024))
            (gc-cons-percentage 0.4)
            (collections gcs-done)
            (gc-start gc-elapsed)
            (started (float-time))
            (transcript-size 0)
            stream-time live-size result)
       (when profile-file (profiler-cpu-start profiler-sampling-interval))
       (unwind-protect
           (cl-letf (((symbol-function 'shell-maker--process)
                      (lambda () (get-buffer-process (current-buffer))))
                     ((symbol-function 'agent-shell--append-transcript)
                      (lambda (&rest args)
                        (setq transcript-size (+ transcript-size (length (plist-get args :text)))))))
             (dotimes (index kib)
               (dolist (state states)
                 (with-current-buffer (map-elt state :buffer)
                   (agent-shell-tool-output-tests--notify
                    state `((status . "in_progress") (title . "Command")
                            (_meta (terminal_output_delta (data . ,chunk)))))))
               (when (and (> flush-every 0) (zerop (% (1+ index) flush-every)))
                 (dolist (state states)
                   (with-current-buffer (map-elt state :buffer)
                     (agent-shell--flush-stream-render state)))
                 (when (> history-turns 0) (accept-process-output nil 0.001))))
             (dolist (state states)
               (with-current-buffer (map-elt state :buffer)
                 (agent-shell--flush-stream-render state)))
             (setq stream-time (- (float-time) started)
                   live-size (apply #'+ (mapcar (lambda (state)
                                                  (with-current-buffer (map-elt state :buffer)
                                                    (buffer-size))) states))
                   started (float-time))
             (dolist (state states)
               (with-current-buffer (map-elt state :buffer)
                 (agent-shell-tool-output-tests--notify state '((status . "completed")))))
             (setq result
                   (list :source (symbol-file 'agent-shell--on-notification)
                         :sessions sessions :kib-per-session kib
                         :flush-every flush-every :history-turns history-turns
                         :stream-seconds stream-time :completion-seconds (- (float-time) started)
                         :gc-count (- gcs-done collections) :gc-seconds (- gc-elapsed gc-start)
                         :live-characters live-size :transcript-characters transcript-size)))
         (when profile-file
           (let ((log (profiler-cpu-log)))
             (profiler-cpu-stop)
             (profiler-write-profile (profiler-make-profile :type 'cpu :log log :timestamp (current-time))
                                     profile-file))))
       (prin1 result)
       (terpri)
       result))))

(provide 'agent-shell-tool-output-benchmark)
;;; agent-shell-tool-output-benchmark.el ends here
