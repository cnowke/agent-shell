;;; agent-shell-tool-output.el --- Streamed tool output -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Christian Nowke

;; This file is part of agent-shell.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Literal terminal output and deferred assembly for transcripts.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'subr-x)

(declare-function agent-shell--active-requests-p "agent-shell")
(declare-function agent-shell--activity-group-id "agent-shell")
(declare-function agent-shell--activity-group-initial-expanded-p "agent-shell")
(declare-function agent-shell--append-transcript "agent-shell")
(declare-function agent-shell--collapse-expanded-activity-group "agent-shell")
(declare-function agent-shell--delete-fragment "agent-shell")
(declare-function agent-shell--extract-tool-parameters "agent-shell")
(declare-function agent-shell--format-diffs-as-text "agent-shell")
(declare-function agent-shell--format-tool-call-input "agent-shell")
(declare-function agent-shell--longest-backtick-run "agent-shell")
(declare-function agent-shell--make-transcript-tool-call-entry "agent-shell")
(declare-function agent-shell--refresh-activity-group-header "agent-shell")
(declare-function agent-shell--save-tool-call "agent-shell")
(declare-function agent-shell--sync-activity-group-fold "agent-shell")
(declare-function agent-shell--tool-call-update-output-markdown "agent-shell")
(declare-function agent-shell--truncate-tool-output-lines "agent-shell")
(declare-function agent-shell--update-fragment "agent-shell")
(declare-function agent-shell--update-streamed-fragment "agent-shell")
(declare-function agent-shell-make-tool-call-label "agent-shell")
(declare-function agent-shell-elicitation--pending-for-tool-call-p "agent-shell-elicitation")
(declare-function agent-shell-elicitation--questionnaire-p "agent-shell-elicitation")

(defvar agent-shell--activity-group-label)
(defvar agent-shell--transcript-file)
(defvar agent-shell-tool-use-expand-by-default)

(defun agent-shell--tool-output-chunks-markdown (chunks)
  "Join reversed terminal output CHUNKS into a fenced Markdown block.
For example, \='(\"world\\n\" \"hello\\n\") becomes
\"```\\nhello\\nworld\\n```\".  Empty output returns an empty string."
  (let* ((output (apply #'concat (reverse chunks)))
         (end (length output)))
    (while (and (> end 0) (eq (aref output (1- end)) ?\n))
      (setq end (1- end)))
    (if (zerop end)
        ""
      (let ((fence (make-string (max 3 (1+ (agent-shell--longest-backtick-run output))) ?`)))
        (concat fence "\n" (substring output 0 end) "\n" fence)))))

(defun agent-shell--tool-output-stream-text (update append)
  "Return literal terminal text to insert for normalized UPDATE.
APPEND inserts only `:output-delta'.  Otherwise rebuild the body from
reversed `:output-chunks' so a newly rendered command includes all output.
Metadata-only appends leave the body untouched."
  (unless (and append (map-elt update :output-unchanged)
               (not (map-elt update :output-input-changed)))
    (propertize
     (if append
         (map-elt update :output-delta)
       (apply #'concat (reverse (map-elt update :output-chunks))))
     'agent-shell-markdown-frozen t
     'font-lock-face 'fixed-pitch)))

(cl-defun agent-shell--update-tool-call-output (&key state update finalize)
  "Render tool-call output from normalized UPDATE in STATE.
FINALIZE saves unfinished output on interruption or history replay without
changing the tool's reported status, events, or activity-group fold state.
Retain the rendered `:output-chunks' on the tool call until finalization;
buffered history may contain later output from turns omitted during replay."
  (let* ((tool-call-id (map-elt update 'toolCallId))
         (tool-call (map-nested-elt state (list :tool-calls tool-call-id)))
         (completed (member (map-elt tool-call :status) '("completed" "failed")))
         (streaming (and (not finalize)
                         (map-elt update :output-chunks)
                         (not (member (map-elt update 'status) '("completed" "failed")))
                         (not (and (map-elt update :output-input-changed)
                                   completed))
                         (or (and (not completed) (stringp (map-elt update :output-delta)))
                             (and (map-elt update :output-unchanged)
                                  (or (not (map-elt update :output-input-changed))
                                      (map-elt update :output-append))))))
         (preview-append (and (map-contains-key tool-call :output-chunks)
                              (not (map-elt update :output-input-changed))
                              (or (map-elt update :output-append)
                                  (map-elt update :output-unchanged))))
         (render-labels (or (not streaming) (not preview-append)
                            (map-elt update :output-labels-changed)))
         (output (unless streaming
                   (concat "\n\n" (agent-shell--tool-call-update-output-markdown update) "\n\n")))
         (diff-text (unless streaming (agent-shell--format-diffs-as-text (map-elt tool-call :diffs))))
         (full-body (unless streaming
                      (if diff-text (concat output "\n\n" diff-text) output)))
         (body-text (if streaming
                        (agent-shell--tool-output-stream-text update preview-append)
                      (agent-shell--truncate-tool-output-lines full-body)))
         (tool-call-labels (agent-shell-make-tool-call-label state tool-call-id))
         (group-id (agent-shell--activity-group-id state tool-call-id))
         (command-block (when-let* ((command (map-elt tool-call :command)))
                          (concat "```console\n" command "\n```")))
         (input-block (when (and (member (map-elt tool-call :kind) '(nil "other"))
                                 (map-elt tool-call :raw-input)
                                 (not command-block)
                                 (not (agent-shell-elicitation--questionnaire-p
                                       (map-elt tool-call :raw-input))))
                        (agent-shell--format-tool-call-input (map-elt tool-call :raw-input)))))
    (when (and (map-elt update :output-chunks)
               (or (not streaming) body-text (stringp (map-elt update :output-delta))))
      (setf (map-elt (map-elt (map-elt state :tool-calls) tool-call-id) :output-chunks)
            (and streaming (map-elt update :output-chunks))))
    (when finalize
      (agent-shell--save-tool-call
       state tool-call-id
       (list (cons :content `(((type . "content")
                               (content (type . "text") (text . ,(string-trim output)))))))))
    (when (or finalize (member (map-elt update 'status) '("completed" "failed"))
              (and completed (stringp (map-elt update :output-delta))))
      (agent-shell--append-transcript
       :text (agent-shell--make-transcript-tool-call-entry
              :status (map-elt tool-call :status)
              :title (map-elt tool-call :title)
              :kind (map-elt tool-call :kind)
              :description (map-elt tool-call :description)
              :command (map-elt tool-call :command)
              :parameters (agent-shell--extract-tool-parameters (map-elt tool-call :raw-input))
              :output (if (map-elt update :output-chunks) full-body body-text))
       :file-path agent-shell--transcript-file))
    (when (member (map-elt update 'status) '("completed" "failed"))
      (agent-shell--delete-fragment :state state :block-id (format "permission-%s" tool-call-id)))
    (when (and streaming preview-append body-text)
      (agent-shell--update-streamed-fragment
       :state state :block-id tool-call-id
       :body (propertize body-text 'agent-shell-ui-preserve-whitespace t)
       :render-body nil
       :above-last-prompt (not (agent-shell--active-requests-p state))))
    (when render-labels
      (agent-shell--update-fragment
       :state state
       :block-id tool-call-id
       :label-left (map-elt tool-call-labels :status)
       :label-right (map-elt tool-call-labels :title)
       :group-id group-id
       :group-label agent-shell--activity-group-label
       :group-expanded (agent-shell--activity-group-initial-expanded-p)
       :body (cond
              (streaming
               (when (and (not preview-append) body-text)
                 (propertize
                  (if (or command-block input-block)
                      (concat (or command-block input-block) "\n\n" body-text)
                    body-text)
                  'agent-shell-ui-preserve-whitespace t)))
              ((agent-shell-elicitation--pending-for-tool-call-p
                :state state :tool-call-id tool-call-id)
               "")
              ((or command-block input-block)
               (concat (or command-block input-block) "\n\n" (string-trim body-text)))
              (t (string-trim body-text)))
       :render-body (or (not streaming)
                        (and (not preview-append) (or command-block input-block) t))
       :expanded agent-shell-tool-use-expand-by-default
       :above-last-prompt (not (agent-shell--active-requests-p state))))
    (when render-labels
      (agent-shell--refresh-activity-group-header state group-id)
      (unless finalize
        (agent-shell--sync-activity-group-fold :state state :group-id group-id)))))

(defun agent-shell--tool-output-finalize (state snapshots)
  "Render and save unfinished terminal output from SNAPSHOTS in STATE.
The turn may end without a completed tool update, for example on cancellation.
Keep the agent's last reported status rather than inventing a completion.
Finalize the chunks retained by rendered previews, which may differ from
SNAPSHOTS when history replay omits later turns."
  (let ((expanded (map-elt state :expanded-activity-group)))
    (dolist (entry snapshots)
      (when-let* ((chunks (map-nested-elt state (list :tool-calls (car entry) :output-chunks))))
        (agent-shell--update-tool-call-output
         :state state :finalize t
         :update (list (cons 'toolCallId (car entry))
                       (cons :output-chunks chunks)))
        (setf (map-elt (cdr entry) :streaming) nil)))
    (unless expanded
      (agent-shell--collapse-expanded-activity-group state))))

(provide 'agent-shell-tool-output)
;;; agent-shell-tool-output.el ends here
