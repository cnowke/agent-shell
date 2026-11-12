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
;; Incremental previews and deferred assembly of terminal output.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)

(declare-function agent-shell--on-notification "agent-shell")

(defconst agent-shell--tool-output-preview-length 32768
  "Maximum number of terminal output characters previewed during execution.")

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
      (concat "```\n" (substring output 0 end) "\n```"))))

(defun agent-shell--tool-output-preview (update)
  "Return the new live terminal preview text for normalized UPDATE.
An appended delta only visits its new characters.  A replacement reads
at most `agent-shell--tool-output-preview-length' characters from the
reversed `:output-chunks'.  Metadata-only updates return nil.
Complete output remains available for rendering when execution finishes."
  (unless (map-elt update :output-unchanged)
    (let* ((limit agent-shell--tool-output-preview-length)
           (start (if (map-elt update :output-append)
                      (- (map-elt update :output-length)
                         (length (map-elt update :output-delta)))
                    0))
           (remaining (max 0 (- limit start)))
           (chunks (if (map-elt update :output-append)
                       (list (map-elt update :output-delta))
                     (reverse (map-elt update :output-chunks))))
           parts)
      (while (and chunks (> remaining 0))
        (let ((chunk (seq-first chunks)))
          (push (substring chunk 0 (min remaining (length chunk))) parts)
          (setq remaining (max 0 (- remaining (length chunk)))
                chunks (seq-rest chunks))))
      (when (and (<= start limit) (> (map-elt update :output-length) limit))
        (push "\n\nOutput continues; complete output will appear when the command finishes.\n" parts))
      (when parts
        (propertize (apply #'concat (nreverse parts))
                    'agent-shell-markdown-frozen t
                    'font-lock-face 'fixed-pitch)))))

(defun agent-shell--tool-output-finalize (state snapshots)
  "Render and save unfinished terminal output from SNAPSHOTS in STATE.
The turn may end without a completed tool update, for example on cancellation.
Keep the agent's last reported status rather than inventing a completion."
  (dolist (entry snapshots)
    (when (and (map-elt (cdr entry) :streaming)
               (map-nested-elt state (list :tool-calls (car entry))))
      (agent-shell--on-notification
       :state state
       :acp-notification
       `((method . "session/update")
         (params (update (sessionUpdate . "tool_call_update")
                         (toolCallId . ,(car entry))
                         (:output-chunks . ,(map-elt (cdr entry) :chunks))
                         (:output-final . t))))))))

(provide 'agent-shell-tool-output)
;;; agent-shell-tool-output.el ends here
