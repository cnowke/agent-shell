;;; agent-shell-openai.el --- OpenAI agent configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

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
;;
;; This file includes OpenAI-specific configurations.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'acp)
(require 'map)
(require 'seq)
(require 'agent-shell-tool-output)

(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell-welcome-message "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--dwim "agent-shell")
(declare-function agent-shell--tool-call-update-output-markdown "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--flush-stream-render "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")

(cl-defun agent-shell-openai-make-authentication (&key api-key codex-api-key login)
  "Create OpenAI authentication configuration.

API-KEY is the OpenAI API key string or function that returns it.
CODEX-API-KEY is the Codex-specific API key.
LOGIN when non-nil indicates to use login-based authentication."
  (when (> (seq-count #'identity (list api-key codex-api-key login)) 1)
    (error "Cannot specify multiple authentication methods - choose one"))
  (unless (> (seq-count #'identity (list api-key codex-api-key login)) 0)
    (error "Must specify one of :api-key, :codex-api-key, :login"))
  (cond
   (api-key `((:api-key . ,api-key)))
   (codex-api-key `((:codex-api-key . ,codex-api-key)))
   (login `((:login . t)))))

(defcustom agent-shell-openai-authentication
  (agent-shell-openai-make-authentication :login t)
  "Configuration for OpenAI authentication.
For login-based authentication (default):

  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :login t))

For OpenAI API key (string):

  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :api-key \"your-key\"))

For OpenAI API key (function):

  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :api-key (lambda () ...)))

For Codex API key (string):

  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :codex-api-key \"codex-key\"))

For Codex API key (function):

  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :codex-api-key (lambda () ...)))"
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-openai-codex-acp-command
  '("codex-acp")
  "Command and parameters for the OpenAI Codex client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-openai-codex-environment
  nil
  "Environment variables for the OpenAI Codex client.

This should be a list of environment variables to be used when
starting the Codex client process.

Example usage to set custom environment variables:

  (setq agent-shell-openai-codex-environment
        (`agent-shell-make-environment-variables'
         \"MY_VAR\" \"some-value\"
         \"MY_OTHER_VAR\" \"another-value\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-openai-default-model-id
  nil
  "Default Codex model ID.

Must be one of the model ID's displayed under \"Available models\"
when starting a new Codex shell.

Can be set to either a string or a function that returns a string."
  :type '(choice (const nil) string function)
  :group 'agent-shell)

(defcustom agent-shell-openai-default-config-options
  nil
  "Default Codex config options, applied at session start.

An alist of (OPTION . VALUE), using the option IDs and values
advertised by Codex.  This can be used for reasoning effort, for
example:

  (setq agent-shell-openai-default-config-options
        \\='((\"reasoning_effort\" . \"high\")))

Available values may depend on the selected model.
Options are applied in the order listed."
  :type '(alist :key-type string :value-type string)
  :group 'agent-shell)

(defcustom agent-shell-openai-default-session-mode-id
  nil
  "Default Codex session mode ID.

Must be one of the mode ID's displayed under \"Available modes\"
when starting a new Codex shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defun agent-shell-openai--codex-default-auth-request ()
  "Create the Codex default auth request for the current auth config.

Returns a JSON string matching @agentclientprotocol/codex-acp's ACP
authenticate request shape (see its \"DEFAULT_AUTH_REQUEST\")."
  (cond ((map-elt agent-shell-openai-authentication :api-key)
         (let ((api-key (agent-shell-openai-key)))
           (unless api-key
             (user-error "Please set your `agent-shell-openai-authentication'"))
           (json-serialize
            `((methodId . "api-key")
              (_meta . ((api-key . ((apiKey . ,api-key)))))))))
        ((map-elt agent-shell-openai-authentication :codex-api-key)
         (let ((codex-key (agent-shell-openai-key)))
           (unless codex-key
             (user-error "Please set your `agent-shell-openai-authentication'"))
           (json-serialize
            `((methodId . "api-key")
              (_meta . ((api-key . ((apiKey . ,codex-key)))))))))
        (t
         (json-serialize
          '((methodId . "chat-gpt"))))))

(defvar-local agent-shell-openai--terminal-output-snapshots nil
  "Alist of Codex tool-call IDs and terminal output state.
Each entry holds reversed `:chunks', their `:length', and whether the
last rendered output was an incremental `:streaming' preview.  The last
`:status' and `:raw-input' survive updates omitting those protocol fields.")

(defun agent-shell-openai--subscribe-output-cleanup ()
  "Finalize Codex output and release snapshots no longer needed.
Unfinished tools may continue streaming after their prompt turn ends."
  (let (subscription)
    (setq subscription
          (agent-shell-subscribe-to
           :shell-buffer (current-buffer)
           :on-event (lambda (event)
                       (when-let* ((kind (map-elt event :event))
                                   ((memq kind '(turn-complete session-restored session-selected
                                                               init-session error clean-up))))
                         (agent-shell--tool-output-finalize
                          (agent-shell--state) agent-shell-openai--terminal-output-snapshots)
                         (when (memq kind '(session-selected init-session clean-up))
                           (agent-shell--flush-stream-render (agent-shell--state)))
                         (setq agent-shell-openai--terminal-output-snapshots
                               (unless (memq kind '(session-selected clean-up))
                                 (seq-filter
                                  (lambda (entry)
                                    (when-let* ((tool-call (map-nested-elt
                                                            (agent-shell--state)
                                                            (list :tool-calls (car entry)))))
                                      (not (member (map-elt tool-call :status) '("completed" "failed")))))
                                  agent-shell-openai--terminal-output-snapshots)))
                         (unless agent-shell-openai--terminal-output-snapshots
                           (agent-shell-unsubscribe :subscription subscription))))))))

(cl-defun agent-shell-openai--notification-adapter (&key acp-notification)
  "Normalize Codex terminal output in ACP-NOTIFICATION for rendering.

Retain `_meta.terminal_output_delta.data' as reversed chunks by tool-call
ID.  Normalized updates carry `:output-chunks' and, for streamed data,
`:output-delta', `:output-length', and `:output-append'.  The core handler
appends a bounded live preview and assembles the complete output once on
completion.  For example, \"hello\\n\" and \"world\\n\" retain the chunks
\='(\"world\\n\" \"hello\\n\") without copying earlier output.

Only tool calls that stream deltas are tracked.  Once tracked, complete
content or `rawOutput.formatted_output' snapshots replace the accumulated
output.  Status-only updates reuse it, including completion after
`terminal_exit'.  Retain unfinished calls across prompt turns; release
completed snapshots at turn end and all snapshots when switching sessions.

Return ACP-NOTIFICATION after adaptation."
  (when-let* (((equal (map-elt acp-notification 'method) "session/update"))
              (update (map-nested-elt acp-notification '(params update)))
              ((equal (map-elt update 'sessionUpdate) "tool_call_update"))
              (tool-call-id (map-elt update 'toolCallId))
              ((stringp tool-call-id))
              ((or (stringp (map-nested-elt update '(_meta terminal_output_delta data)))
                   (map-elt agent-shell-openai--terminal-output-snapshots tool-call-id)))
              (snapshot (agent-shell--tool-call-update-output-markdown update)))
    (let* ((delta (map-nested-elt update '(_meta terminal_output_delta data)))
           (previous (map-elt agent-shell-openai--terminal-output-snapshots tool-call-id))
           (input (or (map-elt update 'rawInput) (map-elt previous :raw-input)))
           (status (or (map-elt update 'status) (map-elt previous :status)
                       (map-nested-elt (agent-shell--state) (list :tool-calls tool-call-id :status))))
           (replacement (or (not (string-empty-p snapshot))
                            (stringp (map-nested-elt update '(rawOutput formatted_output)))))
           (chunks (cond (replacement (list snapshot))
                         ((stringp delta) (cons delta (map-elt previous :chunks)))
                         (t (map-elt previous :chunks))))
           (size (cond (replacement (length snapshot))
                       ((stringp delta) (+ (map-elt previous :length 0) (length delta)))
                       (t (map-elt previous :length 0)))))
      (unless agent-shell-openai--terminal-output-snapshots
	(agent-shell-openai--subscribe-output-cleanup))
      (setf (map-elt update :output-chunks) chunks
            (map-elt update :output-length) size
            (map-elt update :output-append) (map-elt previous :streaming)
            (map-elt update :output-input-changed) (not (equal input (map-elt previous :raw-input))))
      (cond (replacement)
            ((stringp delta) (setf (map-elt update :output-delta) delta))
            (t (setf (map-elt update :output-unchanged) t)))
      (when-let* (((not replacement))
                  ((member status '("completed" "failed")))
                  ((or (stringp delta) (map-elt update 'status)))
                  (output (agent-shell--tool-output-chunks-markdown chunks))
                  ((not (string-empty-p output))))
        (setf (map-elt update 'content)
              (append (map-elt update 'content)
                      (list `((type . "content")
                              (content (type . "text") (text . ,output)))))))
      (setf (map-elt agent-shell-openai--terminal-output-snapshots tool-call-id)
            (list (cons :chunks chunks)
                  (cons :length size)
                  (cons :raw-input input)
                  (cons :status status)
                  (cons :streaming
			(and (not replacement)
                             (or (stringp delta) (map-elt previous :streaming))
                             (not (member status '("completed" "failed")))))))
      (setf (alist-get 'update (alist-get 'params acp-notification)) update)))
  acp-notification)

(defun agent-shell-openai-make-codex-config ()
  "Create a Codex agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (when (and (boundp 'agent-shell-openai-key) agent-shell-openai-key)
    (user-error "Please migrate to use agent-shell-openai-authentication and eval (setq agent-shell-openai-key nil)"))
  (agent-shell-make-agent-config
   :identifier 'codex
   :mode-line-name "Codex"
   :buffer-name "Codex"
   :shell-prompt "Codex> "
   :shell-prompt-regexp "Codex> "
   :welcome-function #'agent-shell-openai--codex-welcome-message
   :icon-name "openai.png"
   ;; Let codex-acp decide when authentication is needed (via
   ;; DEFAULT_AUTH_REQUEST) rather than eagerly authenticating.
   :needs-authentication nil
   :default-model-id (lambda () (if (functionp agent-shell-openai-default-model-id)
                                    (funcall agent-shell-openai-default-model-id)
                                  agent-shell-openai-default-model-id))
   :default-session-mode-id (lambda () agent-shell-openai-default-session-mode-id)
   :default-config-options (lambda () agent-shell-openai-default-config-options)
   :client-maker (lambda (buffer)
                   (agent-shell-openai-make-codex-client :buffer buffer))
   :notification-adapter #'agent-shell-openai--notification-adapter
   :install-instructions "See https://github.com/agentclientprotocol/codex-acp for installation."))

(defun agent-shell-openai-start-codex ()
  "Start an interactive Codex agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-openai-make-codex-config)
                     :new-shell t))

(cl-defun agent-shell-openai-make-codex-client (&key buffer)
  "Create a Codex client using configured authentication with BUFFER as context.

Uses `agent-shell-openai-authentication' for authentication configuration."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (when (and (boundp 'agent-shell-openai-codex-command) agent-shell-openai-codex-command)
    (user-error "Please migrate to use agent-shell-openai-codex-acp-command and eval (setq agent-shell-openai-codex-command nil)"))
  (cond
   ((map-elt agent-shell-openai-authentication :api-key)
    (let ((api-key (agent-shell-openai-key)))
      (unless api-key
        (user-error "Please set your `agent-shell-openai-authentication'"))
      (agent-shell--make-acp-client :command (car agent-shell-openai-codex-acp-command)
                                    :command-params (cdr agent-shell-openai-codex-acp-command)
                                    :environment-variables (append (list (format "OPENAI_API_KEY=%s" api-key)
                                                                         (format "DEFAULT_AUTH_REQUEST=%s"
                                                                                (agent-shell-openai--codex-default-auth-request)))
                                                                   agent-shell-openai-codex-environment)
                                    :context-buffer buffer)))
   ((map-elt agent-shell-openai-authentication :codex-api-key)
    (let ((codex-key (agent-shell-openai-key)))
      (unless codex-key
        (user-error "Please set your `agent-shell-openai-authentication'"))
      (agent-shell--make-acp-client :command (car agent-shell-openai-codex-acp-command)
                                    :command-params (cdr agent-shell-openai-codex-acp-command)
                                    :environment-variables (append (list (format "CODEX_API_KEY=%s" codex-key)
                                                                         (format "DEFAULT_AUTH_REQUEST=%s"
                                                                                 (agent-shell-openai--codex-default-auth-request)))
                                                                   agent-shell-openai-codex-environment)
                                    :context-buffer buffer)))
   ((map-elt agent-shell-openai-authentication :login)
    (agent-shell--make-acp-client :command (car agent-shell-openai-codex-acp-command)
                                  :command-params (cdr agent-shell-openai-codex-acp-command)
                                  :environment-variables (append (list "OPENAI_API_KEY="
                                                                       (format "DEFAULT_AUTH_REQUEST=%s"
                                                                               (agent-shell-openai--codex-default-auth-request)))
                                                                 agent-shell-openai-codex-environment)
                                  :context-buffer buffer))
   (t
    (error "Invalid authentication configuration"))))

(defun agent-shell-openai--codex-welcome-message (config)
  "Return Codex welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-openai--codex-ascii-art)))
        (message (agent-shell-welcome-message config)))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-openai--codex-ascii-art ()
  "Codex ASCII art.

From https://github.com/openai/codex/blob/main/codex-rs/tui/frames/slug/frame_1.txt."
  (let* ((text (string-trim "
          d-dcottoottd
      dot5pot5tooeeod dgtd
    tepetppgde   egpegxoxeet
   cpdoppttd            5pecet
  odc5pdeoeoo            g-eoot
 xp te  ep5ceet           p-oeet
tdg-p    poep5ged          g e5e
eedee     t55ecep            gee
eoxpe    ceedoeg-xttttttdtt og e
 dxcp  dcte 5p egeddd-cttte5t5te
 oddgd dot-5e   edpppp dpg5tcd5
  pdt gt e              tp5pde
    doteotd          dodtedtg
      dptodgptccocc-optdtep
        epgpexxdddtdctpg
" "\n")))
    (propertize text 'font-lock-face 'font-lock-doc-face)))

(defun agent-shell-openai-key ()
  "Get the OpenAI API key."
  (cond ((stringp (map-elt agent-shell-openai-authentication :api-key))
         (map-elt agent-shell-openai-authentication :api-key))
        ((functionp (map-elt agent-shell-openai-authentication :api-key))
         (condition-case _err
             (funcall (map-elt agent-shell-openai-authentication :api-key))
           (error
            (error "Api key not found.  Check out `agent-shell-openai-authentication'"))))
        ((stringp (map-elt agent-shell-openai-authentication :codex-api-key))
         (map-elt agent-shell-openai-authentication :codex-api-key))
        ((functionp (map-elt agent-shell-openai-authentication :codex-api-key))
         (condition-case _err
             (funcall (map-elt agent-shell-openai-authentication :codex-api-key))
           (error
            (error "Codex API key not found.  Check out `agent-shell-openai-authentication'"))))
        (t
         nil)))

(provide 'agent-shell-openai)

;;; agent-shell-openai.el ends here
