;;; inkling.el --- Intelligent LLM code hints for Emacs -*- lexical-binding: t -*-

;; Author: David
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (gptel "0.3.0") (lsp-mode "8.0.0") (company "0.9.13"))

;;; Commentary:
;; This package provides subtle, intelligent code suggestions using GPTel and LSP mode.
;; It automatically suggests code improvements and fixes while you're editing.

;;; Code:

(require 'gptel)
(require 'lsp-mode)
(require 'company)
(require 'cl-lib)
(require 'overlay)
(require 'tooltip) ;; Built-in tooltip support

;;; Customization

(defgroup inkling nil
  "Intelligent LLM-based code suggestions in Emacs."
  :group 'tools
  :prefix "inkling-")

(defcustom inkling-idle-delay 0.8
  "Idle delay in seconds before requesting LLM suggestions."
  :type 'number
  :group 'inkling)

(defcustom inkling-context-size 50
  "Number of lines of context to include before and after point."
  :type 'number
  :group 'inkling)

(defcustom inkling-backend nil
  "GPTel backend to use for suggestions."
  :type 'symbol
  :group 'inkling)

(defcustom inkling-temperature 0.2
  "Temperature setting for LLM suggestions."
  :type 'number
  :group 'inkling)

(defcustom inkling-suggestion-face '(:inherit font-lock-comment-face :slant italic)
  "Face for suggestion text."
  :type 'face
  :group 'inkling)

(defcustom inkling-highlight-face '(:background "#3a3d4d")
  "Face for highlighting regions that would be changed by a suggestion."
  :type 'face
  :group 'inkling)

(defcustom inkling-preview-face '(:background "#4b4e6d")
  "Face for previewing suggestion changes."
  :type 'face
  :group 'inkling)

(defcustom inkling-navigation-face '(:inherit font-lock-keyword-face :box t)
  "Face for suggestion navigation indicators."
  :type 'face
  :group 'inkling)

(defcustom inkling-popup-background "#333333"
  "Background color for suggestion popups."
  :type 'string
  :group 'inkling)

(defcustom inkling-popup-foreground "#BBBBBB"
  "Foreground color for suggestion popups."
  :type 'string
  :group 'inkling)

(defcustom inkling-popup-max-width 80
  "Maximum width in characters for suggestion popups."
  :type 'number
  :group 'inkling)

(defcustom inkling-display-style 'highlight
  "How to display suggestions.
Options are:
- 'highlight: Highlight regions that would be changed (recommended)
- 'popup: Show suggestions in a popup window
- 'inline: Show suggestions as inline text (old behavior)"
  :type '(choice (const :tag "Highlight" highlight)
                 (const :tag "Popup" popup)
                 (const :tag "Inline" inline))
  :group 'inkling)

;;; Logging and Stats

(defgroup inkling-logging nil
  "Logging and statistics settings for inkling."
  :group 'inkling)

(defcustom inkling-enable-logging t
  "Whether to enable logging of LLM requests and responses."
  :type 'boolean
  :group 'inkling-logging)

(defcustom inkling-log-file "~/.emacs.d/inkling-log.org"
  "File where inkling logs will be stored."
  :type 'string
  :group 'inkling-logging)

(defcustom inkling-log-with-context nil
  "Whether to include buffer context in logs.
Warning: This will store code snippets in the log file."
  :type 'boolean
  :group 'inkling-logging)

(defcustom inkling-token-cost-map
  '((claude-3-7-sonnet-20250219 . ((input . 0.0005) (output . 0.0015)))
    (04-mini . ((input . 0.00010) (output . 0.0003)))
    (gemini-1.5-pro . ((input . 0.0001) (output . 0.0002))))
  "Cost per token in USD for different models (input and output costs)."
  :type '(alist :key-type symbol :value-type (alist :key-type symbol :value-type number))
  :group 'inkling-logging)

;;; Internal variables

(defvar inkling--timer nil
  "Timer for triggering suggestion updates.")

(defvar inkling--overlays nil
  "List of active suggestion overlays.")

(defvar inkling--last-buffer-text nil
  "Last analyzed buffer text.")

(defvar inkling--suggestion-positions nil
  "Alist of suggestion positions in the buffer.")

(defvar inkling--current-suggestion-index -1
  "Current suggestion index when navigating.")

(defvar inkling--active-request nil
  "Currently active LLM request.")

(defvar inkling--current-suggestion nil
  "Current active suggestion text.")

(defvar inkling--suggestion-region nil
  "Region (beg . end) that would be modified by the current suggestion.")

(defvar inkling--preview-active nil
  "Whether the suggestion preview is currently being shown.")

(defvar inkling--preview-overlay nil
  "Overlay used for previewing the suggestion.")

(defvar inkling--original-text nil
  "Original text before preview was applied.")

(defvar inkling--stats-total-tokens 0
  "Total number of tokens used in the current session.")

(defvar inkling--stats-total-cost 0.0
  "Estimated total cost in USD for the current session.")

(defvar inkling--stats-total-requests 0
  "Total number of LLM requests made in the current session.")

(defvar inkling--stats-total-responses 0
  "Total number of LLM responses received in the current session.")

(defvar inkling--stats-backend-usage (make-hash-table :test 'eq)
  "Hash table tracking usage per backend.")

;;; Core functionality

(defun inkling--get-buffer-segment ()
  "Get relevant portion of buffer for analysis."
  (let* ((line-count (line-number-at-pos (point-max)))
         (context-start (max 1 (- (line-number-at-pos) inkling-context-size)))
         (context-end (min line-count (+ (line-number-at-pos) inkling-context-size)))
         (start-pos (save-excursion (goto-char (point-min)) (forward-line (1- context-start)) (point)))
         (end-pos (save-excursion (goto-char (point-min)) (forward-line (1- context-end)) (end-of-line) (point))))
    (buffer-substring-no-properties start-pos end-pos)))

(defun inkling--get-lsp-diagnostics ()
  "Get LSP diagnostic information."
  (when (bound-and-true-p lsp-mode)
    (let* ((diagnostics (lsp--get-buffer-diagnostics))
           (result nil))
      (dolist (diag diagnostics)
        (let* ((range (lsp-diagnostic-range diag))
               (start (lsp-diagnostic-range-start range))
               (line (lsp-position-line start))
               (character (lsp-position-character start))
               (severity (lsp-diagnostic-severity diag))
               (message (lsp-diagnostic-message diag))
               (source (lsp-diagnostic-source diag)))
          (push (format "Diagnostic on line %d, col %d [%s]: %s"
                        (1+ line) character
                        (or source "unknown")
                        message) result)))
      (nreverse result))))

(defun inkling--prepare-prompt (buffer-text diagnostics-info cursor-pos)
  "Create a prompt for LLM with BUFFER-TEXT, DIAGNOSTICS-INFO, and CURSOR-POS."
  (format "
You are acting as an intelligent Emacs code assistant with these goals:
1. Suggest code improvements at the cursor position
2. Fix errors detected by LSP
3. Improve code quality

Current file: %s
Language: %s
Cursor position: line %d, column %d

LSP diagnostics:
%s

CODE SEGMENT:
```
%s
```

INSTRUCTIONS:
- Suggest improvements to the code at or near the cursor position
- If there are LSP diagnostics, fix them
- Your response should be in this format for each suggestion:
  <cursor-position>[line:column]
  <suggested-code>
- ONLY INCLUDE THE CODE, no explanations or markdown
- Multiple suggestions are allowed
"
          (buffer-name)
          (symbol-name major-mode)
          (line-number-at-pos)
          (current-column)
          (if diagnostics-info
              (mapconcat #'identity diagnostics-info "\n")
            "No diagnostics")
          buffer-text))

(defun inkling--log-to-file (type data)
  "Log TYPE and DATA to the inkling log file.
TYPE can be 'request, 'response, or 'error."
  (when inkling-enable-logging
    (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
      (with-temp-buffer
        (when (file-exists-p inkling-log-file)
          (insert-file-contents inkling-log-file))
        (goto-char (point-max))
        (unless (> (buffer-size) 0)
          (insert "#+TITLE: Inkling LLM Interaction Log\n")
          (insert "#+OPTIONS: ^:nil\n\n"))

        (insert (format "* %s - %s\n" timestamp type))

        (cond
         ((eq type 'request)
          (let ((buffer-name (plist-get data :buffer-name))
                (major-mode (plist-get data :major-mode))
                (prompt (plist-get data :prompt))
                (backend (plist-get data :backend))
                (context (plist-get data :context)))
            (insert (format "** Buffer: %s (%s)\n" buffer-name major-mode))
            (insert (format "** Backend: %s\n" backend))
            (when (and inkling-log-with-context context)
              (insert "** Context:\n#+BEGIN_SRC\n")
              (insert context)
              (insert "\n#+END_SRC\n"))
            (insert "** Prompt:\n#+BEGIN_SRC\n")
            (insert prompt)
            (insert "\n#+END_SRC\n")))

         ((eq type 'response)
          (let ((response-text (plist-get data :response))
                (tokens (plist-get data :tokens))
                (cost (plist-get data :cost))
                (model (plist-get data :model)))
            (insert (format "** Model: %s\n" model))
            (insert (format "** Tokens: %d\n" (or tokens 0)))
            (insert (format "** Estimated Cost: $%.6f\n" (or cost 0.0)))
            (insert "** Response:\n#+BEGIN_SRC\n")
            (insert response-text)
            (insert "\n#+END_SRC\n")))

         ((eq type 'error)
          (insert (format "** Error: %s\n" data))))

        ;; Write back to file
        (write-region (point-min) (point-max) inkling-log-file nil 'silent)))))

(defun inkling--estimate-tokens (text)
  "Roughly estimate the number of tokens in TEXT.
This is a simple approximation based on whitespace-separated words."
  (let ((word-count (length (split-string text nil t))))
    ;; Adjust to approximate GPT tokenization (typically 4 chars per token)
    (ceiling (* word-count 1.3))))

(defun inkling--estimate-cost (input-tokens output-tokens model)
  "Estimate cost based on INPUT-TOKENS and OUTPUT-TOKENS for MODEL."
  (let* ((model-costs (cdr (assoc model inkling-token-cost-map)))
         (input-cost (cdr (assoc 'input model-costs)))
         (output-cost (cdr (assoc 'output model-costs))))
    (+ (* input-tokens (or input-cost 0.0001))
       (* output-tokens (or output-cost 0.0003)))))

(defun inkling--update-stats (input-tokens output-tokens model)
  "Update inkling statistics with INPUT-TOKENS, OUTPUT-TOKENS for MODEL."
  (let* ((total-tokens (+ input-tokens output-tokens))
         (cost (inkling--estimate-cost input-tokens output-tokens model))
         (backend-stats (gethash model inkling--stats-backend-usage)))

    ;; Update global stats
    (cl-incf inkling--stats-total-tokens total-tokens)
    (cl-incf inkling--stats-total-cost cost)
    (cl-incf inkling--stats-total-responses)

    ;; Update backend-specific stats
    (if backend-stats
        (progn
          (cl-incf (plist-get backend-stats :tokens) total-tokens)
          (cl-incf (plist-get backend-stats :cost) cost)
          (cl-incf (plist-get backend-stats :uses)))
      (puthash model
               (list :tokens total-tokens
                     :cost cost
                     :uses 1)
               inkling--stats-backend-usage))))

(defun inkling--process-response (response &optional info)
  "Process suggestion RESPONSE from LLM.
INFO contains metadata from gptel about the response."
  (setq inkling--active-request nil)
  (setq inkling--suggestion-positions nil)

  (inkling--clear-overlays)

  ;; Log the response and gather statistics
  (when info
    (let* ((model (plist-get (plist-get info :data) :model))
           (input-text (plist-get (plist-get info :data) :messages))
           (input-tokens (inkling--estimate-tokens (format "%s" input-text)))
           (output-tokens (inkling--estimate-tokens response)))

      ;; Update stats
      (inkling--update-stats input-tokens output-tokens model)

      ;; Log response
      (inkling--log-to-file 'response
                          (list :response response
                                :tokens (+ input-tokens output-tokens)
                                :cost (inkling--estimate-cost input-tokens output-tokens model)
                                :model model))))

  ;; Process the response content
  (with-temp-buffer
    (insert response)
    (goto-char (point-min))

    (while (re-search-forward "\\[\\([0-9]+\\):\\([0-9]+\\)\\]" nil t)
      (let ((line (string-to-number (match-string 1)))
            (column (string-to-number (match-string 2)))
            (pos (point))
            (suggestion-start nil)
            (suggestion-text ""))

        (forward-line)
        (setq suggestion-start (point))

        ;; Extract suggestion text until next pattern or end of buffer
        (if (re-search-forward "\\[\\([0-9]+\\):\\([0-9]+\\)\\]" nil t)
            (progn
              (goto-char (match-beginning 0))
              (setq suggestion-text (buffer-substring-no-properties suggestion-start (point)))
              (goto-char (match-beginning 0)))
          (setq suggestion-text (buffer-substring-no-properties suggestion-start (point-max))))

        ;; Add to suggestion positions list
        (push (cons (cons line column) (string-trim suggestion-text))
              inkling--suggestion-positions)))

    (setq inkling--suggestion-positions
          (nreverse inkling--suggestion-positions)))

  (inkling--display-suggestions))

(defun inkling--request-suggestions ()
  "Request code suggestions from the LLM."
  (when (and inkling-mode
             (not inkling--active-request)
             (derived-mode-p 'prog-mode)
             ;; Skip minibuffer and non-file buffers
             (not (minibufferp))
             (not (string-match-p "\\*.*\\*" (buffer-name))))
    (let ((buffer-text (inkling--get-buffer-segment))
          (diagnostics (inkling--get-lsp-diagnostics))
          (cursor-pos (cons (line-number-at-pos) (current-column))))

      ;; Only process if buffer content has changed
      (unless (equal buffer-text inkling--last-buffer-text)
        (setq inkling--last-buffer-text buffer-text)

        ;; Prepare prompt
        (let ((prompt (inkling--prepare-prompt buffer-text diagnostics cursor-pos)))

          ;; Log the request
          (when inkling-enable-logging
            (cl-incf inkling--stats-total-requests)
            (inkling--log-to-file 'request
                                (list :buffer-name (buffer-name)
                                      :major-mode major-mode
                                      :prompt prompt
                                      :backend gptel-backend
                                      :context (when inkling-log-with-context buffer-text))))

          ;; Request suggestions from gptel
          ;; Use global gptel settings - must ensure these are set before activation
          (setq inkling--active-request
                (gptel-request
                 prompt
                 :callback #'inkling--process-response
                 :system "You are a code assistant that specializes in providing code suggestions.")))))))

(defun inkling--clear-overlays ()
  "Clear all suggestion overlays."
  (dolist (ov inkling--overlays)
    (delete-overlay ov))
  (setq inkling--overlays nil)
  
  ;; Also clear previews when clearing overlays
  (inkling--clear-previews))

(defun inkling--get-position-in-buffer (line column)
  "Get buffer position for LINE and COLUMN."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)
    (point)))

(defun inkling--compute-suggestion-region (pos suggestion-text)
  "Compute region that would be modified by SUGGESTION-TEXT at POS."
  (let ((end-pos nil))
    (save-excursion
      (goto-char pos)
      (setq end-pos (min (point-max) 
                         (save-excursion 
                           (forward-char (length suggestion-text))
                           (point))))
      (cons pos end-pos))))

(defun inkling--display-suggestions-highlight ()
  "Display suggestions by highlighting regions that would be modified."
  (when inkling--suggestion-positions
    ;; Clear any existing overlays and previews
    (inkling--clear-previews)
    
    ;; Use the first suggestion by default
    (setq inkling--current-suggestion-index 0)
    (let* ((suggestion (nth inkling--current-suggestion-index inkling--suggestion-positions))
           (pos (car suggestion))
           (text (cdr suggestion))
           (line (car pos))
           (column (cdr pos))
           (buffer-pos (inkling--get-position-in-buffer line column)))
      
      ;; Store the current suggestion text
      (setq inkling--current-suggestion text)
      
      ;; Compute the region that would be modified
      (setq inkling--suggestion-region (inkling--compute-suggestion-region buffer-pos text))
      
      ;; Create highlight overlay
      (let ((ov (make-overlay (car inkling--suggestion-region) (cdr inkling--suggestion-region))))
        (overlay-put ov 'face inkling-highlight-face)
        (overlay-put ov 'inkling-suggestion t)
        (overlay-put ov 'help-echo "TAB: Accept, S-TAB: Preview, ESC: Dismiss")
        (push ov inkling--overlays)))))

(defun inkling--preview-suggestion ()
  "Preview the current suggestion by temporarily applying it."
  (interactive)
  (when (and inkling--suggestion-positions inkling--suggestion-region)
    ;; Save the original text
    (unless inkling--preview-active
      (setq inkling--original-text 
            (buffer-substring-no-properties 
             (car inkling--suggestion-region) 
             (cdr inkling--suggestion-region)))
      
      ;; Apply the suggestion text temporarily
      (save-excursion
        (let ((inhibit-modification-hooks t))
          (delete-region (car inkling--suggestion-region) (cdr inkling--suggestion-region))
          (goto-char (car inkling--suggestion-region))
          (insert inkling--current-suggestion)))
      
      ;; Create preview overlay on the inserted text
      (setq inkling--preview-overlay 
            (make-overlay (car inkling--suggestion-region)
                         (+ (car inkling--suggestion-region) (length inkling--current-suggestion))))
      (overlay-put inkling--preview-overlay 'face inkling-preview-face)
      (overlay-put inkling--preview-overlay 'priority 200)
      
      ;; Set the preview state
      (setq inkling--preview-active t)
      
      ;; Set up temporary keymap for accepting/canceling preview
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "TAB") 'inkling-accept-suggestion)
        (define-key map [remap self-insert-command] 'inkling--cancel-preview)
        (define-key map (kbd "<return>") 'inkling--cancel-preview)
        (define-key map (kbd "<escape>") 'inkling--cancel-preview)
        (define-key map (kbd "C-g") 'inkling--cancel-preview)
        ;; Use set-transient-map for temporary keybinding
        (set-transient-map map t 'inkling--cancel-preview)))))

(defun inkling--cancel-preview ()
  "Cancel the current suggestion preview."
  (interactive)
  (when inkling--preview-active
    ;; Restore original text
    (save-excursion
      (let ((inhibit-modification-hooks t))
        (delete-region (car inkling--suggestion-region) 
                       (+ (car inkling--suggestion-region) (length inkling--current-suggestion)))
        (goto-char (car inkling--suggestion-region))
        (insert inkling--original-text)))
    
    ;; Remove preview overlay
    (when inkling--preview-overlay
      (delete-overlay inkling--preview-overlay)
      (setq inkling--preview-overlay nil))
    
    ;; Reset preview state
    (setq inkling--preview-active nil)
    (setq inkling--original-text nil))
  
  ;; Allow this function to be called as a hook without arguments
  nil)

(defun inkling--clear-previews ()
  "Clear any active suggestion previews."
  (when inkling--preview-active
    (inkling--cancel-preview))
  (when inkling--preview-overlay
    (delete-overlay inkling--preview-overlay)
    (setq inkling--preview-overlay nil)))

(defun inkling--format-popup-text (text)
  "Format TEXT for display in a popup."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    ;; Limit width of each line for better popup display
    (while (not (eobp))
      (let ((line-end (line-end-position)))
        (when (> (- line-end (line-beginning-position)) inkling-popup-max-width)
          (goto-char (+ (line-beginning-position) inkling-popup-max-width))
          (insert "\n")
          (setq line-end (line-end-position))))
      (forward-line 1))
    (buffer-string)))

(defun inkling--show-popup (pos text)
  "Show a popup at POS with TEXT as content."
  (let ((formatted-text (inkling--format-popup-text text)))
    (save-excursion
      (goto-char pos)
      ;; Create a tooltip with our text
      (tooltip-show formatted-text
                   (list :fg-color inkling-popup-foreground
                         :bg-color inkling-popup-background)))))

(defun inkling--display-suggestions-inline ()
  "Display suggestion overlays directly in the buffer (traditional method)."
  (dolist (suggestion inkling--suggestion-positions)
    (let* ((pos (car suggestion))
           (text (cdr suggestion))
           (line (car pos))
           (column (cdr pos))
           (buffer-pos (inkling--get-position-in-buffer line column))
           (ov (make-overlay buffer-pos buffer-pos)))

      ;; Create overlay for suggestion
      (overlay-put ov 'inkling-suggestion t)
      (overlay-put ov 'after-string
                   (propertize (concat " " text)
                              'face inkling-suggestion-face
                              'inkling-suggestion text))

      ;; Store overlay
      (push ov inkling--overlays)))

  ;; Add navigation indicators if we have multiple suggestions
  (when (> (length inkling--suggestion-positions) 1)
    (dolist (suggestion inkling--suggestion-positions)
      (let* ((pos (car suggestion))
             (line (car pos))
             (column (cdr pos))
             (buffer-pos (inkling--get-position-in-buffer line column))
             (nav-ov (make-overlay buffer-pos buffer-pos)))

        ;; Create overlay for navigation indicator
        (overlay-put nav-ov 'inkling-navigation t)
        (overlay-put nav-ov 'before-string
                     (propertize " [Tab to navigate] "
                                'face inkling-navigation-face))

        ;; Store overlay
        (push nav-ov inkling--overlays)))))

(defun inkling--display-suggestions-popup ()
  "Display suggestions in popup windows."
  (when inkling--suggestion-positions
    ;; Create overlays for navigation indicators
    (dolist (suggestion inkling--suggestion-positions)
      (let* ((pos (car suggestion))
             (text (cdr suggestion))
             (line (car pos))
             (column (cdr pos))
             (buffer-pos (inkling--get-position-in-buffer line column))
             (indicator-ov (make-overlay buffer-pos buffer-pos)))

        ;; Add a clickable indicator that will show the popup
        (overlay-put indicator-ov 'inkling-suggestion t)
        (overlay-put indicator-ov 'after-string
                     (propertize " 💡"
                                'face inkling-navigation-face
                                'mouse-face 'highlight
                                'help-echo "Click to view suggestion"
                                'inkling-suggestion text
                                'keymap (let ((map (make-sparse-keymap)))
                                          (define-key map [mouse-1]
                                            (lambda (event)
                                              (interactive "e")
                                              (inkling--show-popup
                                               buffer-pos
                                               text)))
                                          map)))

        ;; Store overlay
        (push indicator-ov inkling--overlays)))

    ;; For the first suggestion (current index), show the popup immediately
    (when (and inkling--suggestion-positions
               (>= inkling--current-suggestion-index 0)
               (< inkling--current-suggestion-index (length inkling--suggestion-positions)))
      (let* ((suggestion (nth inkling--current-suggestion-index inkling--suggestion-positions))
             (pos (car suggestion))
             (text (cdr suggestion))
             (line (car pos))
             (column (cdr pos))
             (buffer-pos (inkling--get-position-in-buffer line column)))
        (inkling--show-popup buffer-pos text)))))

(defun inkling--display-suggestions ()
  "Display suggestions using the configured display style."
  (when inkling--suggestion-positions
    (pcase inkling-display-style
      ('inline (inkling--display-suggestions-inline))
      ('popup (inkling--display-suggestions-popup))
      ('highlight (inkling--display-suggestions-highlight))
      (_ (inkling--display-suggestions-highlight)))))

(defun inkling-next-suggestion ()
  "Navigate to the next suggestion."
  (interactive)
  (when inkling--suggestion-positions
    ;; Clear existing popups
    (tooltip-hide)

    ;; Update index
    (setq inkling--current-suggestion-index
          (% (1+ inkling--current-suggestion-index)
             (length inkling--suggestion-positions)))

    (let* ((suggestion (nth inkling--current-suggestion-index
                          inkling--suggestion-positions))
           (pos (car suggestion))
           (text (cdr suggestion))
           (line (car pos))
           (column (cdr pos))
           (buffer-pos (inkling--get-position-in-buffer line column)))

      ;; Move to suggestion position
      (goto-char buffer-pos)

      ;; Show popup if using popup display style
      (when (eq inkling-display-style 'popup)
        (inkling--show-popup buffer-pos text)))))

(defun inkling-previous-suggestion ()
  "Navigate to the previous suggestion."
  (interactive)
  (when inkling--suggestion-positions
    ;; Clear existing popups
    (tooltip-hide)

    ;; Update index
    (setq inkling--current-suggestion-index
          (% (+ (length inkling--suggestion-positions)
               (1- inkling--current-suggestion-index))
             (length inkling--suggestion-positions)))

    (let* ((suggestion (nth inkling--current-suggestion-index
                          inkling--suggestion-positions))
           (pos (car suggestion))
           (text (cdr suggestion))
           (line (car pos))
           (column (cdr pos))
           (buffer-pos (inkling--get-position-in-buffer line column)))

      ;; Move to suggestion position
      (goto-char buffer-pos)

      ;; Show popup if using popup display style
      (when (eq inkling-display-style 'popup)
        (inkling--show-popup buffer-pos text)))))

(defun inkling-accept-suggestion ()
  "Accept the current suggestion."
  (interactive)
  (when inkling--suggestion-positions
    (if inkling--preview-active
        ;; If preview is active, just accept it by keeping the changes and clearing state
        (progn
          ;; Remove preview overlay
          (when inkling--preview-overlay
            (delete-overlay inkling--preview-overlay)
            (setq inkling--preview-overlay nil))
          
          ;; Reset preview state but keep the changes
          (setq inkling--preview-active nil)
          (setq inkling--original-text nil))
      
      ;; If no preview, apply the suggestion directly
      (let* ((suggestion (nth inkling--current-suggestion-index
                           inkling--suggestion-positions))
             (pos (car suggestion))
             (text (cdr suggestion))
             (line (car pos))
             (column (cdr pos))
             (buffer-pos (inkling--get-position-in-buffer line column)))

        ;; Hide tooltips
        (tooltip-hide)

        ;; Delete any text in the suggestion region if using highlight mode
        (when (and (eq inkling-display-style 'highlight)
                   inkling--suggestion-region)
          (delete-region (car inkling--suggestion-region) 
                         (cdr inkling--suggestion-region)))

        ;; Insert the suggestion
        (save-excursion
          (goto-char buffer-pos)
          (insert text))))
    
    ;; Clear overlays and update
    (inkling--clear-overlays)
    (inkling--request-suggestions)))

(defun inkling-dismiss-suggestions ()
  "Dismiss all current suggestions."
  (interactive)
  ;; Hide tooltips
  (tooltip-hide)

  ;; Clear any active previews
  (inkling--clear-previews)
  
  ;; Clear overlays
  (inkling--clear-overlays)

  ;; Reset state
  (setq inkling--suggestion-positions nil)
  (setq inkling--current-suggestion-index -1)
  (setq inkling--current-suggestion nil)
  (setq inkling--suggestion-region nil))

;;; Company integration

(defun inkling--company-backend (command &optional arg &rest ignored)
  "Company-mode backend for inkling suggestions."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'inkling--company-backend))
    (prefix (when inkling-mode
              (company-grab-symbol)))
    (candidates
     (when (and inkling-mode inkling--suggestion-positions)
       (let ((current-line (line-number-at-pos))
             (current-col (current-column))
             (matching-suggestions nil))
         ;; Find suggestions near the current position
         (dolist (suggestion inkling--suggestion-positions)
           (let* ((pos (car suggestion))
                  (line (car pos))
                  (col (cdr pos))
                  (text (cdr suggestion)))
             ;; Check if suggestion is within small distance of cursor
             (when (and (<= (abs (- line current-line)) 3)
                        (<= (abs (- col current-col)) 10))
               (push text matching-suggestions))))
         matching-suggestions)))
    (annotation (when inkling-mode " [Inkling]"))
    (meta (when inkling-mode "LLM code suggestion"))
    (post-completion (inkling--clear-overlays))))

;; Add inkling as a company backend
(defun inkling--setup-company ()
  "Add inkling company backend."
  (add-to-list 'company-backends 'inkling--company-backend))

;;; Minor mode definition

(defvar inkling-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'inkling-accept-suggestion)
    (define-key map (kbd "<backtab>") 'inkling--preview-suggestion)
    (define-key map (kbd "<escape>") 'inkling-dismiss-suggestions)
    (define-key map (kbd "C-g") 'inkling-dismiss-suggestions)
    map)
  "Keymap for inkling mode.")

;;;###autoload
(define-minor-mode inkling-mode
  "Toggle Inkling code assistance mode."
  :lighter " Inkling"
  :keymap inkling-mode-map
  :global nil
  (if inkling-mode
      (progn
        (setq inkling--timer
              (run-with-idle-timer inkling-idle-delay t
                                  #'inkling--request-suggestions))
        (add-hook 'lsp-diagnostics-updated-hook #'inkling--request-suggestions nil t)
        (inkling--setup-company)
        (inkling--request-suggestions))
    (when inkling--timer
      (cancel-timer inkling--timer)
      (setq inkling--timer nil))
    (remove-hook 'lsp-diagnostics-updated-hook #'inkling--request-suggestions t)
    (inkling--clear-overlays)))

;;;###autoload
(define-globalized-minor-mode global-inkling-mode
  inkling-mode
  (lambda ()
    (when (derived-mode-p 'prog-mode)
      (inkling-mode 1))))

;;; Command Interface for Logging and Stats

(defun inkling-display-statistics ()
  "Display inkling usage statistics in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*Inkling Stats*")
    (erase-buffer)
    (org-mode)
    (insert "#+TITLE: Inkling Usage Statistics\n\n")

    ;; Insert session totals
    (insert "* Session Totals\n")
    (insert (format "- Total Requests: %d\n" inkling--stats-total-requests))
    (insert (format "- Total Responses: %d\n" inkling--stats-total-responses))
    (insert (format "- Total Tokens Used: %d\n" inkling--stats-total-tokens))
    (insert (format "- Estimated Total Cost: $%.4f\n\n" inkling--stats-total-cost))

    ;; Insert per-model stats
    (insert "* Per-Model Statistics\n")
    (maphash
     (lambda (model stats)
       (insert (format "** %s\n" model))
       (insert (format "- Uses: %d\n" (plist-get stats :uses)))
       (insert (format "- Tokens: %d\n" (plist-get stats :tokens)))
       (insert (format "- Estimated Cost: $%.4f\n\n" (plist-get stats :cost))))
     inkling--stats-backend-usage)

    ;; Cost comparison information
    (insert "* Model Cost Comparison (per 1000 tokens)\n")
    (insert "| Model | Input Cost | Output Cost |\n")
    (insert "|-------+------------+-------------|\n")
    (dolist (model-cost inkling-token-cost-map)
      (let* ((model (car model-cost))
             (costs (cdr model-cost))
             (input-cost (cdr (assoc 'input costs)))
             (output-cost (cdr (assoc 'output costs))))
        (insert (format "| %s | $%.4f | $%.4f |\n"
                        model
                        (* (or input-cost 0.0001) 1000)
                        (* (or output-cost 0.0003) 1000)))))

    ;; Tips for cost optimization
    (insert "\n* Cost Optimization Tips\n")
    (insert "- Consider using smaller models for simpler tasks\n")
    (insert "- Adjust context size to reduce token usage\n")
    (insert "- Increase idle delay to reduce frequency of requests\n")

    ;; Display the buffer
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))))

(defun inkling-open-log ()
  "Open the inkling log file."
  (interactive)
  (find-file inkling-log-file))

(defun inkling-clear-log ()
  "Clear the inkling log file."
  (interactive)
  (when (yes-or-no-p "Really clear the inkling log file? ")
    (with-temp-file inkling-log-file
      (insert "#+TITLE: Inkling LLM Interaction Log\n")
      (insert "#+OPTIONS: ^:nil\n\n"))
    (message "Inkling log cleared")))

(defun inkling-reset-statistics ()
  "Reset all inkling usage statistics."
  (interactive)
  (when (yes-or-no-p "Really reset all inkling statistics? ")
    (setq inkling--stats-total-tokens 0
          inkling--stats-total-cost 0.0
          inkling--stats-total-requests 0
          inkling--stats-total-responses 0
          inkling--stats-backend-usage (make-hash-table :test 'eq))
    (message "Inkling statistics reset")))

;;; Backend switching utilities

(defun inkling-use-backend (backend)
  "Switch to a different BACKEND for inkling.
BACKEND should be a gptel backend object."
  (interactive
   (list (completing-read "Select backend: "
                          '("claude" "openai" "gemini")
                          nil t)))
  (let ((backend-sym (intern backend)))
    (when (boundp backend-sym)
      (setq gptel-backend (symbol-value backend-sym))
      (message "Inkling now using %s backend" backend))))

(provide 'inkling)
;;; inkling.el ends here
