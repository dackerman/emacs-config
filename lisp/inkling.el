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
  "Face for suggestion overlays."
  :type 'face
  :group 'inkling)

(defcustom inkling-navigation-face '(:inherit font-lock-keyword-face :box t)
  "Face for suggestion navigation indicators."
  :type 'face
  :group 'inkling)

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

(defun inkling--process-response (response &optional _info)
  "Process suggestion RESPONSE from LLM.
Optional _INFO contains metadata from gptel about the response."
  (setq inkling--active-request nil)
  (setq inkling--suggestion-positions nil)
  
  (inkling--clear-overlays)
  
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
             (derived-mode-p 'prog-mode))
    (let ((buffer-text (inkling--get-buffer-segment))
          (diagnostics (inkling--get-lsp-diagnostics))
          (cursor-pos (cons (line-number-at-pos) (current-column))))
      
      ;; Only process if buffer content has changed
      (unless (equal buffer-text inkling--last-buffer-text)
        (setq inkling--last-buffer-text buffer-text)
        
        ;; Request suggestions from gptel
        ;; Use global gptel settings - must ensure these are set before activation
        (setq inkling--active-request
              (gptel-request
               (inkling--prepare-prompt buffer-text diagnostics cursor-pos)
               :callback #'inkling--process-response
               :system "You are a code assistant that specializes in providing code suggestions."))))))

(defun inkling--clear-overlays ()
  "Clear all suggestion overlays."
  (dolist (ov inkling--overlays)
    (delete-overlay ov))
  (setq inkling--overlays nil))

(defun inkling--get-position-in-buffer (line column)
  "Get buffer position for LINE and COLUMN."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)
    (point)))

(defun inkling--display-suggestions ()
  "Display suggestion overlays in the buffer."
  (when inkling--suggestion-positions
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
          (push nav-ov inkling--overlays))))))

(defun inkling-next-suggestion ()
  "Navigate to the next suggestion."
  (interactive)
  (when inkling--suggestion-positions
    (setq inkling--current-suggestion-index
          (% (1+ inkling--current-suggestion-index)
             (length inkling--suggestion-positions)))
    
    (let* ((suggestion (nth inkling--current-suggestion-index 
                          inkling--suggestion-positions))
           (pos (car suggestion))
           (line (car pos))
           (column (cdr pos))
           (buffer-pos (inkling--get-position-in-buffer line column)))
      
      ;; Move to suggestion position
      (goto-char buffer-pos))))

(defun inkling-previous-suggestion ()
  "Navigate to the previous suggestion."
  (interactive)
  (when inkling--suggestion-positions
    (setq inkling--current-suggestion-index
          (% (+ (length inkling--suggestion-positions)
               (1- inkling--current-suggestion-index))
             (length inkling--suggestion-positions)))
    
    (let* ((suggestion (nth inkling--current-suggestion-index 
                          inkling--suggestion-positions))
           (pos (car suggestion))
           (line (car pos))
           (column (cdr pos))
           (buffer-pos (inkling--get-position-in-buffer line column)))
      
      ;; Move to suggestion position
      (goto-char buffer-pos))))

(defun inkling-accept-suggestion ()
  "Accept the current suggestion."
  (interactive)
  (when inkling--suggestion-positions
    (let* ((suggestion (nth inkling--current-suggestion-index 
                         inkling--suggestion-positions))
           (pos (car suggestion))
           (text (cdr suggestion))
           (line (car pos))
           (column (cdr pos))
           (buffer-pos (inkling--get-position-in-buffer line column)))
      
      ;; Insert the suggestion
      (save-excursion
        (goto-char buffer-pos)
        (insert text))
      
      ;; Clear overlays and update
      (inkling--clear-overlays)
      (inkling--request-suggestions))))

(defun inkling-dismiss-suggestions ()
  "Dismiss all current suggestions."
  (interactive)
  (inkling--clear-overlays)
  (setq inkling--suggestion-positions nil)
  (setq inkling--current-suggestion-index -1))

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
    (define-key map (kbd "TAB") 'inkling-next-suggestion)
    (define-key map (kbd "<backtab>") 'inkling-previous-suggestion)
    (define-key map (kbd "RET") 'inkling-accept-suggestion)
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

(provide 'inkling)
;;; inkling.el ends here