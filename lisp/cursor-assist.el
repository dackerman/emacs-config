;;; cursor-assist.el --- Cursor-like LLM code assistance for Emacs -*- lexical-binding: t -*-

;; Author: David
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (gptel "0.3.0") (lsp-mode "8.0.0") (company "0.9.13"))

;;; Commentary:
;; This package provides Cursor-like code assistance using GPTel and LSP mode.
;; It automatically suggests code improvements and fixes while you're editing.

;;; Code:

(require 'gptel)
(require 'lsp-mode)
(require 'company)
(require 'cl-lib)
(require 'overlay)

;;; Customization

(defgroup cursor-assist nil
  "Cursor-like LLM assistance in Emacs."
  :group 'tools
  :prefix "cursor-assist-")

(defcustom cursor-assist-idle-delay 0.8
  "Idle delay in seconds before requesting LLM suggestions."
  :type 'number
  :group 'cursor-assist)

(defcustom cursor-assist-context-size 50
  "Number of lines of context to include before and after point."
  :type 'number
  :group 'cursor-assist)

(defcustom cursor-assist-backend 'claude
  "GPTel backend to use for suggestions."
  :type 'symbol
  :group 'cursor-assist)

(defcustom cursor-assist-temperature 0.2
  "Temperature setting for LLM suggestions."
  :type 'number
  :group 'cursor-assist)

(defcustom cursor-assist-suggestion-face '(:inherit font-lock-comment-face :slant italic)
  "Face for suggestion overlays."
  :type 'face
  :group 'cursor-assist)

(defcustom cursor-assist-navigation-face '(:inherit font-lock-keyword-face :box t)
  "Face for suggestion navigation indicators."
  :type 'face
  :group 'cursor-assist)

;;; Internal variables

(defvar cursor-assist--timer nil
  "Timer for triggering suggestion updates.")

(defvar cursor-assist--overlays nil
  "List of active suggestion overlays.")

(defvar cursor-assist--last-buffer-text nil
  "Last analyzed buffer text.")

(defvar cursor-assist--suggestion-positions nil
  "Alist of suggestion positions in the buffer.")

(defvar cursor-assist--current-suggestion-index -1
  "Current suggestion index when navigating.")

(defvar cursor-assist--active-request nil
  "Currently active LLM request.")

;;; Core functionality

(defun cursor-assist--get-buffer-segment ()
  "Get relevant portion of buffer for analysis."
  (let* ((line-count (line-number-at-pos (point-max)))
         (context-start (max 1 (- (line-number-at-pos) cursor-assist-context-size)))
         (context-end (min line-count (+ (line-number-at-pos) cursor-assist-context-size)))
         (start-pos (save-excursion (goto-char (point-min)) (forward-line (1- context-start)) (point)))
         (end-pos (save-excursion (goto-char (point-min)) (forward-line (1- context-end)) (end-of-line) (point))))
    (buffer-substring-no-properties start-pos end-pos)))

(defun cursor-assist--get-lsp-diagnostics ()
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

(defun cursor-assist--prepare-prompt (buffer-text diagnostics-info cursor-pos)
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

(defun cursor-assist--process-response (response)
  "Process suggestion RESPONSE from LLM."
  (setq cursor-assist--active-request nil)
  (setq cursor-assist--suggestion-positions nil)
  
  (cursor-assist--clear-overlays)
  
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
              cursor-assist--suggestion-positions)))
    
    (setq cursor-assist--suggestion-positions 
          (nreverse cursor-assist--suggestion-positions)))
  
  (cursor-assist--display-suggestions))

(defun cursor-assist--request-suggestions ()
  "Request code suggestions from the LLM."
  (when (and cursor-assist-mode
             (not cursor-assist--active-request)
             (derived-mode-p 'prog-mode))
    (let ((buffer-text (cursor-assist--get-buffer-segment))
          (diagnostics (cursor-assist--get-lsp-diagnostics))
          (cursor-pos (cons (line-number-at-pos) (current-column))))
      
      ;; Only process if buffer content has changed
      (unless (equal buffer-text cursor-assist--last-buffer-text)
        (setq cursor-assist--last-buffer-text buffer-text)
        
        ;; Request suggestions from gptel
        (setq cursor-assist--active-request
              (gptel-request
               (cursor-assist--prepare-prompt buffer-text diagnostics cursor-pos)
               :callback #'cursor-assist--process-response
               :backend cursor-assist-backend
               :system "You are a code assistant that specializes in providing code suggestions."
               :temperature cursor-assist-temperature))))))

(defun cursor-assist--clear-overlays ()
  "Clear all suggestion overlays."
  (dolist (ov cursor-assist--overlays)
    (delete-overlay ov))
  (setq cursor-assist--overlays nil))

(defun cursor-assist--get-position-in-buffer (line column)
  "Get buffer position for LINE and COLUMN."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)
    (point)))

(defun cursor-assist--display-suggestions ()
  "Display suggestion overlays in the buffer."
  (when cursor-assist--suggestion-positions
    (dolist (suggestion cursor-assist--suggestion-positions)
      (let* ((pos (car suggestion))
             (text (cdr suggestion))
             (line (car pos))
             (column (cdr pos))
             (buffer-pos (cursor-assist--get-position-in-buffer line column))
             (ov (make-overlay buffer-pos buffer-pos)))
        
        ;; Create overlay for suggestion
        (overlay-put ov 'cursor-assist-suggestion t)
        (overlay-put ov 'after-string
                     (propertize (concat " " text)
                                'face cursor-assist-suggestion-face
                                'cursor-assist-suggestion text))
        
        ;; Store overlay
        (push ov cursor-assist--overlays)))
    
    ;; Add navigation indicators if we have multiple suggestions
    (when (> (length cursor-assist--suggestion-positions) 1)
      (dolist (suggestion cursor-assist--suggestion-positions)
        (let* ((pos (car suggestion))
               (line (car pos))
               (column (cdr pos))
               (buffer-pos (cursor-assist--get-position-in-buffer line column))
               (nav-ov (make-overlay buffer-pos buffer-pos)))
          
          ;; Create overlay for navigation indicator
          (overlay-put nav-ov 'cursor-assist-navigation t)
          (overlay-put nav-ov 'before-string
                       (propertize " [Tab to navigate] "
                                  'face cursor-assist-navigation-face))
          
          ;; Store overlay
          (push nav-ov cursor-assist--overlays))))))

(defun cursor-assist-next-suggestion ()
  "Navigate to the next suggestion."
  (interactive)
  (when cursor-assist--suggestion-positions
    (setq cursor-assist--current-suggestion-index
          (% (1+ cursor-assist--current-suggestion-index)
             (length cursor-assist--suggestion-positions)))
    
    (let* ((suggestion (nth cursor-assist--current-suggestion-index 
                          cursor-assist--suggestion-positions))
           (pos (car suggestion))
           (line (car pos))
           (column (cdr pos))
           (buffer-pos (cursor-assist--get-position-in-buffer line column)))
      
      ;; Move to suggestion position
      (goto-char buffer-pos))))

(defun cursor-assist-previous-suggestion ()
  "Navigate to the previous suggestion."
  (interactive)
  (when cursor-assist--suggestion-positions
    (setq cursor-assist--current-suggestion-index
          (% (+ (length cursor-assist--suggestion-positions)
               (1- cursor-assist--current-suggestion-index))
             (length cursor-assist--suggestion-positions)))
    
    (let* ((suggestion (nth cursor-assist--current-suggestion-index 
                          cursor-assist--suggestion-positions))
           (pos (car suggestion))
           (line (car pos))
           (column (cdr pos))
           (buffer-pos (cursor-assist--get-position-in-buffer line column)))
      
      ;; Move to suggestion position
      (goto-char buffer-pos))))

(defun cursor-assist-accept-suggestion ()
  "Accept the current suggestion."
  (interactive)
  (when cursor-assist--suggestion-positions
    (let* ((suggestion (nth cursor-assist--current-suggestion-index 
                         cursor-assist--suggestion-positions))
           (pos (car suggestion))
           (text (cdr suggestion))
           (line (car pos))
           (column (cdr pos))
           (buffer-pos (cursor-assist--get-position-in-buffer line column)))
      
      ;; Insert the suggestion
      (save-excursion
        (goto-char buffer-pos)
        (insert text))
      
      ;; Clear overlays and update
      (cursor-assist--clear-overlays)
      (cursor-assist--request-suggestions))))

(defun cursor-assist-dismiss-suggestions ()
  "Dismiss all current suggestions."
  (interactive)
  (cursor-assist--clear-overlays)
  (setq cursor-assist--suggestion-positions nil)
  (setq cursor-assist--current-suggestion-index -1))

;;; Company integration

(defun cursor-assist--company-backend (command &optional arg &rest ignored)
  "Company-mode backend for cursor-assist suggestions."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'cursor-assist--company-backend))
    (prefix (when cursor-assist-mode
              (company-grab-symbol)))
    (candidates
     (when (and cursor-assist-mode cursor-assist--suggestion-positions)
       (let ((current-line (line-number-at-pos))
             (current-col (current-column))
             (matching-suggestions nil))
         ;; Find suggestions near the current position
         (dolist (suggestion cursor-assist--suggestion-positions)
           (let* ((pos (car suggestion))
                  (line (car pos))
                  (col (cdr pos))
                  (text (cdr suggestion)))
             ;; Check if suggestion is within small distance of cursor
             (when (and (<= (abs (- line current-line)) 3)
                        (<= (abs (- col current-col)) 10))
               (push text matching-suggestions))))
         matching-suggestions)))
    (annotation (when cursor-assist-mode " [Cursor]"))
    (meta (when cursor-assist-mode "LLM code suggestion"))
    (post-completion (cursor-assist--clear-overlays))))

;; Add cursor-assist as a company backend
(defun cursor-assist--setup-company ()
  "Add cursor-assist company backend."
  (add-to-list 'company-backends 'cursor-assist--company-backend))

;;; Minor mode definition

(defvar cursor-assist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'cursor-assist-next-suggestion)
    (define-key map (kbd "<backtab>") 'cursor-assist-previous-suggestion)
    (define-key map (kbd "RET") 'cursor-assist-accept-suggestion)
    (define-key map (kbd "C-g") 'cursor-assist-dismiss-suggestions)
    map)
  "Keymap for cursor-assist mode.")

;;;###autoload
(define-minor-mode cursor-assist-mode
  "Toggle Cursor-like code assistance mode."
  :lighter " Cursor"
  :keymap cursor-assist-mode-map
  :global nil
  (if cursor-assist-mode
      (progn
        (setq cursor-assist--timer
              (run-with-idle-timer cursor-assist-idle-delay t
                                  #'cursor-assist--request-suggestions))
        (add-hook 'lsp-diagnostics-updated-hook #'cursor-assist--request-suggestions nil t)
        (cursor-assist--setup-company)
        (cursor-assist--request-suggestions))
    (when cursor-assist--timer
      (cancel-timer cursor-assist--timer)
      (setq cursor-assist--timer nil))
    (remove-hook 'lsp-diagnostics-updated-hook #'cursor-assist--request-suggestions t)
    (cursor-assist--clear-overlays)))

;;;###autoload
(define-globalized-minor-mode global-cursor-assist-mode
  cursor-assist-mode
  (lambda ()
    (when (derived-mode-p 'prog-mode)
      (cursor-assist-mode 1))))

(provide 'cursor-assist)
;;; cursor-assist.el ends here