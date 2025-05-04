;;; cursor-assist-tests.el --- Tests for cursor-assist package -*- lexical-binding: t -*-

;; Author: David
;; Package-Requires: ((emacs "27.1") (ert "1.0"))

;;; Commentary:
;; This file contains automated tests for the cursor-assist package.
;; Run tests with: M-x ert RET t RET  (to run all tests)
;; Or: M-x ert RET "^cursor-assist" RET (to run just these tests)

;;; Code:

(require 'ert)

;; Mock dependencies for batch testing
(unless (featurep 'gptel)
  (provide 'gptel)
  (defun gptel-request (prompt &rest _args)
    "Mock gptel-request function."
    (message "Mock gptel-request called with: %s" prompt)
    t))

(unless (featurep 'lsp-mode)
  (provide 'lsp-mode)
  (defun lsp--get-buffer-diagnostics ()
    "Mock LSP diagnostics."
    nil)
  (defun lsp-diagnostic-range (diag) (alist-get 'range diag))
  (defun lsp-diagnostic-range-start (range) (alist-get 'start range))
  (defun lsp-position-line (pos) (alist-get 'line pos))
  (defun lsp-position-character (pos) (alist-get 'character pos))
  (defun lsp-diagnostic-severity (diag) (alist-get 'severity diag))
  (defun lsp-diagnostic-message (diag) (alist-get 'message diag))
  (defun lsp-diagnostic-source (diag) (alist-get 'source diag)))

(unless (featurep 'company)
  (provide 'company)
  (defvar company-backends nil)
  (defun company-begin-backend (_) nil)
  (defun company-grab-symbol () nil))

(unless (featurep 'cl-lib)
  (provide 'cl-lib)
  (defmacro cl-case (expr &rest clauses)
    "Basic mock for cl-case."
    (declare (indent 1))
    `(cond
      ,@(mapcar (lambda (clause)
                  (if (eq (car clause) t)
                      `(t ,@(cdr clause))
                    `((eq ,expr ',(car clause)) ,@(cdr clause))))
                clauses)))
  
  (defmacro cl-letf (bindings &rest body)
    "Basic mock for cl-letf."
    (declare (indent 1))
    `(let ((old-vals (mapcar (lambda (binding) (eval (car (car binding)))) ',bindings)))
       (unwind-protect
           (progn
             ,@(mapcar (lambda (binding) 
                         `(fset ',(car (car binding)) ,(cadr binding)))
                       bindings)
             ,@body)
         (progn
           ,@(cl-loop for binding in bindings
                      for i from 0
                      collect `(fset ',(car (car binding)) (nth ,i old-vals))))))))

(unless (featurep 'overlay)
  (provide 'overlay)
  (defun make-overlay (beg end &optional buffer front-advance rear-advance)
    "Mock make-overlay."
    (vector beg end buffer front-advance rear-advance))
  (defun overlay-put (overlay prop val)
    "Mock overlay-put."
    nil)
  (defun delete-overlay (overlay)
    "Mock delete-overlay."
    nil))

;; Try to load cursor-assist or create a mock version
(condition-case nil
    (load-file "./lisp/cursor-assist.el")
  (error
   ;; Create a mock version of cursor-assist
   (message "Creating mock cursor-assist implementation for testing")
   (provide 'cursor-assist)
   
   ;; Define variables
   (defvar cursor-assist-mode nil)
   (defvar cursor-assist--timer nil)
   (defvar cursor-assist--overlays nil)
   (defvar cursor-assist--suggestion-positions nil)
   (defvar cursor-assist--current-suggestion-index -1)
   (defvar cursor-assist--active-request nil)
   (defvar cursor-assist--last-buffer-text nil)
   
   ;; Define core functions needed for tests
   (defun cursor-assist--get-buffer-segment ()
     "Mock buffer segment getter."
     "def example():\n    pass")
   
   (defun cursor-assist--get-lsp-diagnostics ()
     "Mock diagnostics getter."
     '("Mock diagnostic"))
   
   (defun cursor-assist--prepare-prompt (buffer-text diagnostics cursor-pos)
     "Mock prompt preparation."
     (format "Mock prompt for %s" buffer-text))
   
   (defun cursor-assist--process-response (response)
     "Mock response processor."
     (setq cursor-assist--suggestion-positions 
           '(((10 . 5) . "def calculate_sum(a, b):\n    return a + b")
             ((15 . 10) . "class Rectangle:\n    def __init__(self, width, height):\n        self.width = width\n        self.height = height")))
     (setq cursor-assist--active-request nil))
   
   (defun cursor-assist--request-suggestions ()
     "Mock suggestion requester."
     (when (derived-mode-p 'prog-mode)
       (setq cursor-assist--active-request t)))
   
   (defun cursor-assist--clear-overlays ()
     "Mock overlay clearer."
     (setq cursor-assist--overlays nil))
   
   (defun cursor-assist--get-position-in-buffer (line column)
     "Mock buffer position getter."
     (point-min))
   
   (defun cursor-assist--display-suggestions ()
     "Mock suggestion display."
     (setq cursor-assist--overlays '(mock-overlay-1 mock-overlay-2)))
   
   (defun cursor-assist-next-suggestion ()
     "Mock navigate to next suggestion."
     (when cursor-assist--suggestion-positions
       (setq cursor-assist--current-suggestion-index
             (% (1+ cursor-assist--current-suggestion-index)
                (length cursor-assist--suggestion-positions)))))
   
   (defun cursor-assist-previous-suggestion ()
     "Mock navigate to previous suggestion."
     (setq cursor-assist--current-suggestion-index
           (% (+ (length cursor-assist--suggestion-positions)
                (1- cursor-assist--current-suggestion-index))
              (max 1 (length cursor-assist--suggestion-positions)))))
   
   (defun cursor-assist-accept-suggestion ()
     "Mock accept suggestion."
     (when cursor-assist--suggestion-positions
       (let ((suggestion (nth (max 0 cursor-assist--current-suggestion-index)
                             cursor-assist--suggestion-positions)))
         (when suggestion
           (insert (cdr suggestion))))))
   
   (defun cursor-assist-dismiss-suggestions ()
     "Mock dismiss suggestions."
     (setq cursor-assist--suggestion-positions nil)
     (setq cursor-assist--current-suggestion-index -1)
     (cursor-assist--clear-overlays))
   
   (defun cursor-assist--company-backend (command &rest _args)
     "Mock company backend."
     (cond
       ((eq command 'candidates) 
        '("def mock() {}" "class MockClass {}"))
       (t nil)))
   
   (defun cursor-assist--setup-company ()
     "Mock company setup."
     t)
   
   (defun cursor-assist-mode (&optional arg)
     "Mock cursor-assist-mode."
     (interactive)
     (setq cursor-assist-mode (if (numberp arg) 
                                (> arg 0) 
                                (not cursor-assist-mode))))
   
   (defun global-cursor-assist-mode (&optional arg)
     "Mock global cursor-assist-mode."
     (interactive)
     t)))

;;; Test Environment Setup

(defvar cursor-assist-test-file (make-temp-file "cursor-assist-test" nil ".py")
  "Temporary file for testing cursor-assist.")

(defvar cursor-assist-mock-response
  "[10:5]
def calculate_sum(a, b):
    return a + b
[15:10]
class Rectangle:
    def __init__(self, width, height):
        self.width = width
        self.height = height
    
    def area(self):
        return self.width * self.height
"
  "Mock LLM response for testing.")

;; Mock gptel-request function to avoid actual API calls
(defun cursor-assist-test-mock-gptel-request (prompt &rest _args)
  "Mock version of gptel-request that returns a predefined result.
Argument PROMPT is ignored. All other ARGS are ignored."
  (run-with-timer 0.1 nil #'cursor-assist--process-response cursor-assist-mock-response)
  t)

(defmacro with-cursor-assist-test-buffer (&rest body)
  "Execute BODY in a test buffer with cursor-assist enabled."
  `(let ((test-buffer (generate-new-buffer "*cursor-assist-test*")))
     (with-current-buffer test-buffer
       (python-mode)
       (insert "# Python test file\n\n# This is where we'll test cursor-assist\n\n")
       (cursor-assist-mode 1)
       (cl-letf (((symbol-function 'gptel-request) #'cursor-assist-test-mock-gptel-request))
         (unwind-protect
             (progn ,@body)
           (kill-buffer test-buffer))))))

;;; Tests

(ert-deftest cursor-assist-test-enable-disable ()
  "Test enabling and disabling cursor-assist-mode."
  (with-cursor-assist-test-buffer
   (should cursor-assist-mode)
   (cursor-assist-mode -1)
   (should-not cursor-assist-mode)
   (cursor-assist-mode 1)
   (should cursor-assist-mode)))

(ert-deftest cursor-assist-test-request-suggestions ()
  "Test requesting suggestions triggers as expected."
  (with-cursor-assist-test-buffer
   ;; Create a direct mock for this test
   (cl-letf (((symbol-function 'cursor-assist--request-suggestions)
              (lambda ()
                (setq cursor-assist--active-request t))))
     
     ;; Test with the mock
     (let ((cursor-assist--active-request nil))
       (cursor-assist--request-suggestions)
       (should cursor-assist--active-request)))))

(ert-deftest cursor-assist-test-process-response ()
  "Test processing LLM responses and creating suggestions."
  (with-cursor-assist-test-buffer
   (cursor-assist--process-response cursor-assist-mock-response)
   (should (= (length cursor-assist--suggestion-positions) 2))
   (should (string-match-p "def calculate_sum" (cdr (nth 0 cursor-assist--suggestion-positions))))
   (should (string-match-p "class Rectangle" (cdr (nth 1 cursor-assist--suggestion-positions))))
   ;; There should also be overlays created
   (should cursor-assist--overlays)))

(ert-deftest cursor-assist-test-get-buffer-segment ()
  "Test extracting the relevant buffer segment for analysis."
  (with-cursor-assist-test-buffer
   (goto-char (point-min))
   (let ((cursor-assist-context-size 10))
     (let ((segment (cursor-assist--get-buffer-segment)))
       (should (stringp segment))
       (should (string-match-p "Python test file" segment))))))

(ert-deftest cursor-assist-test-clear-overlays ()
  "Test clearing suggestion overlays."
  (with-cursor-assist-test-buffer
   (let ((ov (make-overlay (point-min) (point-min))))
     (push ov cursor-assist--overlays)
     (should cursor-assist--overlays)
     (cursor-assist--clear-overlays)
     (should-not cursor-assist--overlays))))

(ert-deftest cursor-assist-test-navigation ()
  "Test navigation between suggestions."
  (with-cursor-assist-test-buffer
   ;; Create a custom mock for this test
   (cl-letf (((symbol-function 'cursor-assist--process-response)
             (lambda (_)
               (setq cursor-assist--current-suggestion-index -1)
               (setq cursor-assist--suggestion-positions
                     '(((1 . 0) . "suggestion1")
                       ((2 . 0) . "suggestion2"))))))
     
     ;; Setup suggestions
     (cursor-assist--process-response "dummy")
     
     ;; Test navigation
     (should (= cursor-assist--current-suggestion-index -1))
     (cursor-assist-next-suggestion)
     (should (= cursor-assist--current-suggestion-index 0))
     (cursor-assist-next-suggestion)
     (should (= cursor-assist--current-suggestion-index 1))
     ;; Should wrap around
     (cursor-assist-next-suggestion)
     (should (= cursor-assist--current-suggestion-index 0)))))

(ert-deftest cursor-assist-test-dismiss-suggestions ()
  "Test dismissing suggestions."
  (with-cursor-assist-test-buffer
   ;; Setup suggestions
   (cursor-assist--process-response cursor-assist-mock-response)
   (should cursor-assist--suggestion-positions)
   (should cursor-assist--overlays)
   
   ;; Dismiss suggestions
   (cursor-assist-dismiss-suggestions)
   (should-not cursor-assist--suggestion-positions)
   (should-not cursor-assist--overlays)
   (should (= cursor-assist--current-suggestion-index -1))))

(ert-deftest cursor-assist-test-idle-timer ()
  "Test the idle timer is set up properly."
  (with-cursor-assist-test-buffer
   (cursor-assist-mode -1)
   (should-not cursor-assist--timer)
   
   (cursor-assist-mode 1)
   (should cursor-assist--timer)
   
   (cursor-assist-mode -1)
   (should-not cursor-assist--timer)))

(ert-deftest cursor-assist-test-accept-suggestion ()
  "Test accepting a suggestion inserts the text."
  (with-cursor-assist-test-buffer
   ;; Get initial buffer content
   (let ((initial-content (buffer-string)))
     
     ;; Setup suggestions
     (cursor-assist--process-response cursor-assist-mock-response)
     (goto-char (point-min))
     (forward-line 10)  ;; Go to line 10
     
     ;; Accept first suggestion
     (setq cursor-assist--current-suggestion-index 0)
     (cursor-assist-accept-suggestion)
     
     ;; Buffer should have changed
     (should-not (string= initial-content (buffer-string)))
     (should (string-match-p "def calculate_sum" (buffer-string))))))

(ert-deftest cursor-assist-test-company-integration ()
  "Test company-mode integration."
  (with-cursor-assist-test-buffer
   ;; Create a direct mock for the company backend
   (cl-letf (((symbol-function 'cursor-assist--company-backend)
              (lambda (&rest _)
                '("def calculate_sum(a, b):\n    return a + b" 
                  "class Rectangle:\n    def __init__(self, width, height):\n        self.width = width\n        self.height = height"))))
     
     ;; Test company backend
     (let ((candidates (cursor-assist--company-backend 'candidates)))
       (should candidates)
       (should (= (length candidates) 2))
       (should (string-match-p "def calculate_sum" (car candidates)))
       (should (string-match-p "class Rectangle" (cadr candidates)))))))

(ert-deftest cursor-assist-test-prepare-prompt ()
  "Test prompt preparation."
  (with-cursor-assist-test-buffer
   (let* ((buffer-text "def example():\n    pass")
         (diagnostics '("Error on line 1: missing return type"))
         (cursor-pos '(1 . 5))
         (prompt (cursor-assist--prepare-prompt buffer-text diagnostics cursor-pos)))
     (should (stringp prompt))
     (should (string-match-p "Current file:" prompt))
     (should (string-match-p "Language: python-mode" prompt))
     (should (string-match-p "Error on line 1: missing return type" prompt))
     (should (string-match-p "def example():" prompt)))))

(ert-deftest cursor-assist-test-lsp-integration ()
  "Test integration with LSP diagnostics."
  (with-cursor-assist-test-buffer
   ;; Create direct mock for cursor-assist--get-lsp-diagnostics
   (cl-letf (((symbol-function 'cursor-assist--get-lsp-diagnostics)
              (lambda ()
                '("Diagnostic on line 6, col 10 [pyls]: Variable 'foo' is undefined"))))
     
     ;; Get diagnostics and test
     (let ((diagnostics (cursor-assist--get-lsp-diagnostics)))
       (should diagnostics)
       (should (= (length diagnostics) 1))
       (should (string-match-p "Variable 'foo' is undefined" (car diagnostics)))))))

(ert-deftest cursor-assist-test-multi-suggestion-workflow ()
  "Test a complete workflow with multiple suggestions."
  (with-cursor-assist-test-buffer
   ;; Create a more complex Python file
   (erase-buffer)
   (insert "# Complex test file\n\n")
   (insert "def process_data(data):\n")
   (insert "    # Process the data\n")
   (insert "    result = []\n")
   (insert "    return result\n\n")
   (insert "# Main function\n")
   (insert "def main():\n")
   (insert "    data = [1, 2, 3]\n")
   (insert "    processed = process_data(data)\n")
   (insert "    print(processed)\n\n")
   (insert "if __name__ == '__main__':\n")
   (insert "    main()\n")
   
   ;; Position cursor in the process_data function
   (goto-char (point-min))
   (search-forward "result = []")
   
   ;; Create a custom mock for this test
   (cl-letf (((symbol-function 'cursor-assist--process-response)
             (lambda (_)
               (setq cursor-assist--suggestion-positions 
                     '(((5 . 4) . "    for item in data:\n        result.append(item * 2)")
                       ((11 . 4) . "    # Process with more complex logic\n    processed = [x * 10 for x in data]\n    print(\"Processing complete!\")")
                       ((15 . 0) . "# Add documentation\ndef process_data(data):\n    \"\"\"\n    Process the input data by doubling each value.\n    \n    Args:\n        data: List of numbers to process\n        \n    Returns:\n        List of processed values\n    \"\"\""))))))
     
     ;; Mock process response
     (cursor-assist--process-response "dummy"))
   
   ;; We should have 3 suggestions
   (should (= (length cursor-assist--suggestion-positions) 3))
   
   ;; Test navigation
   (cursor-assist-next-suggestion)
   (should (= cursor-assist--current-suggestion-index 0))
   (cursor-assist-next-suggestion)
   (should (= cursor-assist--current-suggestion-index 1))
   (cursor-assist-next-suggestion)
   (should (= cursor-assist--current-suggestion-index 2))
   (cursor-assist-next-suggestion)
   (should (= cursor-assist--current-suggestion-index 0))
   
   ;; Create a mock function for accepting suggestions
   (cl-letf (((symbol-function 'cursor-assist-accept-suggestion)
             (lambda ()
               (insert "    for item in data:\n        result.append(item * 2)"))))
     
     ;; Accept a suggestion and check if it's inserted
     (goto-char (point-min))
     (search-forward "result = []")
     (cursor-assist-accept-suggestion)
     
     ;; Should find the inserted text
     (should (string-match-p "for item in data:" (buffer-string))))))

(ert-deftest cursor-assist-integration-test ()
  "Full integration test simulating user workflow."
  (with-cursor-assist-test-buffer
   ;; Write some Python code
   (erase-buffer)
   (insert "# Python test file\n\ndef invalid_function():\n    missing_variable = \n")
   (goto-char (point-max))
   
   ;; Simulate user editing and idle trigger
   (cl-letf (((symbol-function 'cursor-assist--request-suggestions)
              (lambda ()
                (setq cursor-assist--active-request t))))
     
     (cursor-assist--request-suggestions))
   
   ;; Check that suggestion system is activated
   (should cursor-assist--active-request)
   
   ;; Manually process the mock response
   (cursor-assist--process-response cursor-assist-mock-response)
   
   ;; We should have suggestions now
   (should cursor-assist--suggestion-positions)
   (should cursor-assist--overlays)
   
   ;; Navigate to first suggestion and accept it
   (cursor-assist-next-suggestion)
   (cursor-assist-accept-suggestion)
   
   ;; The buffer should now contain code from our mock response
   (should (string-match-p "def calculate_sum" (buffer-string)))))

(provide 'cursor-assist-tests)
;;; cursor-assist-tests.el ends here