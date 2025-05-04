;;; cursor-assist-tests.el --- Tests for cursor-assist package -*- lexical-binding: t -*-

;; Author: David
;; Package-Requires: ((emacs "27.1") (gptel "0.3.0") (lsp-mode "8.0.0") (ert "1.0"))

;;; Commentary:
;; This file contains automated tests for the cursor-assist package.
;; Run tests with: M-x ert RET t RET  (to run all tests)
;; Or: M-x ert RET "^cursor-assist" RET (to run just these tests)

;;; Code:

(require 'ert)
(require 'cursor-assist)
(require 'gptel)
(require 'lsp-mode)

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
   (let ((cursor-assist--active-request nil))
     (cursor-assist--request-suggestions)
     (should cursor-assist--active-request))))

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
   ;; Setup suggestions
   (cursor-assist--process-response cursor-assist-mock-response)
   (should (= cursor-assist--current-suggestion-index -1))
   
   ;; Test navigation
   (cursor-assist-next-suggestion)
   (should (= cursor-assist--current-suggestion-index 0))
   
   (cursor-assist-next-suggestion)
   (should (= cursor-assist--current-suggestion-index 1))
   
   ;; Should wrap around
   (cursor-assist-next-suggestion)
   (should (= cursor-assist--current-suggestion-index 0))
   
   ;; Test previous
   (cursor-assist-previous-suggestion)
   (should (= cursor-assist--current-suggestion-index 1))))

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
   ;; Setup suggestions
   (cursor-assist--process-response cursor-assist-mock-response)
   
   ;; Test company backend
   (let ((candidates (cursor-assist--company-backend 'candidates)))
     (should candidates)
     (should (= (length candidates) 2))
     (should (string-match-p "def calculate_sum" (car (last candidates))))
     (should (string-match-p "class Rectangle" (car candidates))))))

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

(ert-deftest cursor-assist-test-get-lsp-diagnostics ()
  "Test retrieving LSP diagnostics."
  (skip-unless (featurep 'lsp-mode))
  (with-cursor-assist-test-buffer
   (let ((diagnostics (cursor-assist--get-lsp-diagnostics)))
     ;; This mostly tests that the function doesn't error when lsp-mode is available
     (should (listp diagnostics)))))

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

;;; Integration Tests

(ert-deftest cursor-assist-integration-test ()
  "Full integration test simulating user workflow."
  (skip-unless (and (featurep 'lsp-mode) (featurep 'gptel)))
  (with-cursor-assist-test-buffer
   ;; Write some Python code
   (erase-buffer)
   (insert "# Python test file\n\ndef invalid_function():\n    missing_variable = \n")
   (goto-char (point-max))
   
   ;; Simulate user editing and idle trigger
   (cursor-assist--request-suggestions)
   
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

(ert-deftest cursor-assist-test-lsp-integration ()
  "Test integration with LSP diagnostics."
  (skip-unless (featurep 'lsp-mode))
  (with-cursor-assist-test-buffer
   ;; Mock LSP diagnostics
   (cl-letf (((symbol-function 'lsp--get-buffer-diagnostics)
              (lambda () 
                '((diagnostic1 . ((range . ((start . ((line . 5) (character . 10)))
                                           (end . ((line . 5) (character . 20)))))
                                 (severity . 1)
                                 (message . "Variable 'foo' is undefined")
                                 (source . "pyls"))))))
             ((symbol-function 'lsp-diagnostic-range) #'car)
             ((symbol-function 'lsp-diagnostic-range-start) #'cdr)
             ((symbol-function 'lsp-position-line) (lambda (pos) (alist-get 'line pos)))
             ((symbol-function 'lsp-position-character) (lambda (pos) (alist-get 'character pos)))
             ((symbol-function 'lsp-diagnostic-severity) (lambda (diag) (alist-get 'severity (cdr diag))))
             ((symbol-function 'lsp-diagnostic-message) (lambda (diag) (alist-get 'message (cdr diag))))
             ((symbol-function 'lsp-diagnostic-source) (lambda (diag) (alist-get 'source (cdr diag)))))
     
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
   
   ;; Process a response with multiple suggestions
   (let ((multi-suggestion-response 
          "[5:4]
    for item in data:
        result.append(item * 2)
[11:4]
    # Process with more complex logic
    processed = [x * 10 for x in data]
    print(\"Processing complete!\")
[15:0]
# Add documentation
def process_data(data):
    \"\"\"
    Process the input data by doubling each value.
    
    Args:
        data: List of numbers to process
        
    Returns:
        List of processed values
    \"\"\""))
     (cursor-assist--process-response multi-suggestion-response))
   
   ;; We should have 3 suggestions
   (should (= (length cursor-assist--suggestion-positions) 3))
   
   ;; Test navigation
   (should (= cursor-assist--current-suggestion-index -1))
   (cursor-assist-next-suggestion)
   (should (= cursor-assist--current-suggestion-index 0))
   (cursor-assist-next-suggestion)
   (should (= cursor-assist--current-suggestion-index 1))
   (cursor-assist-next-suggestion)
   (should (= cursor-assist--current-suggestion-index 2))
   (cursor-assist-next-suggestion)
   (should (= cursor-assist--current-suggestion-index 0))
   
   ;; Accept a suggestion and check if it's inserted
   (goto-char (point-min))
   (search-forward "result = []")
   (cursor-assist-next-suggestion)
   (cursor-assist-accept-suggestion)
   
   ;; Should find the inserted text
   (should (string-match-p "for item in data:" (buffer-string)))))

(provide 'cursor-assist-tests)
;;; cursor-assist-tests.el ends here