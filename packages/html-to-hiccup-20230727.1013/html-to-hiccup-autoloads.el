;;; html-to-hiccup-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "html-to-hiccup" "html-to-hiccup.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from html-to-hiccup.el

(autoload 'html-to-hiccup-convert-region "html-to-hiccup" "\
Convert the region between START and END from HTML to Hiccup.
If BODYTAGS is non-nil, skip the first element returned from the HTML parser.

\(fn START END &optional BODYTAGS)" t nil)

(autoload 'html-to-hiccup-yank "html-to-hiccup" "\
Like `yank' but insert killed HTML as Hiccup.
ARGs are the same as in the `yank' command.

Code is copied from the `yank' function in simple.el.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "html-to-hiccup" '("html-to-hiccup-"))

;;;***

;;;### (autoloads nil nil ("html-to-hiccup-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; html-to-hiccup-autoloads.el ends here
