;;; rush-mode.el --- Major Mode for editing rush-lang source code -*- lexical-binding: t -*-

;; Copyright (C) 2025 Mark Tyrkba <marktyrkba456@gmail.com>

;; Author: Mark Tyrkba <marktyrkba456@gmail.com>
;; URL: https://github.com/rakivo/rush

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary: Major Mode for editing rush-lang source code
;;
;; Major Mode for editing rush-lang source code.

(defconst rush-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?\# "<")
    (modify-syntax-entry ?\n ">")
    (modify-syntax-entry ?\' "\"")
    (syntax-table))
  "Syntax table for `rush-mode'.")

(eval-and-compile
  (defconst rush-keywords
    '("rule" "build" "command" "description" "depfile")))

(defconst rush-highlights
  `((,(regexp-opt rush-keywords 'symbols) . font-lock-keyword-face)))

;;;###autoload
(define-derived-mode rush-mode prog-mode "rush"
  "Major Mode for editing rush-lang source code."
  :syntax-table rush-mode-syntax-table
  (setq font-lock-defaults '(rush-highlights))
  (setq-local comment-start "#"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rush\\'" . rush-mode))

(provide 'rush-mode)

;;; rush-mode.el ends here
