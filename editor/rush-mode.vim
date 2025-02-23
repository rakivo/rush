;;; rush-mode.el --- Major Mode for editing rush-lang source code

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

" rush.vim - Syntax file for rush-lang

" Define keywords
syn keyword rushKeyword rule build command description depfile phony default subrush

" Highlight keywords
hi def link rushKeyword Keyword

" Comment syntax
syn region rushComment start="# " end="$"
hi def link rushComment Comment

" Set the file type
let b:current_syntax = "rush"
