;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK CHICKEN Docs
;;;Contents: test program for the checks system operations
;;;Date: May 15, 2019
;;;
;;;Abstract
;;;
;;;	This program tests the checks system operations.
;;;
;;;Copyright (C) 2019 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;All rights reserved.
;;;
;;;Redistribution and use  in source and binary forms, with  or without modification,
;;;are permitted provided that the following conditions are met:
;;;
;;;Redistributions of source  code must retain the above copyright  notice, this list
;;;of conditions and the following disclaimer.
;;;
;;;* Redistributions in  binary form must reproduce the above  copyright notice, this
;;;  list  of conditions  and the  following disclaimer  in the  documentation and/or
;;;  other materials provided with the distribution.
;;;
;;;* Neither the name of the author nor  the names of its contributors may be used to
;;;  endorse or  promote products derived  from this software without  specific prior
;;;  written permission.
;;;
;;;THIS SOFTWARE IS PROVIDED BY THE  COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
;;;ANY EXPRESS  OR IMPLIED  WARRANTIES, INCLUDING,  BUT NOT  LIMITED TO,  THE IMPLIED
;;;WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;;IN NO EVENT SHALL THE COPYRIGHT HOLDERS  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;;;INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;;NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED  AND ON ANY THEORY OF LIABILITY,
;;;WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;ARISING  IN ANY  WAY OUT  OF THE  USE OF  THIS SOFTWARE,  EVEN IF  ADVISED OF  THE
;;;POSSIBILITY OF SUCH DAMAGE.
;;;


;;;; units and module header

(module (test-chicken-internals-checks)
    ()
  (import (scheme)
	  (mmck checks)
	  (only (chicken condition)
		condition-case
		print-error-message)
	  (only (chicken blob)
		make-blob)
	  (chicken fixnum)
	  ;; The ones below are for debugging.
	  (only (chicken base)
		current-error-port)
	  (only (chicken pretty-print)
		pretty-print))

(check-set-mode! 'report-failed)
(check-display "*** testing CHICKEN internals: checks system operations\n")


;;;; helpers

(define-syntax define-syntax-rule
  (syntax-rules ()
    ((_ (?name . ?args) ?body0 ?body ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ . ?args)
	  (begin ?body0 ?body ...)))))))

(define-syntax internal-body
  (syntax-rules ()
    ((_ ?body0 ?body ...)
     (let () ?body0 ?body ...))
    ))

(define (debug-print . args)
  (pretty-print args (current-error-port)))

;;; --------------------------------------------------------------------

(define-syntax-rule (check-success ?check-sysop ?good-value)
  (check
      (condition-case
	  (begin
	    (?check-sysop ?good-value (quote caller-name))
	    999)
	((exn type)	#f)
	(E ()		E))
    => 999))

(define-syntax-rule (check-failure ?check-sysop ?bad-value)
  (check-for-false
   (condition-case
       (begin
	 (?check-sysop (quote ?bad-value) (quote caller-name))
	 999)
     ((exn type)	#f)
     (E ()		E))))

(define-syntax-rule (check-checker ?check-sysop ?good-value ?bad-value)
  (check-success ?check-sysop ?good-value)
  (check-failure ?check-sysop ?bad-value))


(parameterise ((check-test-name		'values))

  (check-checker ##sys#check-blob (make-blob 2) "bad")
  (check-checker ##sys#check-pair '(a . b) "bad")
  (check-checker ##sys#check-list '(a b c) "bad")
  (check-checker ##sys#check-string "abc" 777)
  (check-checker ##sys#check-symbol 'ciao "bad")
  (check-checker ##sys#check-vector '#(a b c) "bad")
  (check-checker ##sys#check-char #\A "bad")
  (check-checker ##sys#check-boolean #f "bad")
  (check-checker ##sys#check-boolean #t "bad")

  ;;;(check-checker ##sys#check-special #!eof "bad")

  (check-checker ##sys#check-number 123 "bad")
  (check-checker ##sys#check-integer 123 "bad")
  (check-checker ##sys#check-exact-integer -123 "bad")
  (check-checker ##sys#check-exact-uinteger 123 "bad")
  (check-checker ##sys#check-exact-uinteger 123 -123)
  (check-checker ##sys#check-fixnum 123 "bad")
  (check-checker ##sys#check-exact 123 "bad")
  (check-checker ##sys#check-exact 123 1.23)
  (check-checker ##sys#check-inexact 1.23 "bad")
  (check-checker ##sys#check-inexact 1.23 123)
  (check-checker ##sys#check-real 1.23 "bad")

;;; --------------------------------------------------------------------

  (check
      (condition-case
	  (begin
	    (##sys#check-range 10 0 20 (quote caller-name))
	    999)
	(E (exn bounds)	#f)
	(E ()		E))
    => 999)

  (check
      (condition-case
	  (begin
	    (##sys#check-range -4 0 10 (quote caller-name))
	    999)
	((exn bounds)	#f)
	(E ()		E))
    => #f)

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file
