;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK CHICKEN Docs
;;;Contents: test program for the structs system operations
;;;Date: May 15, 2019
;;;
;;;Abstract
;;;
;;;	This program tests the structs system operations.
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

(module (test-chicken-internals-structs)
    ()
  (import (scheme)
	  (mmck checks)
	  ;; The ones below are for debugging.
	  (only (chicken base)
		current-error-port)
	  (only (chicken pretty-print)
		pretty-print))

(check-set-mode! 'report-failed)
(check-display "*** testing CHICKEN internals: structs system operations\n")


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


(parameterise ((check-test-name		'vanilla-definition))

  (define-syntax-rule (make-<colour> ?red ?green ?blue)
    (##sys#make-structure '<colour> ?red ?green ?blue))

  (define-syntax-rule (colour? ?obj)
    (##sys#structure? ?obj '<colour>))

  (define-syntax-rule (check-colour ?obj ?loc)
    (##core#check
     (##sys#check-structure ?obj '<colour> (quote ?loc))))

  (define-syntax-rule (colour-red ?stru)
    (check-colour ?stru 'colour-red)
    (##sys#slot ?stru 1))

  (define-syntax-rule (colour-green ?stru)
    (check-colour ?stru 'colour-green)
    (##sys#slot ?stru 2))

  (define-syntax-rule (colour-blue ?stru)
    (check-colour ?stru 'colour-blue)
    (##sys#slot ?stru 3))

  (define-syntax-rule (colour-red-set! ?stru ?new-red-value)
    (check-colour ?stru 'colour-red)
    (##sys#setslot ?stru 1 ?new-red-value))

  (define-syntax-rule (colour-green-set! ?stru ?new-green-value)
    (check-colour ?stru 'colour-green)
    (##sys#setslot ?stru 2 ?new-green-value))

  (define-syntax-rule (colour-blue-set! ?stru ?new-blue-value)
    (check-colour ?stru 'colour-blue)
    (##sys#setslot ?stru 3 ?new-blue-value))

;;; --------------------------------------------------------------------

  (check
      (let ((col (make-<colour> 0.2 0.3 0.4)))
	(check-colour col 'here)
	(##sys#slot col 0))
    => '<colour>)

  (check-for-true  (colour? (make-<colour> 0.2 0.3 0.4)))
  (check-for-false (colour? "ciao"))

  (check
      (let ((col (make-<colour> 0.2 0.3 0.4)))
	(values (colour-red   col)
		(colour-green col)
		(colour-blue  col)))
    => 0.2 0.3 0.4)

  (check
      (let ((col (make-<colour> 0.2 0.3 0.4)))
	(colour-red-set!   col 0.5)
	(colour-green-set! col 0.6)
	(colour-blue-set!  col 0.7)
	(values (colour-red   col)
		(colour-green col)
		(colour-blue  col)))
    => 0.5 0.6 0.7)

  #;(check (colour-red "ciao") => #f)

  (values))


(parameterise ((check-test-name		'predicates))

  (check
      (let ((stru (##sys#make-structure '<colour> 1 2 3)))
	(##sys#generic-structure? stru))
    => #t)

  (values))


(parameterise ((check-test-name		'inspection))

  ;;How to retrieve a struct's symbol name.
  ;;
  (check
      (##sys#slot (##sys#make-structure '<colour> 1 2 3) 0)
    => '<colour>)

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file
