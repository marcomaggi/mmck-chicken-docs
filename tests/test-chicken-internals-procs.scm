;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK CHICKEN Docs
;;;Contents: test program for the procedure objects system operations
;;;Date: May 22, 2019
;;;
;;;Abstract
;;;
;;;	This program tests the procedure objects system operations.
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

(module (test-chicken-internals-procs)
    ()
  (import (scheme)
	  (mmck checks)
	  ;; The ones below are for debugging.
	  (only (chicken base)
		current-error-port
		error
		define-constant)
	  (only (chicken pretty-print)
		pretty-print))

(check-set-mode! 'report-failed)
(check-display "*** testing CHICKEN internals: procedure objects system operations\n")


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


(parameterise ((check-test-name		'decoration))

  ;;Keyed pair as decoration value.
  ;;
  (check
      (internal-body
	(define MY_PROCEDURE_DECORATION_KEY
	  'Sau0oosh8coodahthoa4Ohquic6oshoh)

	(define (make-my-procedure-decoration-value proc)
	  (cons MY_PROCEDURE_DECORATION_KEY proc))

	(define (my-procedure-decoration-value? obj)
	  (and (pair? obj)
	       (eq? MY_PROCEDURE_DECORATION_KEY (car obj))))

	(define (decorate-my-procedure proc decoration-value-payload)
	  (##sys#decorate-lambda proc my-procedure-decoration-value?
				 (lambda (new-proc slotidx)
				   (##sys#setslot new-proc slotidx
						  (make-my-procedure-decoration-value decoration-value-payload))
				   new-proc)))

	(define (my-procedure-decoration-payload proc)
	  (cond ((##sys#lambda-decoration proc my-procedure-decoration-value?)
		 => cdr)
		(else
		 (error 'my-procedure-decoration-payload
		   "expected procedure object decorated with my payload" proc))))

	(define (the-proc)
	  123)

	(set! the-proc
	      (decorate-my-procedure the-proc "ciao"))

	(define another-proc
	  (decorate-my-procedure
	   (lambda ()
             123)
	   "hello"))

	(values (my-procedure-decoration-payload the-proc)
		(my-procedure-decoration-payload another-proc)))
    => "ciao" "hello")

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file
