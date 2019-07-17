;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK CHICKEN Docs
;;;Contents: test program for the module (chicken condition)
;;;Date: Jul 17, 2019
;;;
;;;Abstract
;;;
;;;	This program tests the module (chicken condition).
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

(module (test-chicken-core-condition)
    ()
  (import (scheme)
	  (mmck checks)
	  ;; The ones below are for debugging.
	  (only (chicken base)
		current-error-port
		parameterize)
	  (only (chicken pretty-print)
		pretty-print
		pretty-print-width)
	  (chicken condition))

(check-set-mode! 'report-failed)
(check-display "*** testing CHICKEN core: the (chicken condition) module\n")


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
  (parameterize ((pretty-print-width 150))
    (pretty-print args (current-error-port))))

(define (condition-kinds cnd)
  (map car (condition->list cnd)))


(parameterise ((check-test-name		'with-exception-handler))

;;; Test escaping, which would be required with CHICKEN's default exception handler.

  (check
      (call-with-current-continuation
	  (lambda (escape)
	    (with-exception-handler
		(lambda (E)
		  (escape 456))
	      (lambda ()
		123))))
    => 123)

  (check
      (call-with-current-continuation
	  (lambda (escape)
	    (with-exception-handler
		(lambda (E)
		  (escape (get-condition-property E 'demo 'location)
			  (get-condition-property E 'demo 'message)
			  (get-condition-property E 'demo 'irritants)))
	      (lambda ()
		(signal
		 (make-property-condition 'demo
					  'location 'me
					  'message "the message"
					  'irritants '(1 2 3)))))))
    => 'me "the message" '(1 2 3))

  (values))


(parameterise ((check-test-name		'condition-case))

;;; no vars

  (check
      (condition-case
	  (signal (make-property-condition 'demo))
	(()
	 'here))
    => 'here)

  (check
      (condition-case
	  (signal (make-property-condition '&error))
	((&warning)
	 'warning)
	((&error)
	 'error)
	(()
	 'else))
    => 'error)

  (check
      (condition-case
	  (signal (make-property-condition '&who))
	((&warning)
	 'warning)
	((&error)
	 'error)
	(()
	 'else))
    => 'else)

;;; --------------------------------------------------------------------
;;; no vars

  (check
      (condition-case
	  (signal
	   (make-composite-condition (make-property-condition '&error)
				     (make-property-condition '&serious)
				     (make-property-condition '&condition)))
	(var ()
	     (list 'here (condition-kinds var))))
    => '(here (&error &serious &condition)))

  (check
      (condition-case
	  (signal
	   (make-composite-condition (make-property-condition '&error)
				     (make-property-condition '&serious)
				     (make-property-condition '&condition)))
	((&warning)
	 'warning)
	(var (&error)
	     (list 'error (condition-kinds var)))
	(()
	 'else))
    => '(error (&error &serious &condition)))

  (check
      (condition-case
	  (signal
	   (make-composite-condition (make-property-condition '&warning)
				     (make-property-condition '&condition)))
	((&syntax)
	 'syntax)
	((&error)
	 'error)
	(var ()
	     (list 'else (condition-kinds var))))
    => '(else (&warning &condition)))

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file
