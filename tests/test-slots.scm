;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK CHICKEN Docs
;;;Contents: test program for the slot system operations
;;;Date: May 14, 2019
;;;
;;;Abstract
;;;
;;;	This program tests the slots system operations.
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

(require-library (mmck checks))

(module (test-slots)
    ()
  (import (scheme)
	  (mmck checks)
	  ;; The ones below are for debugging.
	  (only (chicken base)
		current-error-port)
	  (only (chicken pretty-print)
		pretty-print))

(check-set-mode! 'report-failed)
(check-display "*** testing slots\n")


;;;; helpers

(define-syntax define-syntax-rule
  (syntax-rules ()
    ((_ (?name . ?args) ?body0 ?body ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ . ?args)
	  ?body0 ?body ...))))))

(define-syntax internal-body
  (syntax-rules ()
    ((_ ?body0 ?body ...)
     (let () ?body0 ?body ...))
    ))

(define (debug-print . args)
  (pretty-print args (current-error-port)))


(parameterise ((check-test-name		'vectors))

  (define-syntax-rule ($vector-ref ?vector ?slot-index)
    (##sys#slot ?vector ?slot-index))

  (define-syntax-rule ($vector-set! ?vector ?slot-index ?new-value)
    (##sys#setslot ?vector ?slot-index ?new-value))

  (define-syntax-rule ($vector-set-immediate! ?vector ?slot-index ?new-value)
    (##sys#setislot ?vector ?slot-index ?new-value))

  (define-syntax-rule ($vector-length ?vector)
    (##sys#size ?vector))

  (define vec '#(a b c))

;;; --------------------------------------------------------------------

  (check ($vector-length vec)	=> 3)

  (check ($vector-ref vec 0)	=> 'a)
  (check ($vector-ref vec 1)	=> 'b)
  (check ($vector-ref vec 2)	=> 'c)

  (check
      (internal-body
	(define V
	  (vector 'a 'b 'c))

	($vector-set! vec 0 'x)
	($vector-set! vec 1 'y)
	($vector-set! vec 2 'z)

	(values ($vector-ref vec 0)
		($vector-ref vec 1)
		($vector-ref vec 2)))
    => 'x 'y 'z)

  (check
      (internal-body
	(define V
	  (vector 'a 'b 'c))

	($vector-set-immediate! vec 0 'x)
	($vector-set-immediate! vec 1 'y)
	($vector-set-immediate! vec 2 'z)

	(values ($vector-ref vec 0)
		($vector-ref vec 1)
		($vector-ref vec 2)))
    => 'x 'y 'z)

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file
