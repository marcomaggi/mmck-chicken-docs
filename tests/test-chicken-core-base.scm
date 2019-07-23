;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK CHICKEN Docs
;;;Contents: test program for the module (chicken base)
;;;Date: Jul 21, 2019
;;;
;;;Abstract
;;;
;;;	This program tests the module (chicken base).
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

(module (test-chicken-core-base)
    ()
  (import (scheme)
	  (mmck checks)
	  ;; The ones below are for debugging.
	  (chicken base)
	  (chicken condition)
	  (only (chicken pretty-print)
		pretty-print
		pretty-print-width)
	  (chicken condition))

(check-set-mode! 'report-failed)
(check-display "*** testing CHICKEN core: the (chicken base) module\n")


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


(parameterise ((check-test-name		'arithmetic))

  (check (signum 0)		=> 0)
  (check (signum +123)		=> +1)
  (check (signum -123)		=> -1)

  (check (signum 0.)		=> 0.)
  (check (signum +123.)		=> +1.)
  (check (signum -123.)		=> -1.)

  (values))


(parameterise ((check-test-name		'errors))

  ;;Call ERROR without location and message.
  ;;
  (check
      (call/cc
	  (lambda (escape)
	    (with-exception-handler
		(lambda (E)
		  (escape (list (get-condition-property E 'exn 'location)
				(get-condition-property E 'exn 'message)
				(get-condition-property E 'exn 'arguments))))
	      (lambda ()
		(error 1 2 3)))))
    => '(#f 1 (2 3)))

  ;;Call ERROR without message.
  ;;
  (check
      (call/cc
	  (lambda (escape)
	    (with-exception-handler
		(lambda (E)
		  (escape (list (get-condition-property E 'exn 'location)
				(get-condition-property E 'exn 'message)
				(get-condition-property E 'exn 'arguments))))
	      (lambda ()
		(error "something wrong" 1 2 3)))))
    => '(#f "something wrong" (1 2 3)))

  ;;Call ERROR with all the arguments.
  ;;
  (check
      (call/cc
	  (lambda (escape)
	    (with-exception-handler
		(lambda (E)
		  (escape (list (get-condition-property E 'exn 'location)
				(get-condition-property E 'exn 'message)
				(get-condition-property E 'exn 'arguments))))
	      (lambda ()
		(error 'me "something wrong" 1 2 3)))))
    => '(me "something wrong" (1 2 3)))

  (values))


(parameterise ((check-test-name		'lists))

  (check (flatten)		=> '())
  (check (flatten '(1))		=> '(1))
  (check (flatten '(1 (2) 3))	=> '(1 2 3))

  (check
      (flatten '((1 2 3) (4) (((5)))))
    => '(1 2 3 4 5))

  (values))


(parameterise ((check-test-name		'vectors))

;;;; vecotor-copy!

  (check
      (let ((vec.in	(vector 1 2 3))
	    (vec.ou	(make-vector 3 #f)))
	(vector-copy! vec.in vec.ou)
	vec.ou)
    => '#(1 2 3))

  (check
      (let ((vec.in	(vector 1 2 3))
	    (vec.ou	(make-vector 3 #f)))
	(vector-copy! vec.in vec.ou 2)
	vec.ou)
    => '#(1 2 #f))

  (check
      (call/cc
	  (lambda (escape)
	    (with-exception-handler
		(lambda (E)
		  (escape (list (get-condition-property E 'exn 'location)
				(get-condition-property E 'exn 'message)
				(get-condition-property E 'exn 'arguments))))
	      (lambda ()
		(let ((vec.in	(vector 1 2 3))
		      (vec.ou	(make-vector 2 #f)))
		  (vector-copy! vec.in vec.ou 3)
		  vec.ou)))))
    => '(vector-copy!
	 "cannot copy vector - count exceeds length"
	 (#(1 2 3) #(#f #f) 3)))

  (check
      (condition-case
	  (let ((vec.in	(vector 1 2 3))
		(vec.ou	(make-vector 2 #f)))
	    (vector-copy! vec.in vec.ou 3)
	    vec.ou)
	(E (exn bounds)
	   (list (get-condition-property E 'exn 'location)
		 (get-condition-property E 'exn 'message)
		 (get-condition-property E 'exn 'arguments))))
    => '(vector-copy!
	 "cannot copy vector - count exceeds length"
	 (#(1 2 3) #(#f #f) 3)))

  (values))


(parameterise ((check-test-name		'combinators))

;;; conjoin

  (check ((conjoin) 123)		=> #t)
  (check ((conjoin) "123")		=> #t)

;;; --------------------------------------------------------------------
;;; disjoin

  (check ((disjoin) 123)		=> #f)
  (check ((disjoin) "123")		=> #f)

  (values))


(parameterise ((check-test-name		'symbols))

;;; conjoin

  (check (symbol-append)		=> '||)
  (check (symbol-append 'a 'b)		=> 'ab)

  (check (eq? (symbol-append) '||)	=> #t)

  (values))


(parameterise ((check-test-name		'records))

;;; constructor arguments

  (check
      (let ()
	(define-record-type <colour>
	  (make-colour red blue)
	  colour?
	  (red   colour-red)
	  (green colour-green)
	  (blue  colour-blue))

	(define O
	  (make-colour 1 3))

	(values (colour? O)
		(colour-red   O)
		(colour-green O)
		(colour-blue  O)))
    => #t 1 (void) 3)

;;; --------------------------------------------------------------------
;;; accessors and mutators

  (check
      (let ()
	(define-record-type <colour>
	  (make-colour red green blue)
	  colour?
	  (red   colour-red)
	  (green colour-green colour-green-set!)
	  (blue  colour-blue  colour-blue-set!))

	(define O
	  (make-colour 1 2 3))

	(colour-green-set! O 99)

	(values (colour? O)
		(colour-red   O)
		(colour-green O)
		(colour-blue  O)))
    => #t 1 99 3)

  (check
      (let ()
	(define-record-type <colour>
	  (make-colour red green blue)
	  colour?
	  (red   colour-red)
	  (green colour-green (setter colour-green))
	  (blue  colour-blue  colour-blue-set!))

	(define O
	  (make-colour 1 2 3))

	(set! (colour-green O) 99)

	(values (colour? O)
		(colour-red   O)
		(colour-green O)
		(colour-blue  O)))
    => #t 1 99 3)

  (check
      (let ()
	(define (blue O)
	  #f)

	(define-record-type <colour>
	  (make-colour red green blue)
	  colour?
	  (red   colour-red)
	  (green colour-green (setter colour-green))
	  (blue  colour-blue  (setter blue)))

	(define O
	  (make-colour 1 2 3))

	(set! (colour-green O) 88)
	(set! (blue O) 99)

	(values (colour? O)
		(colour-red   O)
		(colour-green O)
		(colour-blue  O)))
    => #t 1 88 99)

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file
