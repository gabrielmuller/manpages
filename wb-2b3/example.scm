;; "example.scm" WB-tree File Based Associative String Data Base System.
;; Copyright (C) 1991, 1992, 1993, 2000, 2010 Free Software Foundation, Inc.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.


;;;	       MUMPS Style Database Phone Book Example

(require 'wb)
(defconst IND-TYP #\T)
(init-wb 75 150 2048)
(define seg (make-seg "mydata" 2048))
;;(close-seg seg #t)
;;(set! seg (open-seg "mydata" #t))	;opens a previously created segment.

(define seg2 (make-seg "empty2" 2048))
(define seg3 (make-seg "empty3" 2048))
(define seg4 (make-seg "empty4" 2048))
(close-seg seg2 #t)
(close-seg seg3 #t)
(close-seg seg4 #t)

(define pb (create-db seg IND-TYP "phone-book")) ;create an array called
					;"phone-book" which will
					;contain the phone book
					;records.
(define pi (create-db seg IND-TYP "phone-index")) ;create an array called
					;"phone-index" which we will
					;use for indexing by phone
					;number.
(define lni (create-db seg IND-TYP "lastname-index"))
					;create an array called
					;"lastname-index" which we will
					;use for indexing by last name
(define record-number 0)

;;;MAKE-NAME is a routine which concatenates its arguments together
;;;separated by control characters.  This assures that the arguments act
;;;as independent subscripts.
(define (make-name arg1 . args)
  (apply string-append
	 (if (number? arg1) (number->string arg1) arg1)
	 (apply append
		(map (lambda (arg)
		       (cond ((equal? "" arg) (list stringofnull))
			     ((number? arg)
			      (set! arg (number->string arg))
			      (list (string (integer->char
					     (min 30 (string-length arg))))
				    arg))
			     (else (list (string (integer->char 30)) arg))))
		     args))))


(bt:put! pb (make-name record-number "LN") "Doe") ;last name
(bt:put! pb (make-name record-number "FN") "Joe") ;first name
(bt:put! pb (make-name record-number "PN") "5551212") ;phone number
(bt:put! pb (make-name record-number "AD1") "13 Hi St.") ;street address
(bt:put! pb (make-name record-number "CITY") "Podunk")
(bt:put! pb (make-name record-number "ST") "NY")
(bt:put! pb (make-name record-number "ZIP") "10000")
(bt:put! lni (make-name "Doe" record-number) "")
					;This adds index entry so that
					;(bt:next lni (make-name "Doe"))
					;will find the record with
					;complete information.
(bt:put! pi (make-name "5551212" record-number) "")
					;similarly for looking up by
					;phone number.

;;; Note we put the record number into the key.  This is so that we
;;; can index records for more than one "Doe".

;(define doe-rec
;  (get-subscript
;   (bt:next lni (make-name "Doe"))		;returns the a name which
;   2))					;includes the record number of
					;the record.

;(bt:get pb (make-name doe-rec "PN"))	;returns Doe's Phone number.

(bt:scan pb 0 "" "zz" (lambda (k v) (print k v) #t) -1)

(close-seg seg #t)
