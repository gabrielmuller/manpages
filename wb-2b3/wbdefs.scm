;; "wbdefs.scm" WB-tree File Based Associative String Data Base System.
;; Copyright (C) 1991, 1992, 1993, 2000 Free Software Foundation, Inc.
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

(java:import "SchlepRT" "Ents" "Wbsys" "Blk" "Lck")

;;; FLC-LEN must be larger than 2 times the maximum number of blocks
;;; which would ever be needed for a freelist split.
(defconst FLC-LEN 10)			;minumum FLC-LEN is 10

;;amount to increase the ENT-TAB by when allocating buffers.
(defconst ENT-TAB-INC 512)

;;;; Byte-array directory names
;;; These should be protected against EBCDIC.
(defconst FLC-byts "FLC")
(defconst FLD-byts "FLD")
(defconst USED-byts "USED")
(defconst BSIZ-byts "BSIZ")
(defconst ROOT-byts "ROOT")
(defconst CLN-byts "CLN")
(defconst AUTHORS-byts "authors")
(defconst NO-byts "")

;;@body Return @1 if a valid error code (-1 @dots{} @var{MAXERR});
;;else 0.
(define (err? x)
  (and (number? x) (negative? x) (>= x MAXERR)))

(define (realerr? x)
  (and (number? x) (<= x RETRYERR) (>= x MAXERR)))

;;@body Not @cname{err?}.
(define (success? x)
  (not (err? x)))

;;;; Return Codes

;;@body Successful execution (0).
(defconst SUCCESS 0)			; successful execution

;;; ERROR Return Codes

;;@noindent
;;Negative integers are used for errors according to increasingly
;;severity, as follows:

;;@body Successful execution; no data present or no change made.
(defconst NOTPRES -1)

;;@body Failure; no damage; caller can retry operation.
(defconst TERMINATED -2)

;;@body Failure; no damage; caller can retry operation.
(defconst RETRYERR -10)

;;@body Failure, no damage, call was in error.
(defconst KEYERR -13)

;;@body Failure, no damage, call was in error.
(defconst ARGERR -15)

;;@body Failure, no damage, out of room in file.
(defconst NOROOM -20)

;;@body Failure, file or object was not of correct type.
(defconst TYPERR -30)

;;@body I/O error, DB may be damaged.
(defconst IOERR -40)

;;@body Internal error, DB may be damaged.
(defconst STRANGERR -45)

;;@body Placeholder code.
(defconst UNKERR -90)

;;@body All error codes are between 0 and @samp{maxerr}.
(defconst MAXERR -100)


(defconst DIR-TYP #x44)			;#\D
(defconst IND-TYP #x54)			;#\T
(defconst SEQ-TYP #x53)			;#\S
(defconst FRL-TYP #x46)			;#\F

(defconst WCB-SAP 1)
(defconst WCB-SAR 2)
(defconst WCB-SAC 4)
(defconst WCB-FAC 8)

(defconst END-OF-CHAIN -1)
(defconst START-OF-CHAIN -2)

(defconst LEAF #x30)			;(char->integer #\0)

;;; BLK PREDICATES

(define (ROOT? blk) (= (BLK:ID blk) (BLK:TOP-ID blk)))

(define (END-OF-CHAIN? blk) (zero? (BLK:NXT-ID blk)))

(define (LEAF? blk) (= (BLK:LEVEL blk) LEAF))

(define (FIELD-LEN blk pos) (byte-ref blk pos))

(define (SET-FIELD-LEN! blk pos len) (byte-set! blk pos len))

;;; This is dangerous.  At the moment all occurences of next-field
;;; have simple expressions for the second argument.

(define (next-field blk pos)
  (+ (FIELD-LEN blk pos) pos 1))

(define (NEXT-CNVPAIR blk pos)
  (next-field blk (next-field blk (+ 1 pos))))

(define (blk-empty? blk)
  (= (BLK:END blk) (next-field blk (+ 1 BLK-DATA-START))))

;;; LCK and ENT tables

;;; If you change this, then change amnesia-ent!
;;; This depends on segid never being less than -1
(define (HASH2INT segid num)
  (remainder (+ (* segid 97) num (* num-buks (+ 1 (quotient 97 num-buks))))
	     num-buks))

(define (SAME-BUK? a-segid a-num b-segid b-num)
  (= (HASH2INT a-segid a-num) (HASH2INT b-segid b-num)))

(define (GET-BUK segid blk-num)
  (vector-ref buk-tab (HASH2INT segid blk-num)))

;;; doesnt wait, ie, returns #F if busy
(define (GET-BUK-LCK? segid blk-num)
  (try-lck? (vector-ref lck-tab (HASH2INT segid blk-num))))

(define (GET-BUK-WAIT segid blk-num)
  (lck! (vector-ref lck-tab (HASH2INT segid blk-num)))
  (vector-ref buk-tab (HASH2INT segid blk-num)))

(define (REL-BUK! segid blk-num)
  (unlck! (vector-ref lck-tab (HASH2INT segid blk-num))))

;;; SET-BUK! assumes BUK is already lcked by caller
(define (SET-BUK! segid blk-num ent)
  (vector-set! buk-tab (HASH2INT segid blk-num) ent))

;;(defconst ACCFOO 'ACCFOO)
(defconst ACCNONE 'ACCNONE)
(defconst ACCREAD 'ACCREAD)
(defconst ACCWRITE 'ACCWRITE)
(defconst ACCPEND 'ACCPEND)

;;; Tokens for use in the PKT:MATCH-POS field
(defconst PASTP 'PASTP)	;not exact match;repeat count of next key will change.
			;match(new-key, after-key) > repeatcount(after-key)
(defconst QPASTP 'QPASTP) ;not exact match;repeat count of next key will not change.
			;match(new-key, after-key) <= repeatcount(after-key)
(defconst MATCH 'MATCH)			;exact match (not split key).
(defconst MATCHEND 'MATCHEND)		;matched split key.
(defconst PASTEND 'PASTEND)		;greater than split key.

;;; Operation codes for SCAN
(defconst REM-SCAN -1)
(defconst COUNT-SCAN 0)
(defconst MODIFY-SCAN 1)
