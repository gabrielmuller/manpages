;; "test.scm" WB-tree File Based Associative String Data Base System.
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

(require 'stdio)
(slib:load-source (in-vicinity (program-vicinity) "wbsys"))

;(define diagout stdout)

;;  call (SCANHAN root-handle [test-prev? #f] [verbose? #t])

(define (scanhan han . args)
  (define errors 0)
  (define test-prev (if (> (length args) 0) (car args) #f))
  (define verbose? (if (> (length args) 1) (cadr args) #f))
  (dprintf "FORWARD key scan\n")
  (let loop ((key #f) (prior #f) (init #t) (list ()))
    (cond (verbose? (dprintf "NEXT KEY IS ") (write key) (newline)))
    (if (and test-prev (not init))
	(let ((prev-key (bt:prev han key)))
	  (cond ((or (and prev-key prior (not (string=? prev-key prior)))
		     (and prev-key (not prior))
		     (and prior (not prev-key) (not (string=? prior ""))))
		 (set! errors (+ 1 errors))
		 (dprintf "NEXT/PREV error: key= ")  (write prior) (newline)
		 (dprintf "   next= ")  (write key)
		 (dprintf " prev= ")  (write prev-key) (newline))
;;;		(else
;;;		 (dprintf "PREV of ") (write key)
;;;		 (dprintf " OK. \n"))
		)))
    (cond ((or (and key (not (equal? "" key))) init)
	   (loop (bt:next han key) key #f (if key (cons key list) list)))
	  (else
	   (if test-prev
	       (dprintf "%s: %d next/prev errors found.\n" 'SCANHAN errors))
	   (dprintf "%s: %d items found.\n" 'SCANHAN (length list))
	   list))))

(define (count-keys han)
  (let loop ((key "") (ct 0) (init #t))
    (if (or (and key (not (equal? "" key))) init)
	(loop (bt:next han key) (+ ct 1) #f)
	ct)))

(define (scanb han)
  (dprintf "REVERSE key scan\n")
  (let loop ((key #f) (init #t) (list ()))
    (dprintf "PREV KEY IS ")  (write key) (newline)
    (cond ((or (and key (not (equal? "" key))) init)
	   (loop (bt:prev han key) #f (if key (cons key list) list)))
	  (else
	   (dprintf "%s: %d items found.\n" 'SCANB (length list))
	   list))))

(define (db-size han)
  (+ (SEG:USED (HAN:SEG han)) 1))

(define current-bt #f)
(define current-seg #f)
(define add-key-num 0)
(define add-str "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz")

;; ADD! count [first-key add-key-num] [key-increment 1]  [value-string "abc...xyz"x2]

(define (add! n . args)
  (let ((cur-key-num (if (> (length args) 0) (car args) add-key-num))
	(incr (if (> (length args) 1) (cadr args) 1))
	(addstr (if (> (length args) 2) (caddr args) add-str))
	)
    (do ((i 1 (+ i 1)))
	((> i n))
      (let* ((key-str (number->string cur-key-num)))
	(set! cur-key-num (+ incr cur-key-num))
;;;	(dprintf "putting %s\n" key-str)
	(bt:put! current-bt key-str addstr)))
    (set! add-key-num (max add-key-num cur-key-num))))

;; REMOVE! count first-key [key-increment 1] [unused-arg]

(define (remove! n cur-key . args)
  (let ((incr (if (> (length args) 0) (car args) 1))
	(start-trace (if (> (length args) 1) (cadr args) #f)))
    (do ((i 1 (+ i 1)))
	((> i n) i)
      (let ((key-str (number->string cur-key)))
	(set! cur-key (+ incr cur-key))
;;;	(dprintf "deleting %s\n" key-str)
	(bt:rem! current-bt key-str)))))

;; old test code, still useful!

(define (test0! b-han)
  (bt:put! b-han "foo" "bar")
  (bt:put! b-han "foz" "oof")
  (bt:put! b-han "fonz" "zonf")
  (bt:put! b-han "foo" "raboof")
  (bt:put! b-han "food" "thought")
  (scanhan b-han)
  (bt:rem! b-han "foo")
  (bt:rem! b-han "fonz")
  ;;(tscan-full b-han)
  (bt:put! b-han "foo" "foo=bar")
  (bt:put! b-han "foz" "foz=oof")
  (bt:put! b-han "fonz" "fonz=zonf")
  (bt:put! b-han "foo" "foo=raboof")
  (bt:put! b-han "food" "food=for thought")
  (bt:put! b-han "very very very long key field" "very very very longer value field")
  (scanhan b-han))

;;; THIS IS TEST CASE #1.
;;; It is used to test the addition of data elements with similar names
;;; to see if the repeat count routine is working correctly
(define (test1)
  (define a-han (create-bt current-seg IND-TYP 0))
  (bt:put! a-han "cat" "6")
  (bt:put! a-han "caddy" "4")
  (bt:put! a-han "catalytic" "7")
  (bt:put! a-han "cadalack" "3")
  (bt:put! a-han "catastrophy" "10")
  (bt:put! a-han "catastrophic" "9")
  (bt:put! a-han "cad" "2")
  (bt:put! a-han "cadjole" "5")
  (bt:put! a-han "cataract" "8")
  (bt:put! a-han "cacky" "1")
  (bt:put! a-han "d" "12")
  (scanhan a-han)
  (dprintf "   TEST 1: nexting on block 'a' index 'cadz'\n")
  (let* ((ans-str (bt:next a-han "cadz")))
    (dprintf "%s\n" ans-str))
  (dprintf "   TEST 1: removing 'd' from 'a'\n")
  (bt:rem! a-han "d")
  (scanhan a-han)
  (dprintf "   TEST 1: removing 'cad' from 'a'\n")
  (bt:rem! a-han "cad")
  (scanhan a-han)
  (close-bt! a-han))

;;; THIS IS TEST #2 FOR BLOCK SPLITTING STUFF
;;; this test fills a block completely then adds a new data element and value
;;; to the block to make it split
(define (test2)
  (define b-han (create-bt current-seg IND-TYP 0))
  (bt:put! b-han "foo" "bar")
  (bt:put! b-han "foz" "oof")
  (bt:put! b-han "fonz" "zonf")
  (bt:put! b-han "foo" "raboof")
  (bt:put! b-han "food" "thought")
  (scanhan b-han)
  (bt:rem! b-han "foo")
  (bt:rem! b-han "fonz")
  (scanhan b-han)
  (dprintf "   TEST 2: creating a full block named 'b'\n")
  (bt:put! b-han "foo" "1234567")
  (bt:put! b-han "foz" "12345678901")
  (bt:put! b-han "fonz" "123456789")
  (bt:put! b-han "foo" "1234567890")
  (bt:put! b-han "food" "1234567890123456")
  (bt:put! b-han "test" "123456789012345678901234")
  (dprintf "   TEST 2: doing a PUT to fill the block\n")
  (bt:put! b-han "a" "12345678")
  (scanhan b-han)
  (close-bt! b-han))

;;; THIS IS TEST #3 FOR BLOCK SPLITTING STUFF
;;; this test fills a block , then it tries to make the block split by making
;;; the first data element 1 char bigger by replacement
(define (test3)
  (define c-han (create-bt current-seg IND-TYP 0))
  (bt:put! c-han "foo" "1234567")
  (bt:put! c-han "foz" "12345678901")
  (bt:put! c-han "fonz" "123456789")
  (bt:put! c-han "foo" "1234567890")
  (bt:put! c-han "food" "1234567890123456")
  (bt:put! c-han "test" "123456789012345678901234")
  (bt:put! c-han "a" "12345678")
  (scanhan c-han)
  (dprintf "   TEST 3: split block by increasing first element by one char with PUT\n")
  (bt:put! c-han "a" "123456789")
  (scanhan c-han)
  (close-bt! c-han))

;;; THIS IS TEST #4 FOR BLOCK SPLITTING STUFF
;;; this test fills a block , then it tries to make the block split by making
;;; a middle data element 1 char bigger by replacement
(define (test4)
  (define d-han (create-bt current-seg IND-TYP 0))
  (bt:put! d-han "foo" "1234567")
  (bt:put! d-han "foz" "12345678901")
  (bt:put! d-han "fonz" "123456789")
  (bt:put! d-han "foo" "1234567890")
  (bt:put! d-han "food" "1234567890123456")
  (bt:put! d-han "test" "123456789012345678901234")
  (bt:put! d-han "a" "12345678")
  (scanhan d-han)
  (dprintf "   TEST 4: split block by increasing value of a middle element by one with PUT\n")
  (bt:put! d-han "foz" "123456789012")
  (scanhan d-han)
  (close-bt! d-han))

;;; THIS IS TEST #5 FOR BLOCK SPLITTING STUFF
;;; this test fills a block , then it tries to make the block split by making
;;; the last data element 1 char bigger by replacement
(define (test5)
  (define e-han (create-bt current-seg IND-TYP 0))
  (bt:put! e-han "foo" "1234567")
  (bt:put! e-han "foz" "12345678901")
  (bt:put! e-han "fonz" "123456789")
  (bt:put! e-han "foo" "1234567890")
  (bt:put! e-han "food" "1234567890123456")
  (bt:put! e-han "test" "123456789012345678901234")
  (bt:put! e-han "a" "12345678")
  (scanhan e-han)
  (dprintf "   TEST 5: split block by increasing value of the last data element by one with PUT\n")
  (bt:put! e-han "test" "12345678901234567890123456")
  (scanhan e-han)
  (close-bt! e-han))

;;; THIS IS TEST #6 FOR BLOCK SPLITTING STUFF
;;; this test fills a block , then it deletes the first element. It then
;;; tries to make the block split by reentering the first data element
;;; with 1 more char than the original
(define (test6)
  (define f-han (create-bt current-seg IND-TYP 0))
  (bt:put! f-han "foo" "1234567")
  (bt:put! f-han "foz" "12345678901")
  (bt:put! f-han "fonz" "123456789")
  (bt:put! f-han "foo" "1234567890")
  (bt:put! f-han "food" "1234567890123456")
  (bt:put! f-han "test" "123456789012345678901234")
  (bt:put! f-han "a" "12345678")
  (scanhan f-han)
  (dprintf "   TEST 6: split block by deleting the first data element with REM\n")
  (dprintf "   TEST 6: then reentering the first data element with 1 more char\n")
  (bt:rem! f-han "a")
  (dprintf "   TEST 6: doing a PUT\n")
  (bt:put! f-han "a" "123456789")
  (scanhan f-han)
  (close-bt! f-han))

;;; THIS IS TEST #7 FOR BLOCK SPLITTING STUFF
;;; this test fills a block , then it deletes a middle element. It then
;;; tries to make the block split by reentering a middle element with 1
;;; more char than the original
(define (test7)
  (define g-han (create-bt current-seg IND-TYP 0))
  (bt:put! g-han "foo" "1234567")
  (bt:put! g-han "foz" "12345678901")
  (bt:put! g-han "fonz" "123456789")
  (bt:put! g-han "foo" "1234567890")
  (bt:put! g-han "food" "1234567890123456")
  (bt:put! g-han "test" "123456789012345678901234")
  (bt:put! g-han "a" "12345678")
  (scanhan g-han)
  (dprintf "   TEST 7: split block by deleting a middle data element\n")
  (dprintf "   TEST 7: then reentering a middle data element with 1 more char\n")
  (dprintf "   TEST 7: than the original value.\n")
  (bt:rem! g-han "foo")
  (dprintf "   TEST 7: doing a PUT\n")
  (bt:put! g-han "foo" "12345678901")
  (scanhan g-han)
  (close-bt! g-han))

;;; THIS IS TEST #8 FOR BLOCK SPLITTING STUFF
;;; this test fills a block , then it deletes the last element. It then
;;; tries to make the block split by reentering the last element with 1
;;; more char than the original
(define (test8)
  (define h-han (create-bt current-seg IND-TYP 0))
  (bt:put! h-han "foo" "1234567")
  (bt:put! h-han "foz" "12345678901")
  (bt:put! h-han "fonz" "123456789")
  (bt:put! h-han "foo" "1234567890")
  (bt:put! h-han "food" "1234567890123456")
  (bt:put! h-han "test" "123456789012345678901234")
  (bt:put! h-han "a" "12345678")
  (scanhan h-han)
  (dprintf "   TEST 8: split block by deleting the last data element\n")
  (dprintf "   TEST 8: then reentering the last data element with 1 more char\n")
  (dprintf "   TEST 8: than the original value.  Block name is 'h'\n")
  (bt:rem! h-han "test")
  (dprintf "   TEST 8: doing a PUT\n")
  (bt:put! h-han "test" "12345678901234567890123456")
  (scanhan h-han)
  (close-bt! h-han))

;;; THIS IS TEST #9 FOR BLOCK SPLITTING STUFF
;;; this test fills a block completely then adds a new data element and value
;;; to the block to make it split
(define (test9)
  (define i-han (create-bt current-seg IND-TYP 0))
  (bt:put! i-han "foz" "12345678901")
  (bt:put! i-han "fonz" "123456789")
  (bt:put! i-han "foo" "1234567890")
  (bt:put! i-han "food" "1234567890123456")
  (bt:put! i-han "test" "123456789012345678901234")
  (bt:put! i-han "a" "12345678")
  (dprintf "   TEST 9: The block 'i' is full. Now we are adding a new index to the begining\n")
  (dprintf "   TEST 9: of the block with a value that should make the block split\n")
  (bt:put! i-han "1" "123456789")
  (scanhan i-han)
  (close-bt! i-han))

;;; THIS IS TEST #10 FOR BLOCK SPLITTING STUFF
;;; this test fills a block completely then adds a new data element and value
;;; to the block to make it split
(define (test10)
  (define j-han (create-bt current-seg IND-TYP 0))
  (bt:put! j-han "foz" "12345678901")
  (bt:put! j-han "fonz" "123456789")
  (bt:put! j-han "foo" "1234567890")
  (bt:put! j-han "food" "1234567890123456")
  (bt:put! j-han "test" "123456789012345678901234")
  (bt:put! j-han "a" "12345678")
  (dprintf "   TEST 10: The block 'j' is full. Now we are adding a new index to the middle\n")
  (dprintf "   TEST 10: of the block with a value that should make the block split\n")
  (bt:put! j-han "fooa" "123456789")
  (scanhan j-han)
  (close-bt! j-han))

;;; THIS IS TEST #11 FOR BLOCK SPLITTING STUFF
;;; this test fills a block completely then adds a new data element and value
;;; to the block to make it split
(define (test11)
  (define k-han (create-bt current-seg IND-TYP 0))
  (bt:put! k-han "foz" "12345678901")
  (bt:put! k-han "fonz" "123456789")
  (bt:put! k-han "foo" "1234567890")
  (bt:put! k-han "food" "1234567890123456")
  (bt:put! k-han "test" "123456789012345678901234")
  (bt:put! k-han "a" "12345678")
  (dprintf "   TEST 11: The block 'k' is full. Now we are adding a new index to the end\n")
  (dprintf "   TEST 11: of the block with a value that should make the block split\n")
  (bt:put! k-han "zzz" "123456789")
  (scanhan k-han)
  (close-bt! k-han))

;;; THIS IS TEST #12 FOR BLOCK SPLITTING STUFF
;;; this test fills a block completely then adds a new data element and value
;;; to the block to make it split
(define (test12)
  (define k-han (create-bt current-seg IND-TYP 0))
  (bt:put! k-han "132" "12345678901")
  (bt:put! k-han "1233" "1234567890")
  (bt:put! k-han "26" "1234567890123456")
  (bt:put! k-han "275" "123456789012345678901234")
  (bt:put! k-han "84" "12345678")
  (dprintf "   TEST 12: The block 'k' is full. Now we are adding a new index pastp after b-pos\n")
  (dprintf "   TEST 12: of the block with a value that should make the block split\n")
  (bt:put! k-han "82" "123456789")
  (scanhan k-han)
  (close-bt! k-han))

(define (test13)
  (define k-han (create-bt current-seg IND-TYP 0))
  (bt:put! k-han "0132" "12345678901")
  (bt:put! k-han "01233" "1234567890")
  (bt:put! k-han "026" "1234567890123456")
  (bt:put! k-han "0275" "123456789012345678901234")
  (bt:put! k-han "084" "12345678")
  (dprintf "   TEST 13: The block 'k' is full. Now we are adding a new index pastp after b-pos\n")
  (dprintf "   TEST 13: of the block with a value that should make the block split\n")
  (bt:put! k-han "082" "123456789")
  (scanhan k-han)
  (close-bt! k-han))

(define (test14)
  (define k-han (create-bt current-seg IND-TYP 0))
  (bt:put! k-han "0132" "12345678901")
  (bt:put! k-han "01233" "1234567890")
  (bt:put! k-han "026" "1234567890123456")
  (bt:put! k-han "04" "123456789012345678901234")
  (bt:put! k-han "041" "1234567890")
  (dprintf "   TEST 14: The block 'k' is full. Now we are adding a new index pastp after b-pos\n")
  (dprintf "   TEST 14: of the block with a value that should make the block split\n")
  (tscan-blk (HAN:id k-han))
  (bt:put! k-han "040" "123456789")
  (tscan-blk (HAN:id k-han))
  (close-bt! k-han))

(define (test-128)
  (test1)
  (test2)
  (test3)
  (test4)
  (test5)
  (test6)
  (test7)
  (test8)
  (test9)
  (test10)
  (test11)
  (test12)
  (test13))

(define (main)
  (init-wb 75 150 2048)
  (dprintf "make-seg\n")
  (set! current-seg (make-seg "z" 128))
  ;;(dprintf "close-seg\n")
  ;;(close-seg current-seg #t)
  ;;(dprintf "open-seg\n")
  ;;(set! current-seg (open-seg "z" #t))
  (dprintf "create-bt\n")
  (set! current-bt (create-bt current-seg IND-TYP 0))
  (dprintf "test0!\n")
  (test0! current-bt)
  (dprintf "test!\n")
  (test-128)
  (dprintf "add! 10\n")
  (add! 10)
  ;;(check-access!)
  (close-seg current-seg #f)
  (set! current-seg (open-seg "z" #t))
  (set! current-bt (create-bt current-seg IND-TYP 0))
  (scanhan current-bt)
  (close-seg current-seg #f)
  0)

(define (dbview filename)
  (set! current-seg (open-seg filename #f))
  ;;(set! current-bt (open-db current-seg "test")) (scanhan current-bt)
  (do ((blknum (read) (read)))
      ((eof-object? blknum) (close-seg current-seg #f))
    (tscan-blk blknum)))

(define (rmain . args)
  (dprintf "make-seg\n")
  (set! current-seg (make-seg "z" 128))
  ;;(dprintf "close-seg\n")
  ;;(close-seg current-seg #t)
  ;;(dprintf "open-seg\n")
  ;;(set! current-seg (open-seg "z" #t))
  (dprintf "create-bt\n")
  (set! current-bt (create-db current-seg IND-TYP "test"))
  (dprintf "tscan \n")
  (tscan)
  (dprintf "test0!\n")
  (test0! current-bt)
  ;;  (dprintf "test!\n")
  ;;  (test-128)
  ;; test for INSERT bug!!
  (tscan)
  (cond ((> (length args) 0)
	 (dprintf "TESTING for INSERT BUG\n")
	 (bt:put! current-bt "xxx2" "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")
	 (tscan)
	 (bt:put! current-bt "xxx3" "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")
	 (tscan)
	 (bt:put! current-bt "xxx1" "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")
	 (tscan)
	 (bt:put! current-bt "xxx0" "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")
	 (tscan)
	 ))
  (cond (#f
	 (dprintf "add! 10\n")
	 (add! 10)
	 ;; (check-access!)
	 ))
  (close-seg current-seg #f)
  (set! current-seg (open-seg "z" #t))
  (set! current-bt (open-db current-seg "test"))
  (scanhan current-bt)
  (close-seg current-seg #f)
  (set! current-seg #f)
  (final-wb)
  0)

;; Aubrey -- I expect this routine to leave the file OPEN --roy.
(define (qmain)
  (dprintf "open-seg\n")
  (set! current-seg (open-seg "z" #f))	;read only
  (set! current-bt (let ((han (open-db current-seg "test")))
		     (cond ((not han)
			    (dprintf "`test' created\n")
			    (create-db current-seg IND-TYP "test"))
			   (else han))))
  (if current-bt
      (scanhan current-bt)
      (dprintf "db `test' not found\n")))

;;;; The rest of this will run only in scheme.

;; call (TSCAN-FULL root-seg root-blk
;;              [#of-sublevels 1] [#of-blks-to-chain-fwd 99] [mode 2]
;;  (plus internal args:)
;;              [indent-string ''] [split-key #f] [skey-len 0]
;;              [parent -1] [expected-level -1]
;; MODE: 0=print errors only; 1=errors+warnings; 2=1+block contents
;;
;; call (TSTATS) for just error check and tree stats
;;
;; BUG: subblocks missing ptr from parent wont show up! (what a pain)
;; BUG: especially on the right fringe of the tree!
;; BUG: doesnt yet recurse through directories
;;
;; Instrumented to test five things:
;;  1. that keys are in order at each level;
;;  2. that keys are carried over correctly between levels;
;;  3. to flag blocks missing their parent pointer (not an error, really)
;;  4+5. check consistency of ROOT PTR and TREE TYPE
;;
;; Instrumented to collect statistics:
;;  A. Blocks used (per level; overall)
;;  B. Branching factor (per level; overall)
;;  C. Block utilization (per level; overall)
;;
(define (tscan-full seg blk-num . args)
  (let* ((nlevels (if (> (length args) 0) (car args) 1))
	 (nblks (if (> (length args) 1) (cadr args) 99))
	 (mode (if (> (length args) 2) (caddr args) 2))
	 )
    (set! root-blk 0)
    (set! root-type #f)
    (set! tscan-errs 0)
    (set! tscan-warns 0)
    (do ((i 0 (+ i 1)))			; init things for checks
	((>= i 20))
      (vector-set! tscan-keycts i 0)
      (vector-set! tscan-blkcts i 0)
      (vector-set! tscan-bytects i 0)
      (vector-set! tscan-lengths i -1)
      (vector-set! tscan-prevs i -1)
      (vector-set! tscan-expnexts i -1))
    (tscan-internal seg blk-num nlevels nblks mode "" "" 0 -1 -1)
    (dprintf "\n%d ERRORS, %d WARNINGS.\n\n"
	     tscan-errs tscan-warns)
    (dprintf "TREE SHAPE STATISTICS\n")
    (dprintf "LEVEL  BLOCKS    KEYS BRANCHING  BYTES  %%USED \n")
    (dprintf "                 /BLK FACTOR     /BLK   /BLK  \n")
    (let ((tblks 0)
	  (tkeys 0)
	  (tbytes 0)
	  (bpb (- (SEG:BSIZ seg) BLK-DATA-START)))
      (do ((i 0 (+ i 1)))
	  ((>= i 20))
	(let ((blks (vector-ref tscan-blkcts i))
	      (keys (vector-ref tscan-keycts i))
	      (bytes (vector-ref tscan-bytects i)))
	  (set! tblks (+ tblks blks))
	  (set! tkeys (+ tkeys keys))
	  (set! tbytes (+ bytes tbytes))
	  (cond ((> blks 0)
		 (dprintf "%5d %7d %7d %8d%% %6d %6d%%\n"
			  i blks keys (quotient (* keys 100) blks)
			  (quotient (* (quotient bytes blks) bpb) 100)
			  (quotient bytes blks))))))
      (dprintf "TOTAL %7d %7d %8d%% %6d %6d%%\n"
	       tblks tkeys (quotient (* tkeys 100) tblks)
			  (quotient (* (quotient tbytes tblks) bpb) 100)
			  (quotient tbytes tblks))
      (dprintf "\n BLKS-USED=%d BLK-SIZE=%d FILE-SIZE=%d\n"
	       (+ (SEG:USED seg) 1) (SEG:BSIZ seg)
	       (* (+ (SEG:USED seg) 1) (SEG:BSIZ seg)))
      )))

(define (tscan)
  (check-access!)
  (tscan-blk (HAN:ID current-bt)))

(define (tstats)
  (check-access!)
  (tscan-full current-seg (HAN:ID current-bt) 99 99 1))

(define (tscan-blk blk-num)
  (check-access!)
  (tscan-full current-seg blk-num))

(define tmp-str2 (make-bytes 256))
(define tmp-str3 (make-bytes 256))
(define tscan-errs 0)
(define tscan-warns 0)
(define root-blk 0)
(define root-type 0)
(define tscan-keys (make-vector 20))
(define tscan-lengths (make-vector 20))
(define tscan-prevs (make-vector 20))
(define tscan-expnexts (make-vector 20))
(define tscan-keycts (make-vector 20))
(define tscan-blkcts (make-vector 20))
(define tscan-bytects (make-vector 20))

(do ((i 0 (+ i 1)))
    ((>= i 20))
  (vector-set! tscan-keys i (make-bytes 256)))

(define (tscan-internal seg blk-num nlevels nblks mode
			indent skey skey-len parent explevel)
  (let* ((ent (get-ent seg blk-num ACCREAD))
	 (blk (if ent (ENT:BLK ent) #f))
	 (blklev (if ent (- (BLK:LEVEL blk) LEAF)))
	 (tlen (recon-this-key-debug blk (split-key-pos blk)
				     tmp-str2 0 256))
	 (lidx (- (BLK:LEVEL blk) LEAF))
	 (last-key-str (vector-ref tscan-keys lidx))
	 (last-len (vector-ref tscan-lengths lidx)))
    (cond
     (ent
      (cond ((= parent -1)
	     (set! root-blk (BLK:TOP-ID blk))
	     (set! root-type (BLK:TYP blk))))
      (vector-set! tscan-blkcts blklev
		   (+ (vector-ref tscan-blkcts blklev) 1))
      (if (> mode 1)
	  (dprintf "%s scanning blk %ld:%ld top= %d next= %d len= %d room= %d level= %d type= %c end= %d\n"
		   indent (SEG:ID (ENT:SEG ent)) (BLK:ID blk)
		   (BLK:TOP-ID blk) (BLK:NXT-ID blk)
		   (- (BLK:END blk) BLK-DATA-START)
		   (- (SEG:BSIZ current-seg) (BLK:END blk))
		   (BLK:LEVEL blk) (integer->char (BLK:TYP blk))
		   (BLK:END blk)))
					; test 4+5: root and tree type
      (cond ((not (= root-blk (BLK:TOP-ID blk)))
	     (set! tscan-errs (+ tscan-errs 1))
	     (edprintf "Block %ld:%ld in tree %d thinks its in tree %d\n"
		       (SEG:ID (ENT:SEG ent)) (ENT:ID ent)
		       root-blk (BLK:TOP-ID blk))))
      (cond ((not (= root-type (BLK:TYP blk)))
	     (set! tscan-errs (+ tscan-errs 1))
	     (edprintf "Block %ld:%ld in tree type %c thinks its of type %c\n"
		       (SEG:ID (ENT:SEG ent)) (ENT:ID ent)
		       root-type (integer->char (BLK:TYP blk)))))
					; test 2:split key match
      (cond ((and (> parent -1) (not (str-eql? tmp-str2 0 tlen skey 0 skey-len)))
	     (set! tscan-errs (+ tscan-errs 1))
	     (let ((error? (str-gtr? tmp-str2 0 tlen skey 0 skey-len)))
	       (cond (error? (dprintf "ERROR split key mismatch:\n")
			     (set! tscan-errs (+ tscan-errs 1)))
		     (else (set! tscan-warns (+ tscan-warns 1))
			   (if (> mode 0)
			       (wdprintf "split key mismatch:\n"))))
	       (cond ((or error? (> mode 0))
		      (dprintf "  --Block %ld:%ld has key %.*s (%d)\n"
			       (SEG:ID seg) blk-num tlen tmp-str2 tlen)
		      (dprintf "  --while parent %ld:%ld thinks key is %.*s (%d)\n"
			       (SEG:ID seg) parent skey-len skey skey-len))))))
					; test 3: missing parent ptrs
      (let ((expnext (vector-ref tscan-expnexts lidx))
	    (bt:prev (vector-ref tscan-prevs lidx)))
	(cond ((and (> expnext -1) (not (= expnext blk-num)))
	       (set! warns-errs (+ tscan-warns 1))
	       (cond ((> mode 0)
		      (wdprintf "parent ptr missing for blk %ld:%ld.\n"
				(SEG:ID seg) expnext)
		      (dprintf " --current blk=%d, parent=%d, last=%d, exp=%d.\n"
			       blk-num parent prev expnext)
		      )))))
      (cond ((and (> explevel -1) (not (= explevel (BLK:LEVEL blk))))
	     (set! tscan-errs (+ tscan-errs 1))
	     (edprintf "blk %ld:%ld at level %d is child of blk %ld:%ld at level %d\n"
		       (SEG:ID seg) blk-num (blk:level blk) (SEG:ID seg) parent (+ 1 explevel))))
      (vector-set! tscan-expnexts lidx (BLK:NXT-ID blk))
      (vector-set! tscan-prevs lidx blk-num)
					; scan block contents
      (do ((first-key #t #f)
	   (b-pos BLK-DATA-START)
	   (pos 0) (count 0 (+ count 1))
	   (key-str tmp-str2) (k-len 0))
	  ((>= b-pos (BLK:END blk)))
	(set! pos b-pos)
	(set! k-len (recon-this-key-debug blk pos key-str 0 256))
	(set! b-pos (next-field blk (+ 1 b-pos)))
					; test #1: key order
	(cond ((and (> last-len -1)
		    (if first-key
			(str-gtr? last-key-str 0 last-len key-str 0 k-len)
			(not (str-gtr? key-str 0 k-len last-key-str 0 last-len))))
	       (set! tscan-errs (+ tscan-errs 1))
	       (dprintf "KEY ORDER ERROR at blk %ld:%ld pos %d\n"
			(SEG:ID seg) blk-num pos)
	       (dprintf " --key=%.*s last key=%.*s.\n"
			k-len key-str last-len last-key-str)))
	(subbytes-move! key-str 0 k-len last-key-str 0)
	(vector-set! tscan-lengths lidx k-len)
	(set! last-len k-len)
					; print block contents
	(cond ((< b-pos (BLK:END blk))
	       (cond ((LEAF? blk)
		      (if (> mode 1)
			  (dprintf  "%s at %d key= %s value= %s\n"
				    indent pos
				    (subbytes key-str 0 k-len)
				    (subbytes blk (+ 1 b-pos)
					       (+ 1 b-pos (field-len blk b-pos))))))
		     (else
		      (if (> mode 1)
			  (dprintf "%s at %d key= %s ptr= %d\n"
				   indent pos (subbytes key-str 0 k-len)
				   (str2long blk (+ 1 b-pos))))
		      (if (> nlevels 0)
			  (tscan-internal seg (str2long blk (+ 1 b-pos))
					  (- nlevels 1) 0 mode
					  (string-append indent "   ")
					  key-str k-len blk-num
					  (- (BLK:LEVEL blk) 1)))))
	       (set! b-pos (next-field blk b-pos)))
	      (else
	       (vector-set! tscan-keycts blklev
			    (+ (vector-ref tscan-keycts blklev) count))
	       (vector-set! tscan-bytects blklev
			    (+ (vector-ref tscan-bytects blklev)
			       (quotient (* 100 (- (BLK:END blk) BLK-DATA-START))
					 (- (SEG:BSIZ seg) BLK-DATA-START))))
	       (if (> mode 1)
		   (dprintf "%s at %d split= %s\n"
			    indent pos (subbytes key-str 0 k-len))))))
      (let ((nxt (BLK:NXT-ID blk)))
	(release-ent! ent ACCREAD)
	(cond ((and (not (zero? nxt))
		    (> nblks 0))
	       (newline)
	       (tscan-internal seg nxt nlevels (- nblks 1) mode indent "" 0 parent explevel))
	      (else #f))))
     (else
      (set! tscan-errs (+ tscan-errs 1))
      (dprintf "%s ERROR: can't access blk %ld:%ld. \n"
	       indent (SEG:ID seg) blk-num)))))

(define (str-eql? a-str a-pos a-len b-str b-pos b-len)
  (and (= a-len b-len)
       (let loop ((i 0) (ap a-pos) (bp b-pos))
	 (cond ((>= i a-len) #t)
	       ((not (char=? (string-ref a-str ap) (string-ref b-str bp))) #f)
	       (else (loop (+ i 1) (+ ap 1) (+ bp 1)))))))


;; temp hack in case rean RECON is traced...

(define (recon-this-key-debug blk pos key-str k-pos k-len)
  (do ((b-pos BLK-DATA-START)
       (k-size 0))
      ((> b-pos pos) (subbytes key-str k-pos (+ k-pos k-size)) k-size)
    (subbytes-move! blk (+ b-pos 2)
		     (+ b-pos 2 (field-len blk (+ 1 b-pos)))
		     key-str
		     (+ k-pos (field-len blk b-pos)))
    (set! k-size (+ (field-len blk b-pos) (field-len blk (+ 1 b-pos))))
    (if (>= k-size k-len) not-enough-room)
    (set! b-pos (next-field blk (+ 1 b-pos)))
    (if (< b-pos (blk:end blk)) (set! b-pos (next-field blk b-pos)))))

;; (close-seg current-seg #f) (load "all") (load "main") (rmain)(qmain)(cstats)(add! 50) (add! 300 "a b c d e f g h i j")  (Cstats)

(define (prof)
  (define (start) 0)
  (set! current-seg (make-seg "z" 2048))
  ;;(close-seg current-seg #t)
  ;;(set! current-seg (open-seg "z" #t))
  (set! current-bt (create-db current-seg IND-TYP "test"))
  (clear-stats)
  (set! start (get-internal-run-time))
  (add! 100 0 1)
  (add! 100 990 -10)
  (add! 100 1000 100)
  (display "Adds took ")
  (display (quotient (* 1000 (- (get-internal-run-time) start)) internal-time-units-per-second))
  (display " Msec")
  (newline)
  (cstats)

  (set! start (get-internal-run-time))
  (scanhan current-bt)
  (display "forward scan took ")
  (display (quotient (* 1000 (- (get-internal-run-time) start)) internal-time-units-per-second))
  (display " Msec")
  (newline)
  (cstats)

  (set! start (get-internal-run-time))
  (scanb current-bt)
  (display "backward scan took ")
  (display (quotient (* 1000 (- (get-internal-run-time) start)) internal-time-units-per-second))
  (display " Msec")
  (newline)
  (cstats)

  (set! start (get-internal-run-time))
  (remove! 100 0 1)
  (remove! 100 990 -10)
  (remove! 100 1000 100)
  (display "Removes took ")
  (display (quotient (* 1000 (- (get-internal-run-time) start)) internal-time-units-per-second))
  (display " Msec")
  (newline)
  (cstats)

  (close-seg current-seg #f)
  0)

(define (radd! span)
  (require 'random)
  (dprintf "adding %d records with random keys\n" span)
  (do ((i add-key-num (+ 1 i)) (r (random (* 10 span)) (random (* 10 span))))
      ((>= i (+ add-key-num span)))
    (bt:put! current-bt (number->string r) (string-append (number->string i) "number written in random test")))
  (set! add-key-num (+ add-key-num span)))

(define (radd! span key)
  (require 'random)
  (dprintf "adding %d records with random keys\n" span)
  (do ((i add-key-num (+ 1 i)) (r (random (* 10 span)) (random (* 10 span))))
      ((>= i (+ add-key-num span)))
    (bt:put! current-bt (number->string r) key))
  (set! add-key-num (+ add-key-num span)))

(define (check)
  (system "./wbcheck z"))
