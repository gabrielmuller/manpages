;; "wbsys.scm" WB-tree File Based Associative String Data Base System.
;; Copyright (C) 1991, 1992, 1993, 2000, 2003, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
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

(slib:load-source (in-vicinity (program-vicinity) "schleprt.scm"))
(defmacro:load (in-vicinity (program-vicinity) "wbdefs.scm"))

;;; Analogize byte support from R2RS
(define subbytes-move-left! substring-move-left!)
(define subbytes-move-right! substring-move-right!)
(define (subbytes-move! src start end dst dstart)
  (if (eq? src dst)
      (edprintf "%s: called with same string\n" 'SUBBYTES-MOVE!))
  (subbytes-move-left! src start end dst dstart))

(define lck-list '())
(define (make-lck name)
  (let ((lk (make-arbiter name)))
    (set! lck-list (cons lk lck-list))
    lk))
(define try-lck? try-arbiter)
(define (lck! lck)
  (or (try-arbiter lck) (edprintf "spinning %d\n" lck)))
(define (unlck! lck)
  (or (release-arbiter lck) (edprintf "unlcking %d\n" lck)))
;;; this fixes lcks - testing only
(define (check-lcks)
  (for-each
   (lambda (l) (and (release-arbiter l) (edprintf "%d left lcked\n" l)))
   lck-list))

;;; The handle is a HAN
;;; [HAN:SEG, HAN:ID, HAN:TYP, HAN:LAST, HAN:WriteControlBits]
;;; HAN:ID is always the root of a B-tree.

(define (MAKE-HAND)
  (vector #f #f #f #f 0 #f))

(define HAN-ID-POS 0)
(define HAN-SEG-POS 1)
(define HAN-TYP-POS 2)
(define HAN-LAST-POS 3)
(define HAN-WCB-POS 4)
(define HAN-SPARE-POS 5)

(define (HAN:ID han) (vector-ref han HAN-ID-POS))
(define (HAN:SEG han) (vector-ref han HAN-SEG-POS))
(define (HAN:TYP han) (vector-ref han HAN-TYP-POS))
(define (HAN:LAST han) (vector-ref han HAN-LAST-POS))
(define (HAN:WCB han) (vector-ref han HAN-WCB-POS))

(define (HAN:SET-NUM! han num) (vector-set! han HAN-ID-POS num))
(define (HAN:SET-SEG! han seg) (vector-set! han HAN-SEG-POS seg))
(define (HAN:SET-TYP! han dir) (vector-set! han HAN-TYP-POS dir))
(define (HAN:SET-LAST! han num) (vector-set! han HAN-LAST-POS num))
(define (HAN:SET-WCB! han wcb) (vector-set! han HAN-WCB-POS wcb))

;;; SEGD is a segment descriptor:
;;; [SEGD-PORT,			; file handle for segment
;;;  SEGD-BSIZ,			; block-size
;;;  SEGD-USED,			; number of blocks used (file-size/SEGD-BSIZ)
;;;  SEGD-STR,			; string name of file
;;;  SEGD-RT-HAN,		; handle for 0 block
;;;  SEGD-FL-HAN,		; handle for free-list block (2)
;;;  SEGD-LCK,			; lock for FLC and superblock.
;;;  SEGD-FCK,			; lock for the free-list.
;;;  SEGD-FLC-LEN,		; number of available blocks in free-list-cache
					;-1 means to read in "FLC" image.
					;-2 means read only.
;;;  SEGD-FLC			; free-list-cache
;;;  SEGD-PRV			; previous seg in chain
;;;  SEGD-NXT]			; next seg in chain

(define (new-segd idx)
  (vector #f				;PORT
	  0				;BSIZ
	  0				;USED
	  #f				;STR
	  (MAKE-HAND)			;RT-HAN
	  (MAKE-HAND)			;FL-HAN
	  (make-lck (- -0 (* 2 idx)))	;LCK
	  (make-lck (- -1 (* 2 idx)))	;FCK
	  0				;FLC-LEN
	  #f				;FLC
	  #f				;PRV
	  #f				;NXT
	  idx				;ID
	  ))

(define SEGD-PORT-POS 0)
(define SEGD-BSIZ-POS 1)
(define SEGD-USED-POS 2)
(define SEGD-STR-POS 3)
(define SEGD-RT-HAN-POS 4)
(define SEGD-FL-HAN-POS 5)
(define SEGD-LCK-POS 6)
(define SEGD-FCK-POS 7)
(define SEGD-FLC-LEN-POS 8)
(define SEGD-FLC-POS 9)
(define SEGD-PRV-POS 10)
(define SEGD-NXT-POS 11)
(define SEGD-ID-POS 12)

(define (SEG:PORT seg) (vector-ref seg SEGD-PORT-POS))
(define (SEG:BSIZ seg) (vector-ref seg SEGD-BSIZ-POS))
(define (SEG:USED seg) (vector-ref seg SEGD-USED-POS))
(define (SEG:STR seg) (vector-ref seg SEGD-STR-POS))
(define (SEG:RT-HAN seg) (vector-ref seg SEGD-RT-HAN-POS))
(define (SEG:FL-HAN seg) (vector-ref seg SEGD-FL-HAN-POS))
(define (SEG:LCK seg) (vector-ref seg SEGD-LCK-POS))
(define (SEG:FCK seg) (vector-ref seg SEGD-FCK-POS))
(define (SEG:FLC-LEN seg) (vector-ref seg SEGD-FLC-LEN-POS))
(define (SEG:FLC seg) (vector-ref seg SEGD-FLC-POS))
(define (SEG:PRV seg) (vector-ref seg SEGD-PRV-POS))
(define (SEG:NXT seg) (vector-ref seg SEGD-NXT-POS))
(define (SEG:ID seg) (if seg (vector-ref seg SEGD-ID-POS) -1))

(define (SEG:MUTABLE? seg) (not (eqv? -2 (SEG:FLC-LEN seg))))

(define (SEG:SET-PORT! seg port) (vector-set! seg SEGD-PORT-POS port))
(define (SEG:SET-BSIZ! seg bsiz) (vector-set! seg SEGD-BSIZ-POS bsiz))
(define (SEG:SET-USED! seg used) (vector-set! seg SEGD-USED-POS used))
(define (SEG:SET-STR! seg str) (vector-set! seg SEGD-STR-POS str))
(define (SEG:SET-FLC-LEN! seg flc-len) (vector-set! seg SEGD-FLC-LEN-POS flc-len))
(define (SEG:SET-FLC! seg flc) (vector-set! seg SEGD-FLC-POS flc))
(define (SEG:SET-PRV! seg prev-seg) (vector-set! seg SEGD-PRV-POS prev-seg))
(define (SEG:SET-NXT! seg next-seg) (vector-set! seg SEGD-NXT-POS next-seg))

;;; The hash table element is an ENT:
;;; [ENT-TAG			; The number of this entry (diagnostic).
;;;  ENT-NEXT			; The next entry in this bucket (hash table element)
;;;  ENT-SEG			; segment number for this entry
;;;  ENT-ID			; block number for this entry
;;;  ENT-BLK			; string of length (SEG:BSIZ (ENT:SEG ent))
;;;  ENT-AGE			; aging count.  Gets bigger as time goes on
;;;  ENT-DTY			; buffer has been modified
;;;  ENT-PUS			; parent update state 1, 0 , -1 , -2
;;;  ENT-ACC			; either ACCREAD, ACCWRITE, ACCPEND, or #f.
;;;  ENT-REF]			; count of outstanding pointers to this entry and block
(define (ENT:MAKE-ENT tag)
  (vector tag #f -1 -1 (make-bytes blk-size (char->integer #\~)) 0 #f 0 ACCNONE 0))

(define ENT-TAG-POS 0)
(define ENT-NEXT-POS 1)
(define ENT-SEG-POS 2)
(define ENT-ID-POS 3) ; blk #
(define ENT-BLK-POS 4)
(define ENT-AGE-POS 5) ; grows with age, starts at 0
(define ENT-DTY-POS 6) ; needs writing out if not #f
(define ENT-PUS-POS 7) ; parent uptdate state.
(define ENT-ACC-POS 8) ; ACC-READ, ACC-WRITE, or ACC-PEND (reading)
(define ENT-REF-POS 9) ; ref count for NAME access

(define (ENT:TAG ent) (vector-ref ent ENT-TAG-POS))
(define (ENT:NEXT ent) (vector-ref ent ENT-NEXT-POS))
(define (ENT:SEG ent) (vector-ref ent ENT-SEG-POS))
(define (ENT:ID ent) (vector-ref ent ENT-ID-POS))
(define (ENT:BLK ent) (vector-ref ent ENT-BLK-POS))
(define (ENT:AGE ent) (vector-ref ent ENT-AGE-POS))
(define (ENT:DTY? ent) (vector-ref ent ENT-DTY-POS))
(define (ENT:ACC ent) (vector-ref ent ENT-ACC-POS))
(define (ENT:PUS ent) (vector-ref ent ENT-PUS-POS))
(define (ENT:REF ent) (vector-ref ent ENT-REF-POS))

(define (ENT:SET-TAG! ent tag) (vector-set! ent ENT-TAG-POS tag))
(define (ENT:SET-NEXT! ent next) (vector-set! ent ENT-NEXT-POS next))
(define (ENT:SET-SEG! ent seg) (vector-set! ent ENT-SEG-POS seg))
(define (ENT:SET-ID! ent num) (vector-set! ent ENT-ID-POS num))
(define (ENT:SET-AGE! ent age) (vector-set! ent ENT-AGE-POS age))
(define (ENT:SET-DTY! ent dty) (vector-set! ent ENT-DTY-POS dty))
(define (ENT:SET-PUS! ent pus) (vector-set! ent ENT-PUS-POS pus))
(define (ENT:SET-ACC! ent acc) (vector-set! ent ENT-ACC-POS acc))
(define (ENT:SET-REF! ent ref) (vector-set! ent ENT-REF-POS ref))

;;;; BLK parameters

;;; The IDs are 4 byte numbers identifying this block, the root of
;;; this tree, and the next in the chain.
(defconst BLK-ID-POS 0)
(defconst BLK-TOP-ID-POS 4)
(defconst BLK-NXT-ID-POS 8)
(defconst BLK-TIME-POS 12)
;;; blk-end-pos is position (stored in 2 bytes) of first free byte
(defconst BLK-END-POS 16)
(defconst BLK-LEVEL-POS 18)
(defconst BLK-TYP-POS 19)
(defconst BLK-DATA-START 20)

(define (BLK:ID blk) (str2long blk BLK-ID-POS))
(define (BLK:TOP-ID blk) (str2long blk BLK-TOP-ID-POS))
(define (BLK:NXT-ID blk) (str2long blk BLK-NXT-ID-POS))
(define (BLK:TIME blk) (str2long blk BLK-TIME-POS))
(define (BLK:END blk) (str2short blk BLK-END-POS))
(define (BLK:LEVEL b) (byte-ref b BLK-LEVEL-POS))
(define (BLK:TYP b) (byte-ref b BLK-TYP-POS))
(define (BLK:TYP? b typ) (= (byte-ref b BLK-TYP-POS) typ))

(define (BLK:SET-ID! blk id) (long2str! blk BLK-ID-POS id))
(define (BLK:SET-TOP-ID! blk id) (long2str! blk BLK-TOP-ID-POS id))
(define (BLK:SET-NXT-ID! blk id) (long2str! blk BLK-NXT-ID-POS id))
(define (BLK:SET-TIME! blk tim) (long2str! blk BLK-TIME-POS tim))
(define (BLK:SET-END! blk pos) (short2str! blk BLK-END-POS pos))
(define (BLK:SET-LEVEL! b level) (byte-set! b BLK-LEVEL-POS level))
(define (BLK:SET-TYP! b typ) (byte-set! b BLK-TYP-POS typ))

;;;; Routines for finding the appropriate BLK for an operation.
;;; PACKETs used to return multiple values from chain-find.
;;; and various other operations
(defconst PKT-SIZE 6)

(define (PKT:MATCH-TYPE p) (vector-ref p 0)) ;see below for PASTP, QPASTP,...
(define (PKT:MATCH-POS p) (vector-ref p 1)) ;position of key we (almost) matched.
(define (PKT:KEY-POS p) (vector-ref p 2)) ;number of matching characters
(define (PKT:PREV-MATCH-POS p) (vector-ref p 3)) ;position of PREVIOUS key we (almost) matched.
(define (PKT:BLK-TO-CACHE p) (vector-ref p 4))	;blk number to cache
(define (PKT:SUCCESS-CODE p) (vector-ref p 5)) ;UNUSED

(define (PKT:SET-MATCH-TYPE! p v) (vector-set! p 0 v))
(define (PKT:SET-MATCH-POS! p v) (vector-set! p 1 v))
(define (PKT:SET-KEY-POS! p v) (vector-set! p 2 v))
(define (PKT:SET-PREV-MATCH-POS! p v) (vector-set! p 3 v)) ;position of PREVIOUS key we (almost) matched.
(define (PKT:SET-BLK-TO-CACHE! p v) (vector-set! p 4 v)) ;blk number to cache
(define (PKT:SET-SUCCESS-CODE! p v) (vector-set! p 5 v)) ;UNUSED

(define (PKT:PACK! p type b-pos k-pos p-pos)
  (PKT:SET-MATCH-TYPE! p type)
  (PKT:SET-MATCH-POS! p b-pos)
  (PKT:SET-KEY-POS! p k-pos)
  (PKT:SET-PREV-MATCH-POS! p p-pos))

;;; Aliased function names for SCAN
(define PKT:SKEY-COUNT PKT:MATCH-POS)
(define PKT:SET-SKEY-COUNT! PKT:SET-MATCH-POS!)
(define PKT:SKEY-LEN PKT:KEY-POS)
(define PKT:SET-SKEY-LEN! PKT:SET-KEY-POS!)
