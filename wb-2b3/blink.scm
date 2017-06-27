;; "blink.scm" WB-tree File Based Associative String Data Base System.
;; Copyright (C) 1991, 1992, 1993, 2000, 2003, 2010 Free Software Foundation, Inc.
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

;;; TBD:
;;; allow different size blks for index and leaves.
;;; add multi-record operations

;;; uncomment for newlines at end of blocks:
(provide 'NLEB)

;;; uncomment for extra check in REROOT! and path tracing in BLK:FIND-POS.
;;(provide 'DEBUG)

(pragma.c "#include \"wbsys.h\"")

(java:import "Ents" "Stats" "Wbdefs" "Wbsys" "Blk" "SchlepRT" "Pkt"
	     "Ent" "Seg")

;;; BLK ACCESS AND MODIFICATION ROUTINES

(define (short2str! str pos cint)
  (byte-set! str (+ pos 1) (logand cint #xFF))
  (byte-set! str (+ pos 0) (ash cint -8)))
(define (str2short str pos)
  (logior (byte-ref str (+ pos 1))
	  (ash (byte-ref str pos) 8)))

(define (long2str! str pos clong)
  (byte-set! str (+ pos 3) (logand clong #xFF))
  (byte-set! str (+ pos 2) (logand (ash clong -8) #xFF))
  (byte-set! str (+ pos 1) (logand (ash clong -16) #xFF))
  (byte-set! str (+ pos 0) (ash clong -24)))
(define (str2long str pos)
  (logior (byte-ref str (+ pos 3))
	  (ash (logior (byte-ref str (+ pos 2))
		       (ash (logior (byte-ref str (+ pos 1))
				    (ash (byte-ref str pos)
					 8))
			    8))
	       8)))

(define (set-field blk b-pos val-str f-pos f-len)
  (SET-FIELD-LEN! blk b-pos f-len)
  (subbytes-move! val-str f-pos (+ f-pos f-len) blk (+ 1 b-pos))
  (+ f-len 1 b-pos))

(define LEAF-SPLIT-KEY-STR (bytes #+SCM2JAVA -1 #-SCM2JAVA 255
				  #+SCM2CS #x30 #-SCM2CS LEAF))

(define (init-leaf-blk! nblk bnum typ)
  #+NLEB (byte-set! nblk (- blk-size 1) #x0a) ;#\newline
  (BLK:SET-ID! nblk bnum)
  (BLK:SET-NXT-ID! nblk 0)
  (BLK:SET-TOP-ID! nblk bnum)
  (BLK:SET-TIME! nblk 0)
  (BLK:SET-LEVEL! nblk LEAF)
  (BLK:SET-TYP! nblk typ)
  (SET-FIELD-LEN! nblk BLK-DATA-START 0)
  (set-field nblk (+ BLK-DATA-START 1) LEAF-SPLIT-KEY-STR 0 2)
  (BLK:SET-END! nblk (+ BLK-DATA-START (if (= typ SEQ-TYP) 0 4))))

;;;RBLK= the root block, NBLK= new block to hold root's data, BNUM= its ID

(define (reroot! rblk nblk bnum bsiz)
  (define rpos BLK-DATA-START)
;;;#|s|#  (dprintf "%s: rblk=%ld nblk=%ld bnum=%ld bsiz=%d rpos=%d\n"
;;;#|s|#	   'REROOT (BLK:ID rblk) (BLK:ID nblk) bnum bsiz rpos)
  #+DEBUG
  (cond ((not (= bnum (BLK:ID nblk)))
	 (wdprintf "%s: mismatched blk-num 0x%x 0x%x\n"
		   'reroot! bnum (BLK:ID nblk))
	 (BLK:SET-ID! nblk bnum)))
  (subbytes-move! rblk 4 bsiz nblk 4)	;copy whole block except ID
  (BLK:SET-NXT-ID! rblk 0)		;end of chain
  (BLK:SET-LEVEL! rblk (+ (BLK:LEVEL rblk) 1))
  (SET-FIELD-LEN! rblk rpos 0)
  (set! rpos (set-field rblk (+ rpos 1) LEAF-SPLIT-KEY-STR 0 2))
  (byte-set! rblk (- rpos 1) (- (BLK:LEVEL rblk) 1))
  (set! rpos (set-field rblk rpos nblk 0 4))
  (SET-FIELD-LEN! rblk rpos 1)
  (set! rpos (set-field rblk (+ rpos 1) LEAF-SPLIT-KEY-STR 0 1))
  (byte-set! rblk (- rpos 1) (BLK:LEVEL rblk))
  (BLK:SET-END! rblk rpos))

(define (init-next-blk! blk nblk)
  #+NLEB (byte-set! nblk (- blk-size 1) #x0a) ;#\newline
  ;;  (BLK:SET-ID! nblk bnum)
  (BLK:SET-NXT-ID! nblk (BLK:NXT-ID blk))
  (BLK:SET-TOP-ID! nblk (BLK:TOP-ID blk))
  (BLK:SET-LEVEL! nblk (BLK:LEVEL blk))
  (BLK:SET-TYP! nblk (BLK:TYP blk))
  (BLK:SET-NXT-ID! blk (BLK:ID nblk))
  (SET-FIELD-LEN! nblk BLK-DATA-START 0)
  (set-field nblk (+ BLK-DATA-START 1) NO-byts 0 0)
  (BLK:SET-END! nblk (+ BLK-DATA-START 2)))

(define (split-key-pos blk)
  (define b-end (BLK:END blk))
  (let lp ((b-pos BLK-DATA-START))
    (let ((s-pos (next-field blk (+ 1 b-pos))))
      (cond
       ((= s-pos b-end) b-pos)
       ((< s-pos b-end) (lp (next-cnvpair blk b-pos)))
       (else
	(edprintf "%s: blk past end %ld %d\n"
		  'split-key-pos (BLK:ID blk) s-pos)
	0)))))


;;; Pass in len; -1 to seek END-OF-CHAIN; -2 for START-OF-CHAIN.
;;; If key-str = END-OF-CHAIN, then return PASTEND @ split-pos.
;;; If key-str = START-OF-CHAIN, then return QPASTP @ blk-data-start.
;;; Otherwise, can return any of 5 match conditions.

;;; As we go through blk looking for key, KEY-POS (k-pos) is the
;;; number of characters matching between key and blk.

(define (blk:find-pos blk key-str k-len pkt)
  ;;(vector-fill! pkt 'foo)
  #+DEBUG (dprintf "%s: \"%.*s\" %ld\n" 'blk:find-pos (max 0 k-len) key-str k-len)
  (cond
   ((< k-len 0)
    (if (= k-len END-OF-CHAIN)
	(let ((skpos (split-key-pos blk)))
	  (PKT:PACK! pkt (if (END-OF-CHAIN? blk) QPASTP PASTEND)
		     skpos 0 (blk-prev-key blk skpos)))
	(PKT:PACK! pkt QPASTP BLK-DATA-START 0 0))
    #t)
   (else
    (let ((k-pos 0) (b-end (BLK:END blk)))
      (let chknxt ((b-pos BLK-DATA-START) (p-pos 0)) ;where we are looking in this blk
	(cond
	 ((< (FIELD-LEN blk b-pos) k-pos) ;compress count is less than
	  #+DEBUG (dprintf "****-1\n")
	  (PKT:PACK! pkt QPASTP b-pos k-pos p-pos) #t) ;what already matched.
	 ((> (FIELD-LEN blk b-pos) k-pos) ;matched no more than last time
	  #+DEBUG (dprintf "****-2\n")
	  (let ((s-pos (next-field blk (+ b-pos 1))))
	    (cond
	     ((< s-pos b-end)
	      (chknxt (NEXT-CNVPAIR blk b-pos) b-pos))
	     ((= s-pos b-end)
	      (PKT:PACK! pkt PASTEND b-pos k-pos p-pos) #t)
	     (else (edprintf "%s1: blk past end %ld %d\n"
			     'BLK-FIND-POS (BLK:ID blk) s-pos)
		   #f))))
	 (else
	  (let mchlp ((i (+ b-pos 2)) (f-len (FIELD-LEN blk (+ b-pos 1))))
	    (cond
	     ((>= k-pos k-len)		;end of key
	      #+DEBUG (dprintf "****-3\n")
	      (cond
	       ((> f-len 0)
		(PKT:PACK! pkt PASTP b-pos k-pos p-pos) #t) ;field is longer
	       (else
		(let ((s-pos (next-field blk (+ b-pos 1))))
		  (cond
		   ((< s-pos b-end)
		    (PKT:PACK! pkt MATCH b-pos k-len p-pos) #t)
		   ((= s-pos b-end)
		    (PKT:PACK! pkt MATCHEND b-pos k-pos p-pos) #t) ;reached the end; keys =
		   (else (edprintf "%s2: blk past end %ld %d\n"
				   'BLK-FIND-POS (BLK:ID blk) s-pos)
			 #f))))))
	     ((or (<= f-len 0)		;field was shorter
		  (< (byte-ref blk i) (byte-ref key-str k-pos))) ;key is more
	      #+DEBUG (dprintf "****-4\n")
	      (let ((s-pos (next-field blk (+ b-pos 1))))
		(cond
		 ((< s-pos b-end) (chknxt (NEXT-CNVPAIR blk b-pos) b-pos))
		 ((= s-pos b-end)
		  (PKT:PACK! pkt PASTEND b-pos k-pos p-pos) #t)
		 (else (edprintf "%s3: blk past end %ld %d\n"
				 'BLK-FIND-POS (BLK:ID blk) s-pos)
		       #f))))
	     ((> (byte-ref blk i) (byte-ref key-str k-pos))
	      #+DEBUG (dprintf "****-5\n")
	      (PKT:PACK! pkt (if (> k-pos (FIELD-LEN blk b-pos)) PASTP QPASTP) b-pos k-pos p-pos) #t)
	     (else #+DEBUG (dprintf "****-else\n")
		   (set! k-pos (+ k-pos 1)) ;matched a character
		   (mchlp (+ 1 i) (- f-len 1))))))))))))

;;; Can return QPASTP or PASTP @ any key or MATCH at non-split key.

(define (chain-find ent accmode key-str k-len pkt)
  (define blk (ENT:BLK ent))
  ;;(dprintf "%s: %ld:%ld %d %.*s\n" 'chain-find (SEG:ID (ENT:SEG ent)) (ENT:ID ent) accmode (max 0 k-len) key-str)
  (cond ((not (blk:find-pos blk key-str k-len pkt))
	 (release-ent! ent accmode) #f)
	;;failure case.  BLK-FIND-POS already gave error message
	((not (or (eq? (PKT:MATCH-TYPE pkt) MATCHEND) ;(print 'chain-find: '(PKT:MATCH-TYPE pkt) )
		  (eq? (PKT:MATCH-TYPE pkt) PASTEND)))
	 ent)			    ;If (Q)PASTP or MATCH.  Stop here.
	((END-OF-CHAIN? blk)
	 (edprintf "%s: matched or past end of chain %ld:%ld\n"
		   'CHAIN-FIND (SEG:ID (ENT:SEG ent)) (ENT:ID ent))
	 (PKT:SET-MATCH-TYPE! pkt QPASTP)
	 ent)
	(else
	 (set! chains-to-next (+ 1 chains-to-next))
	 (set! ent (switch-ent ent accmode (BLK:NXT-ID blk) accmode))
	 (and ent (chain-find ent accmode key-str k-len pkt)))))

;;; find-ent is always called with ent = (get-ent <seg> <blk-num> ACCNONE).
;;; TBD - These calls could be colapsed.
;; should be called with LAST-LEVEL=-1

(define (find-ent ent desired-level last-level key-str k-len)
  (and
   ent
   (ents:ent-update-access? ent ACCNONE ACCREAD)
   (let* ((blk (ENT:BLK ent))
	  (blvl (BLK:LEVEL blk)))
     (cond
      ((= blvl desired-level) ent)
      ((< blvl desired-level)
       (edprintf "bad blk level %d (des=%d) in %ld:%ld\n"
		 blvl desired-level (SEG:ID (ENT:SEG ent)) (ENT:ID ent))
       #f)
      ((and (>= last-level 0) (not (= blvl (- last-level 1))))
       (edprintf "bad blk level %d last=%d in %ld:%ld\n"
		 blvl last-level (SEG:ID (ENT:SEG ent)) (ENT:ID ent))
       #f)
      (else
       (let ((pkt (make-vector PKT-SIZE)))
	 (set! ent (chain-find ent ACCREAD key-str k-len pkt))
	 (and
	  ent
	  (let ((pos (next-field blk (+ 1 (PKT:MATCH-POS pkt)))))
	    (set! blk (ENT:BLK ent))
	    (case (PKT:MATCH-TYPE pkt)
	      ((QPASTP PASTP) #f)
	      ((MATCH)
	       (if (= (BLK:END blk) pos)
		   (set! pos (PKT:MATCH-POS pkt))
		   (set! pos (next-field blk pos))))
	      (else (set! pos 0)))
	    (cond
	     ((zero? pos)
	      (edprintf "%s: bad-MATCH-TYPE %d blk %ld:%ld\n" 'FIND-ENT
			(PKT:MATCH-POS pkt) (SEG:ID (ENT:SEG ent)) (ENT:ID ent))
	      #f)
	     (else
	      (set! pos (next-field blk (+ 1 (PKT:MATCH-POS pkt))))
	      (find-ent
	       (switch-ent ent
			   ACCREAD
			   (if (= (BLK:END blk) pos)
			       (if (END-OF-CHAIN? blk)
				   (str2long blk (+ -6 pos))
				   (BLK:NXT-ID blk))
			       (str2long blk (+ 1 pos)))
			   ACCNONE)
	       desired-level
	       (if (and (= (BLK:END blk) pos) (not (END-OF-CHAIN? blk)))
		   (+ (BLK:LEVEL blk) 1) (BLK:LEVEL blk))
	       key-str k-len)))))))))))

(define (blk-prev-key blk pos)
  (do ((b-pos BLK-DATA-START (NEXT-CNVPAIR blk b-pos))
       (p-pos 0 b-pos))
      ((>= b-pos pos)
       (cond ((> b-pos pos)
	      (edprintf "%s: blk past end %ld %d\n"
			'BLK-PREV-KEY (BLK:ID blk) p-pos)
	      0)
	     (else
	      ;;(dprintf "%s %d %d returns %d\n" 'BLK-PREV-KEY (BLK:ID blk) pos p-pos)
	      p-pos)))))

;;;; DATA BASE OPERATIONS

(define (get-this-val blk b-pos ans-str)
  (set! b-pos (next-field blk (+ b-pos 1)))
  (let ((alen (FIELD-LEN blk b-pos)))
    (subbytes-move! blk (+ b-pos 1) (+ b-pos 1 alen) ans-str 0)
    alen))

(define (get-this-key blk b-pos key-str ans-str ent k-len pkt)
  (let ((b-end (BLK:END blk))
	(s-pos (next-field blk (+ b-pos 1))))
    (cond ((< s-pos b-end)
	   (let* ((f-pos (FIELD-LEN blk b-pos))
		  (f-siz (FIELD-LEN blk (+ b-pos 1)))
		  (alen (+ f-pos f-siz)))
	     (if (not (eq? key-str ans-str))
		 (subbytes-move! key-str 0 f-pos ans-str 0))
	     (subbytes-move! blk (+ b-pos 2) (+ b-pos 2 f-siz) ans-str f-pos)
	     (release-ent! ent ACCREAD)
	     alen))
	  ((not (= s-pos b-end))
	   (edprintf "%s: blk past end %ld %d\n"
		     'CHAIN-NEXT (BLK:ID blk) s-pos)
	   (release-ent! ent ACCREAD)
	   STRANGERR)
	  ((END-OF-CHAIN? blk)
	   (release-ent! ent ACCREAD) NOTPRES)
	  (else
	   (set! ent (switch-ent ent ACCREAD (BLK:NXT-ID blk) ACCREAD))
	   (if ent (set! ent (chain-find ent ACCREAD key-str k-len pkt)))
	   (if ent (chain-next ent key-str k-len ans-str pkt)
	       UNKERR)))))	 ; TBDFIXED-- case where get-ent fails

(define (chain-next ent key-str k-len ans-str pkt)
  (PKT:SET-BLK-TO-CACHE! pkt (ENT:ID ent))
  (case (PKT:MATCH-TYPE pkt)
    ((PASTP QPASTP)
     (get-this-key (ENT:BLK ent)
		   (PKT:MATCH-POS pkt)
		   key-str ans-str ent k-len pkt))
    ((MATCH)
     (get-this-key (ENT:BLK ent)
		   (NEXT-CNVPAIR (ENT:BLK ent) (PKT:MATCH-POS pkt))
		   key-str ans-str ent k-len pkt))
    (else (release-ent! ent ACCREAD) NOTPRES)))

;;; To shrink a block give growth less than 0 and location equals
;;; position after deleted.
;;; blk-change-size returns #f if not enough room
(define (blk:change-size blk loc growth bsiz)
  (define b-end (BLK:END blk))
  (cond ((zero? growth) #t)
	((> (+ b-end growth) bsiz) ; (if (END-OF-CHAIN? blk) (- bsiz 1) bsiz)
	 #f)
	((negative? growth)
	 (subbytes-move-left!
	  blk loc b-end blk (+ loc growth))
	 (BLK:SET-END! blk (+ b-end growth))
	 #t)
	(else
	 (subbytes-move-right!
	  blk loc b-end blk (+ loc growth))
	 (BLK:SET-END! blk (+ b-end growth))
	 #t)))

(define (blk:remove-key-and-val blk b-pos bsiz)
  (define nb-pos (NEXT-CNVPAIR blk b-pos))
  (cond ((> (FIELD-LEN blk nb-pos) (FIELD-LEN blk b-pos))
	 (let ((delk-pos
		(- (FIELD-LEN blk nb-pos) (FIELD-LEN blk b-pos))))
	   (SET-FIELD-LEN! blk (+ 1 b-pos)
			   (+ (FIELD-LEN blk (+ 1 nb-pos)) delk-pos))
	   (blk:change-size blk
			    (+ 2 nb-pos)
			    (+ (- b-pos nb-pos) delk-pos)
			    bsiz)))
	(else (blk:change-size blk nb-pos (- b-pos nb-pos) bsiz))))

;;; return #t if operation was succsessful; #f if not
;;;
;;; Note the splitting of OBLK into OBLK+NBLK by inserting the split
;;; key of each block into parent.
;;;
;;; Note this routine does not check if the key(s) have already been
;;; (perhaps by another process) inserted into parent.
;;;
;;; unfortunately, the right way to do this requires that the update
;;; look just like a PUT of the NKEY-STR with value N-ID, albeit one
;;; that then swaps the values of the new entry and the one
;;; following...
;;;
;;; The SCREW-CASE occurs when the key is inserted at the endof the
;;; block, so that we have to get access to the next (NON-EMPTY!)
;;; block to make the swap...

(define defer-insert-updates? #f)

(define (parent-insert-update seg top-id level nkey-str nk-len n-id)
  (define pkt (make-vector PKT-SIZE))
;;;#|s|#(dprintf "%s: nkey=%.*s n-id=%d nk-len=%d\n" 'PARENT-INSERT-UPDATE
;;;#|s|#         (max nk-len 0) nkey-str n-id nk-len)
  (let* ((ent (find-ent (get-ent seg top-id ACCNONE) (+ 1 level) -1
			nkey-str nk-len))
	 (xent #f)
	 (screw-case? #f)
	 (blkidstr (make-bytes 4))
	 (blk #f))
    (cond
     (ent
      (long2str! blkidstr 0 n-id)
      (cond ((ents:ent-update-access? ent ACCREAD ACCWRITE)
	     (set! ent (chain-find ent ACCWRITE nkey-str nk-len pkt))
	     (set! blk (ENT:BLK ent)))
	    (else (release-ent! ent ACCREAD)
		  (set! ent #f)))
      (cond ((and ent (at-split-key-pos? blk (PKT:MATCH-POS pkt)))
	     (set! screw-case? #t)
	     (set! xent (next-nonempty-ent (ENT:SEG ent) (BLK:NXT-ID blk)))
	     (if (not xent)
		 (edprintf "No next key found for index insert %ld:%ld\n"
			   (SEG:ID (ENT:SEG ent)) (BLK:ID blk)))))
      (cond ((and (not defer-insert-updates?)
		  ent
		  (or (not screw-case?) xent)
		  (chain-put ent nkey-str nk-len blkidstr 4 pkt xent WCB-SAR))
	     #t)
	    (else
	     (wdprintf "%s: couldn't update parent n-id=%ld nk-len=%d\n"
		       'PARENT-INSERT-UPDATE n-id nk-len)
	     (set! deferred-inserts (+ 1 deferred-inserts))
	     (if ent (release-ent! ent ACCWRITE))
	     #f)))
     (else #f))))

;; only valid if called with POS=position of some KEY
(define (at-split-key-pos? blk pos)
  (= (BLK:END blk) (next-field blk (+ 1 pos))))

(define (next-nonempty-ent seg blknum)
;;;#|c|#  (dprintf "%s blknum=%d:%d\n" 'NEXT-NONEMPTY-ENT seg blknum)
	  (cond
	   ((<= blknum 0) #f)
	   (else
	    (let loop ((xent (get-ent seg blknum ACCREAD)))
	      (and xent (ents:ent-update-access? xent ACCREAD ACCWRITE))
	      (cond ((not xent) #f)
		    ((not (blk-empty? (ENT:BLK xent))) xent)
		    ((zero? (BLK:NXT-ID (ENT:BLK xent)))
		     (release-ent! xent ACCWRITE)
		     #f)
		    (else
		     (loop (switch-ent xent ACCWRITE (BLK:NXT-ID (ENT:BLK xent)) ACCWRITE))))))))

;; Note: CFP must NOT return the split key position IFF at a LEAF
;; RECON-THIS-KEY returns the data in KEY and its length as its return value.
;; END-OF-CHAIN (-1) is returned if the key reconstructed is the end-of-file mark
;; k-len is now used correctly to signal a potential overflow

(define (recon-this-key blk pos key-str k-pos k-len)
  ;;(dprintf "%s blk=%d pos=%d %d %d\n" 'RECON-THIS-KEY (blk:id blk) pos k-pos k-len)
  (do ((b-pos BLK-DATA-START)
       (k-size 0))
      ((> b-pos pos)
       ;;(dprintf "%s returned: %d\n" 'RECON-THIS-KEY k-size)
       k-size)
    (if (and
	 (> k-size (field-len blk b-pos))
	 (<= (byte-ref blk (+ b-pos 2))
	     (byte-ref key-str (+ k-pos (field-len blk b-pos)))))
	(edprintf "bad key sequence %ld @ %d\n" (BLK:ID blk) b-pos))
    (set! k-size (+ (field-len blk b-pos) (field-len blk (+ 1 b-pos))))
    (if (>= k-size k-len) (edprintf "not-enough-room %d\n" k-len))
    (subbytes-move! blk (+ b-pos 2)
		     (+ b-pos 2 (field-len blk (+ 1 b-pos)))
		     key-str (+ k-pos (field-len blk b-pos)))
    ;;(dprintf "%s at-pos %d key= %.*s size= %d\n" 'RECON-THIS-KEY b-pos (+ k-pos k-size) key-str k-size)
    (set! b-pos (next-field blk (+ 1 b-pos)))
    (if (< b-pos (blk:end blk)) (set! b-pos (next-field blk b-pos)))))

(define (blk:insert-and-adjust blk b-pos k-pos key-str k-len val-str v-len bsiz)
  ;;(dprintf "%s %d %d\n" 'BLK:INSERT-AND-ADJUST b-pos k-pos)
  (let* ((oldk-pos (FIELD-LEN blk b-pos)) ;rep count
	 (oldilen (FIELD-LEN blk (+ 1 b-pos)))
	 (ilen (- k-len oldk-pos)))
    (cond ((blk:change-size blk b-pos (+ 2 (- k-len k-pos) 1 v-len) bsiz)
	   ;;(SET-FIELD-LEN! blk b-pos oldk-pos)
	   (set! b-pos (+ 1 b-pos))
	   (set! b-pos (set-field blk b-pos key-str oldk-pos ilen))
	   (set! b-pos (set-field blk b-pos val-str 0 v-len))
	   (SET-FIELD-LEN! blk b-pos k-pos)
	   (SET-FIELD-LEN! blk (+ b-pos 1) (- oldilen (- k-pos oldk-pos)))
	   #t)
	  (else #f))))

(define (blk:simple-insert blk b-pos k-pos key-str k-len val-str v-len bsiz)
  (define ilen (- k-len k-pos))
  ;;(dprintf "%s %d %d\n" 'BLK:SIMPLE-INSERT b-pos k-pos)
  (cond ((blk:change-size blk b-pos (+ 3 v-len ilen) bsiz)
	 (SET-FIELD-LEN! blk b-pos k-pos)
	 (set! b-pos (+ 1 b-pos))
	 (set! b-pos (set-field blk b-pos key-str k-pos ilen))
	 (set-field blk b-pos val-str 0 v-len)
	 #t)
	(else #f)))

(define (blk:change-existing-value blk b-pos key-str k-len val-str v-len bsiz)
  (define ov-len 0)
  (define v-pos (next-field blk (+ 1 b-pos)))
  (set! ov-len (FIELD-LEN blk v-pos))
  ;;(dprintf "%s %d %d %d %d\n" 'BLK:CHANGE-EXISTING-VALUE b-pos v-pos ov-len v-len)
  (cond ((blk:change-size blk (+ v-pos ov-len 1) (- v-len ov-len) bsiz)
	 (set-field blk v-pos val-str 0 v-len)
	 #t)
	(else #f)))

;;; leaf-splits are called with ACCWRITE on blk and return without it.
(define (val-leaf-split blk nblk b-pos key-str k-pos k-len val-str v-len)
  ;;#|s|#(dprintf "%s %ld %d %.*s %.*s\n" 'VAL-LEAF-SPLIT (BLK:ID blk) b-pos (+ k-pos k-len) key-str v-len val-str)
	(let* ((v-pos (next-field blk (+ 1 b-pos)))
	       (s-pos (next-field blk v-pos))
	       (b-end (BLK:END blk)))
	  (SET-FIELD-LEN! nblk BLK-DATA-START 0)
	  (if (> (- b-end s-pos) (- v-pos BLK-DATA-START))
	      (let ((m-len (FIELD-LEN blk s-pos)) ;more room before v-pos
		    (f-chr (byte-ref blk (+ s-pos 2))))
	  ;;#|s|#(dprintf "more room before v-pos\n")
		(SET-FIELD-LEN! nblk (+ BLK-DATA-START 1)
				(+ m-len (FIELD-LEN blk (+ 1 s-pos))))
		(subbytes-move! key-str 0 m-len nblk (+ BLK-DATA-START 2))
		(subbytes-move! blk (+ s-pos 2) b-end
				 nblk (+ BLK-DATA-START m-len 2))
		(BLK:SET-END! nblk (+ (- b-end s-pos) m-len BLK-DATA-START))

		(set! b-pos (set-field blk v-pos val-str 0 v-len))
		(byte-set! blk (+ b-pos 2) f-chr)
		(SET-FIELD-LEN! blk b-pos m-len))
	      (let ((nb-pos (+ BLK-DATA-START 1))) ;more room after s-pos
	  ;;#|s|#(dprintf "more room after s-pos\n")
		(set! nb-pos (set-field nblk nb-pos key-str 0 k-len))
		(set! nb-pos (set-field nblk nb-pos val-str 0 v-len))
		(subbytes-move! blk s-pos b-end nblk nb-pos)
		(BLK:SET-END! nblk (+ nb-pos (- b-end s-pos)))
		))
	  (SET-FIELD-LEN! blk (+ b-pos 1) 1)
	  (BLK:SET-END! blk (+ b-pos 3))
	  b-pos))

(define (qpastp-leaf-split blk nblk b-pos key-str k-pos k-len val-str v-len)
  ;;#|s|#(dprintf "%s %ld %d %.*s %.*s\n" 'QPASTP-LEAF-SPLIT (BLK:ID blk) b-pos (+ k-pos k-len) key-str v-len val-str)
	(let* ((b-end (BLK:END blk)))
	  (SET-FIELD-LEN! nblk BLK-DATA-START 0)
	  (if (> (- b-end b-pos) (- b-pos BLK-DATA-START))
	      (let ((m-len (FIELD-LEN blk b-pos)) ;more room before b-pos
		    (f-chr (byte-ref blk (+ b-pos 2))))
	  ;;#|s|#(dprintf "more room before b-pos\n")
		(SET-FIELD-LEN! nblk (+ BLK-DATA-START 1)
				(+ m-len (FIELD-LEN blk (+ 1 b-pos))))
		(subbytes-move! key-str 0  m-len nblk (+ BLK-DATA-START 2))
		(subbytes-move! blk (+ b-pos 2) b-end
				 nblk (+ BLK-DATA-START m-len 2))
		(BLK:SET-END! nblk (+ (- b-end b-pos) m-len BLK-DATA-START))

		(SET-FIELD-LEN! blk b-pos k-pos)
		(set! b-pos (set-field blk (+ b-pos 1) key-str k-pos (- k-len k-pos)))
		(set! b-pos (set-field blk b-pos val-str 0 v-len))
		(byte-set! blk (+ b-pos 2) f-chr)
		(SET-FIELD-LEN! blk b-pos m-len))
	      (let ((nb-pos (+ BLK-DATA-START 1))) ;more room after b-pos
	  ;;#|s|#(dprintf "more room after b-pos\n")
		(set! nb-pos (set-field nblk nb-pos key-str 0 k-len))
		(set! nb-pos (set-field nblk nb-pos val-str 0 v-len))
		(subbytes-move! blk b-pos b-end nblk nb-pos)
		(BLK:SET-END! nblk (+ nb-pos (- b-end b-pos)))
		(SET-FIELD-LEN! blk b-pos k-pos)
		(byte-set! blk (+ b-pos 2) (byte-ref key-str k-pos))))
	  (SET-FIELD-LEN! blk (+ b-pos 1) 1)
	  (BLK:SET-END! blk (+ b-pos 3))
	  b-pos))

(define (pastp-leaf-split blk nblk b-pos key-str k-pos k-len val-str v-len)
  ;;#|s|#(dprintf "%s %ld %d %.*s %.*s\n" 'PASTP-LEAF-SPLIT (BLK:ID blk) b-pos (+ k-pos k-len) key-str v-len val-str)
	(let ((m-len (FIELD-LEN blk b-pos))
	      (b-end (BLK:END blk)))
	  (SET-FIELD-LEN! nblk BLK-DATA-START 0)
	  (if (> (- b-end b-pos)
		 (- b-pos BLK-DATA-START))
	      (let ((f-chr (byte-ref blk (+ b-pos 2 (- k-pos (FIELD-LEN blk b-pos))))))
		;;#|s|#(dprintf "more room before b-pos\n") ;fixes "xxx2" bug
		      (SET-FIELD-LEN! nblk (+ BLK-DATA-START 1)
				      (+ m-len (FIELD-LEN blk (+ 1 b-pos))))
		      (subbytes-move! key-str 0 m-len nblk (+ BLK-DATA-START 2))
		      (subbytes-move! blk (+ b-pos 2) b-end
				       nblk (+ BLK-DATA-START m-len 2))
		      (BLK:SET-END! nblk (+ (- b-end b-pos) m-len BLK-DATA-START))

		      (SET-FIELD-LEN! blk b-pos m-len)
		      (set! b-pos (set-field blk (+ b-pos 1) key-str m-len (- k-len m-len)))
		      (set! b-pos (set-field blk b-pos val-str 0 v-len))
		      (byte-set! blk (+ b-pos 2) f-chr)	;truncated split key
		      (SET-FIELD-LEN! blk b-pos k-pos)) ;match count
	      (let ((nb-pos (+ BLK-DATA-START 1)) ;more room after b-pos
		    (c-pos (+ b-pos 2 (- k-pos m-len))))
	  ;;#|s|#(dprintf "more room after b-pos\n")
		(set! nb-pos (set-field nblk nb-pos key-str 0 k-len))
		(set! nb-pos (set-field nblk nb-pos val-str 0 v-len))
		(SET-FIELD-LEN! nblk nb-pos k-pos)
		(SET-FIELD-LEN! nblk (+ nb-pos 1)
				(- (+ (FIELD-LEN blk (+ 1 b-pos)) m-len) k-pos))
		(subbytes-move! blk c-pos b-end nblk (+ nb-pos 2))
		(BLK:SET-END! nblk (+ nb-pos 2 (- b-end c-pos)))
;;;	  (byte-set! blk (+ b-pos 2) f-chr) ;fixed "81" bug
;;;	  (SET-FIELD-LEN! blk b-pos m-len)
		))
	  (SET-FIELD-LEN! blk (+ b-pos 1) 1)
	  (BLK:SET-END! blk (+ b-pos 3))
	  b-pos))

(define (dummy-leaf-split blk nblk b-pos key-str k-pos k-len val-str v-len)
  (edprintf "%s: bad-MATCH-TYPE blk %ld\n" 'DUMMY-LEAF-SPLIT (BLK:ID blk))
  0)

#-(or SCM2JAVA SCM2CS)
(define (select-split-fun type)
  (case type
    ((PASTP)  pastp-leaf-split)
    ((QPASTP) qpastp-leaf-split)
    ((MATCH)  val-leaf-split)
    (else dummy-leaf-split)))

(define (chain-put ent key-str k-len val-str v-len pkt xent wcb)
  ;;#|c|#(dprintf "%s %ld:%ld key=%.*s val=%.*s\n" 'CHAIN-PUT (SEG:ID (ENT:SEG ent)) (ENT:ID ent) (max k-len 0) key-str v-len val-str)
  (let* ((blk (ENT:BLK ent))
	 (blklev (BLK:LEVEL blk))
	 (index? (> blklev LEAF))
	 (root-id (BLK:TOP-ID blk))
	 (nent #f)
	 (nrent #f)
	 (seg (ENT:SEG ent))
	 (bsiz (SEG:BSIZ seg))
	 (result? #f)
	 (split? #f)
	 (nkey-ent ent)
	 (nkey-pos (PKT:MATCH-POS pkt))
	 (okey-ent ent)
	 (okey-pos BLK-DATA-START)
	 (n-id 0)
	 (s-pos 0)
	 (split-str (make-bytes 256))
	 (s-len 0))
    (PKT:SET-BLK-TO-CACHE! pkt (ENT:ID ent))
    (cond
     ((and (eq? (PKT:MATCH-TYPE pkt) PASTP)
	   (blk:insert-and-adjust blk (PKT:MATCH-POS pkt) (PKT:KEY-POS pkt)
			      key-str k-len val-str v-len bsiz))
      (set! result? #t))
     ((and (eq? (PKT:MATCH-TYPE pkt) QPASTP)
	   (blk:simple-insert blk (PKT:MATCH-POS pkt) (PKT:KEY-POS pkt)
			  key-str k-len val-str v-len bsiz))
      (set! result? #t))
     ((and (eq? (PKT:MATCH-TYPE pkt) MATCH)
	   (blk:change-existing-value blk (PKT:MATCH-POS pkt)
				  key-str k-len val-str v-len bsiz))
      (set! result? #t))
     ((begin (set! nent (create-new-blk-ent seg))
	     (not nent))
      ;;(set! result? #f)
      )
     (else
      (set! split? #t)
      (let* ((nblk (ENT:BLK nent)))
	(set! n-id (ENT:ID nent))
	(init-next-blk! blk nblk)
	(set! block-splits (+ block-splits 1))
	;;S-POS is new pos of split key in old block
	#+(or SCM2CS SCM2JAVA)  
	(case (PKT:MATCH-TYPE pkt)
	  ((PASTP)  (set! s-pos (pastp-leaf-split blk nblk (PKT:MATCH-POS pkt)
						  key-str (PKT:KEY-POS pkt) k-len val-str v-len)))
	  ((QPASTP) (set! s-pos (qpastp-leaf-split blk nblk (PKT:MATCH-POS pkt)
						   key-str (PKT:KEY-POS pkt) k-len val-str v-len)))
	  ((MATCH)  (set! s-pos (val-leaf-split blk nblk (PKT:MATCH-POS pkt)
						key-str (PKT:KEY-POS pkt) k-len val-str v-len)))
	  (else     (set! s-pos (dummy-leaf-split blk nblk (PKT:MATCH-POS pkt)
						  key-str (PKT:KEY-POS pkt) k-len val-str v-len))))
	#-(or SCM2JAVA SCM2CS)
	(set! s-pos ((select-split-fun (PKT:MATCH-TYPE pkt))
		     blk nblk (PKT:MATCH-POS pkt)
		     key-str (PKT:KEY-POS pkt) k-len val-str v-len))
	(set! s-len (+ 1 (FIELD-LEN blk s-pos)))
	(subbytes-move! nblk (+ BLK-DATA-START 2)
			(+ 1 (FIELD-LEN blk s-pos)
			   (+ BLK-DATA-START 2))
			split-str 0)
	(cond (index?	; set up special info needed for index inserts
	       (set! okey-ent nent)
	       (cond ((not (= (PKT:MATCH-POS pkt) s-pos)) ;insert-in-old-blk? ("SPLIT CASE 1")
					; need: okey-ent=nent, pos=BDS, nkey-ent=ent, pos=MATCH-POS
		      (set! split-index-inserts (+ 1 split-index-inserts)))
		     (else    ; more room after BPOS! ("SPLIT CASE 0")
		      (set! okey-pos (NEXT-CNVPAIR nblk BLK-DATA-START))
		      (set! nkey-ent nent)
		      (set! nkey-pos BLK-DATA-START)
		      ))))
	(if (= (PKT:MATCH-POS pkt) s-pos) (PKT:SET-BLK-TO-CACHE! pkt (ENT:ID nent)))
	(cond
	 ((ROOT? blk)
	  (set! nrent (create-new-blk-ent seg))
	  (cond (nrent
		 (reroot! blk (ENT:BLK nrent) (ENT:ID nrent) (SEG:BSIZ seg))
		 (cond ((eq? nkey-ent ent)
			(set! nkey-ent nrent)
			(PKT:SET-BLK-TO-CACHE! pkt (ENT:ID nrent))))))))
	(set! result? #t))))

    (cond ((and result? index?)	       ; special code for index update
	   (cond (xent		       ; check for the screw case!
;;;#|c|#	  (dprintf "%s/SPECIAL CASE: %ld:%ld %.*s %.*s\n" 'CHAIN-PUT (SEG:ID (ENT:SEG ent)) (ENT:ID ent) (max k-len 0) key-str v-len val-str)
		  (set! index-screw-case (+ 1 index-screw-case))
		  (set! okey-ent xent)
		  (set! okey-pos BLK-DATA-START))
		 ((not split?)
		  (set! okey-pos (NEXT-CNVPAIR blk (PKT:MATCH-POS pkt)))))

;;;#|c|#  (dprintf "%s/INDEX FIX: ent= %ld:%ld nent= %ld:%ld xent= %ld:%ld newkey@%d pos %d oldkey@%d pos %d.\n" 'CHAIN-PUT
;;;#|c|#	   (SEG:ID (ENT:SEG ent)) (ENT:ID ent) (ENT:SEG ent) (if nent (ENT:ID nent) -1) (ENT:SEG ent) (if xent (ENT:ID xent) -1)
;;;#|c|#      (ENT:ID nkey-ent) nkey-pos (ENT:ID okey-ent) okey-pos)
	   (let ((tmpstr (make-bytes 4)) ; swap pointers
		 (oldv-pos (+ (NEXT-FIELD (ENT:BLK okey-ent) (+ okey-pos 1)) 1))
		 (newv-pos (+ (NEXT-FIELD (ENT:BLK nkey-ent) (+ nkey-pos 1)) 1)))
;;;#|c|#   (dprintf "   newvpos=%d oldvpos=%d; match-pos=%d s-pos=%d nrent= %ld:%ld SPLIT-CASE=%d\n"
;;;#|c|#            newv-pos oldv-pos (PKT:MATCH-POS pkt) s-pos (SEG:ID (ENT:SEG ent)) (if nrent (ENT:ID nrent) -1) (if (eq? okey-ent nkey-ent) 0 1))
	     (subbytes-move-left! (ENT:BLK okey-ent) oldv-pos (+ oldv-pos 4) tmpstr 0)
	     (subbytes-move-left! (ENT:BLK nkey-ent) newv-pos (+ newv-pos 4) (ENT:BLK okey-ent) oldv-pos)
	     (subbytes-move-left! tmpstr 0 4 (ENT:BLK nkey-ent) newv-pos))))

;;; at this point: ENT=original block; NENT=neww blk created if ENT split;
;;;                NRENT=blk created to replace ENT, iff ENT split and ROOT;
;;;                XENT=NEXT(original ENT) iff insert-screw-case occurred
    (cond (nrent
	   (ents:ent-write nrent)
	   (release-ent! nrent ACCWRITE)))
    (cond (nent
	   (ents:ent-write nent)
	   ;;to prevent delete of this block while updating parent.
	   (ents:ent-update-access? nent ACCWRITE ACCNONE)))
    (cond (result?
	   ;;(if (not (ENT:DTY? ent)) (wdprintf "result not dirty?\n"))
	   (ENT:SET-DTY! ent #t)
;;;	   (dprintf "%s: blk=%ld split=%d xent=%d SAP=%d\n" 'CHAIN-PUT
;;;		    (BLK:ID (ENT:BLK ent)) split? xent (logtest WCB-SAP wcb))
	   (if (or split? xent (logtest WCB-SAP wcb))
	       (ents:ent-write ent))
	   (release-ent! ent ACCWRITE)))
    (cond (xent
	   ;;(if (not (ENT:DTY? ent)) (wdprintf "xent not dirty?\n"))
	   (ENT:SET-DTY! xent #t)
	   (ents:ent-write xent)
	   (release-ent! xent ACCWRITE)))

    ;;SPLIT? can happen only if RESULT? is #t
    (if split? (parent-insert-update seg root-id blklev split-str s-len n-id))
    (if nent (release-ent! nent ACCNONE)) ;Ok to delete this block now.
    result?))
