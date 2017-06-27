;; "prev.scm" WB-tree File Based Associative String Data Base System.
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

(pragma.c "#include \"wbsys.h\"")

(java:import "Ents" "Seg" "Blink" "Wbdefs" "Wbsys" "Blk" "SchlepRT"
	     "Pkt" "Ent")

#+TRACE-ON
(define (match-str pkt)
  (case (PKT:MATCH-TYPE pkt)
    ((QPASTP)	"QPASTP")
    ((PASTP)	"PASTP")
    ((MATCH)	"MATCH")
    ((MATCHEND)	"MATCHEND")
    ((PASTEND)	"PASTEND")
    (else ">>>>ERROR<<<<")))

;; CHAIN-PREV-FIND [was CHAIN-FIND-PREV] searches fwd from ENT looking for
;; key preceeding KEY-STR.
;; call with ENT in mode ACCMODE, prev-ent=#f, prev-pos=0
;; if found, returns an ENT in mode ACCMODE (match pos is in PKT, type=MATCH);
;; otherwise, returns an ENT in mode ACCMODE, match type=PASTEND, POS=0

(define (chain-prev-find ent accmode key-str k-len pkt prev-ent prev-pos)
  (let ((blk (ENT:BLK ent)))
    (blk:find-pos blk key-str k-len pkt)
    #+TRACE-ON
    (dprintf "%s blk=%ld res=[%s mpos=%d kpos=%d ppos=%d] prev-ent=%ld:%ld ppos=%d\n"
	     'CHAIN-PREV-FIND
	     (BLK:ID blk) (match-str pkt) (PKT:MATCH-POS pkt) (PKT:KEY-POS pkt) (PKT:PREV-MATCH-POS pkt)
	     (and prev-ent (ENT:SEG prev-ent)) (and prev-ent (ENT:ID prev-ent)) prev-pos)
    (cond ((and (eq? (PKT:MATCH-TYPE pkt) PASTEND) (not (END-OF-CHAIN? blk)))
	   (let* ((nxt-num (BLK:NXT-ID blk))
		  (seg (ENT:SEG ent))
		  (nent #f)
		  (empty-blk? (eq? (PKT:MATCH-POS pkt) BLK-DATA-START))
		  (ppos (if empty-blk?
			    prev-pos
			    ;;(blk-prev-key blk (PKT:MATCH-POS pkt))
			    (PKT:PREV-MATCH-POS pkt)
			    )))
	     #+TRACE-ON
	     (dprintf "%s nxt=%ld empty=%d ppos=%d\n" 'CHAIN-PREV-FIND
		      nxt-num empty-blk? ppos)
	     (cond (empty-blk?
		    (release-ent! ent accmode))
		   (else
		    (if prev-ent (release-ent! prev-ent ACCNONE))
		    (ents:ent-update-access? ent accmode ACCNONE)
		    (set! prev-ent ent)))
	     (set! nent (get-ent seg nxt-num accmode))
	     (chain-prev-find nent accmode key-str k-len pkt prev-ent ppos)))
	  ((eq? (PKT:MATCH-POS pkt) BLK-DATA-START) ; KEY found, but
					; PREV(KEY) in prev block
	   (cond (prev-ent
		  (release-ent! ent accmode)
		  (ents:ent-update-access? prev-ent ACCNONE accmode) ;need to back out if #f
		  (PKT:SET-MATCH-TYPE! pkt MATCH)
		  #+TRACE-ON
		  (dprintf "cfp-res1=MATCH at %ld pos=%d\n"
			   (ENT:ID prev-ent) prev-pos)
		  (PKT:SET-MATCH-POS! pkt prev-pos)
		  prev-ent)
		 (else
		  (PKT:SET-MATCH-TYPE! pkt PASTEND)
		  #+TRACE-ON
		  (dprintf "cfp-res3=PASTEND prev-ent=NONE pos=%d\n"
			   prev-pos)
		  (PKT:SET-MATCH-POS! pkt 0)
		  ent)))
	  (else				; found, current block
	   (if prev-ent (release-ent! prev-ent ACCNONE))
	   (PKT:SET-MATCH-TYPE! pkt MATCH)
	   (PKT:SET-MATCH-POS! pkt (PKT:PREV-MATCH-POS pkt))
	   ;; (PKT:SET-MATCH-POS! pkt (blk-prev-key blk (PKT:MATCH-POS pkt)))
	   #+TRACE-ON
	   (dprintf "cfp-res2=MATCH at %ld pos=%d\n"
		    (BLK:ID blk) (PKT:MATCH-POS pkt))
	   ent))))

(define (str-gtr? a-str a-pos a-len b-str b-pos b-len)
  (let loop ((i 0) (ap a-pos) (bp b-pos))
    (cond ((>= i a-len) #f)
	  ((>= i b-len) #t)
	  ((< (byte-ref a-str ap) (byte-ref b-str bp)) #f)
	  ((< (byte-ref b-str bp) (byte-ref a-str ap)) #t)
	  (else (loop (+ i 1) (+ ap 1) (+ bp 1))))))

;; PREV-KEY-ENT [was PREV-KEY] assumes entry with ACCNONE accmode to BLK.
;; It either returns the  entry contining PREV(key) (with READ access)
;; (and pos(prev) in PKT, type=MATCH) or #f, if there is no such key.
;; call PREV-KEY-ENT with ROOT block...

;; NOTE: PREV:PREV-K-ENT still needs the PENT kluge to keep the block
;; unchanged while it works.
(define (prev:prev-k-ent ent key-str k-len level pkt)
  (and ent				; this is also not an "error"
					; keep ptr to blk till we verify its PREV...
       (let ((pent (get-ent (ENT:SEG ent) (ENT:ID ent) ACCNONE)))
	 (set! ent (chain-prev-find ent ACCREAD key-str k-len pkt #f 0))
	 #+TRACE-ON
	 (dprintf "%s now at blk=%ld:%ld cfp: res=[%s mpos=%d kpos=%d ppos=%d]\n"
		  'PREV-KEY-ENT (and ent (SEG:ID (ENT:SEG ent))) (and ent (ENT:ID ent))
		  (match-str pkt) (PKT:MATCH-POS pkt) (PKT:KEY-POS pkt) (PKT:PREV-MATCH-POS pkt))
					; "[and ent" deleted -- rjz
	 (let ((res-ent ent))
	   (cond ((not (eq? (PKT:MATCH-TYPE pkt) MATCH))
		  (release-ent! ent ACCREAD)
		  (set! res-ent (prev:prev-k-ent (prev-blk-ent pent level)
						 key-str k-len level pkt))))
	   (release-ent! pent ACCNONE)
	   res-ent))))

(define (prev-key-ent ent key-str k-len level pkt)
  (cond (ent
	 #+TRACE-ON
	 (dprintf "%s called key=%.*s level=%d blk=%ld:%ld\n" 'PREV-KEY-ENT
		  (max 0 k-len) key-str level (SEG:ID (ENT:SEG ent)) (ENT:ID ent))
	 (prev:prev-k-ent (prev:find-prev-ent ent level -1 key-str k-len)
			  key-str
			  k-len
			  level
			  pkt))
	(else #f)))

;; CHAIN-TO-PREV-ENT: subroutine for PREV-BLK-ENT
;; this routine chains fwd from FROM-ENT to imm predecessor of GOAL-BLK
;; called with FROM-ENT open with ACCREAD; assumes GOAL-BLOCK-NO Name-locked
;; returns an ENT open ACCREAD unless missed block, which returns #f
;; (routine also checks if its past key)

(define (chain-to-prev-ent from-ent goal-blk-num goal-key-str key-len)
  (let ((from-blk (ENT:BLK from-ent)))
    #+TRACE-ON
    (dprintf "%s from %ld:%ld to %ld\n" 'CHAIN-TO-PREV-ENT
	     (SEG:ID (ENT:SEG from-ent)) (ENT:ID from-ent) goal-blk-num)
    (cond
     ((= (BLK:NXT-ID from-blk) goal-blk-num) from-ent)
     ((END-OF-CHAIN? from-blk)
      (edprintf "%s: hit end of %ld:%ld lev=%d %.*s\n"
		'CHAIN-TO-PREV-ENT
		(ENT:ID from-ent)
		goal-blk-num
		(BLK:LEVEL from-blk)
		key-len goal-key-str)
      #f)
     (else
      (let ((b-pos BLK-DATA-START))
	(cond
	 ((str-gtr? from-blk
		    (+ b-pos 2)
		    (FIELD-LEN from-blk (+ b-pos 1))
		    goal-key-str
		    0
		    key-len)
	  (edprintf "%s: missed blk %ld:%ld lev=%d %.*s\n"
		    'CHAIN-TO-PREV-ENT
		    (ENT:ID from-ent)
		    goal-blk-num
		    (BLK:LEVEL from-blk)
		    key-len
		    goal-key-str)
	  #f)
	 (else
	  (chain-to-prev-ent
	   (switch-ent from-ent ACCREAD (BLK:NXT-ID from-blk) ACCREAD)
	   goal-blk-num
	   goal-key-str
	   key-len ))))))))

;; there must be a more efficient way to check this !!!
(define (at-root-level? seg blk)
  (if (ROOT? blk) #t
      (let* ((rent (get-ent seg (BLK:TOP-ID blk) ACCREAD))
	     (rblk (ENT:BLK rent))
	     (rlevel (BLK:LEVEL rblk))
	     (res? (= (BLK:LEVEL blk) rlevel)))
	#+TRACE-ON
	(dprintf "%s blk=%ld:%ld rootlvl=%d result=%d\n"
		 'AT-ROOT-LEVEL? (SEG:ID seg) (BLK:ID blk) rlevel res)
	(release-ent! rent ACCREAD)
	res?)))

;; PREV-BLK-ENT [was PREV-BLK] is called with ENT (with ACCNONE accmode)
;; which IS PRESERVED. IT finds the block that precedes ENT, or #f.
;; It returns a (second) entry with READ access or #f.
;;; TBD - shouldn't it release ENT if returning #f?
;; (no, not as things are now -- rjz)
(define (prev-blk-ent ent level)
  (ents:ent-update-access? ent ACCNONE ACCREAD)	;need to back out if #f
  (let ((blk (ENT:BLK ent)))
    #+TRACE-ON
    (dprintf "%s blk=%ld:%ld level=%d\n" 'PREV-BLK-ENT
	     (SEG:ID (ENT:SEG ent)) (ENT:ID ent) level)
    (ents:ent-update-access? ent ACCREAD ACCNONE)
    (if (ROOT? blk)
	#f		   ;this is not an error, its AT-START-OF-TREE
	(let ((skey-pos (split-key-pos blk)))
	  (and
	   (positive? skey-pos)
	   (let* ((top-num (BLK:TOP-ID blk))
		  (seg (ENT:SEG ent))
		  (goal-blk-num (ENT:ID ent))
		  (new-str (make-bytes 256))
		  (k-len (recon-this-key blk skey-pos new-str 0 256)))
	     (cond
	      ((at-root-level? seg blk)
	       (dprintf "%s code which has never been run!!\n"
			'PREV-BLK-ENT)
	       (chain-to-prev-ent (get-ent seg top-num ACCREAD)
				  goal-blk-num new-str k-len))
	      (else
	       (let ((pkt (make-vector PKT-SIZE)))
		 #+TRACE-ON
		 (dprintf "%s calling prev-key-ent key= %.*s\n"
			  'PREV-BLK-ENT (max 0 k-len) new-str)
		 (set! ent (prev-key-ent (get-ent seg top-num ACCNONE)
					 new-str k-len (+ level 1) pkt))
		 (if (eq? ent #f)
		     #f
		     (let ((nxt-pos
			    (next-field (ENT:BLK ent)
					(+ 1 (PKT:MATCH-POS pkt)))))
		       (define num 0)
		       (cond ((= nxt-pos (BLK:END (ENT:BLK ent)))
			      (dprintf
			       "%s: I'm confused: at split key of blk %ld:%ld"
			       'PREV-BLK-ENT
			       (SEG:ID (ENT:SEG ent))
			       (ENT:ID ent))
			      (set! num (- (PKT:MATCH-POS pkt) 4)))
			     (else (set! num (+ 1 nxt-pos))))
		       (chain-to-prev-ent
			(switch-ent
			 ent ACCREAD
			 (str2long
			  (ENT:BLK ent)
			  num)
			 ACCREAD)
			goal-blk-num new-str k-len))))))))))))

;; FIND-PREV-ENT: called (like FIND-NEXT) with ACCNONE accmode on ENT.
;; Returns a new ENT with ACCREAD accmode. Will always return an ENT
;; unless some GET-ENT fails.

(define (prev:find-prev-ent ent desired-level last-level key-str k-len)
  (and   ent   (ents:ent-update-access? ent ACCNONE ACCREAD) ;need to back out if #f
	 (let ((blk (ENT:BLK ent)))
	   (cond ((= (BLK:LEVEL blk) desired-level) ent)
		 ((< (BLK:LEVEL blk) desired-level)
		  (edprintf "%s: bad blk level\n" 'FIND-PREV-ENT)
		  #f)
		 ((and (>= last-level 0)
		       (not (= (BLK:LEVEL blk) (- last-level 1))))
		  (edprintf "%s: bad blk level %d last=%d in %ld:%ld\n"
			    'FIND-PREV-ENT (BLK:LEVEL blk)
			    last-level (SEG:ID (ENT:SEG ent)) (ENT:ID ent))
		  #f)
		 (else
		  (let ((pkt (make-vector PKT-SIZE)))
		    (set! ent (chain-find ent ACCREAD key-str k-len pkt))
		    (and ent
			 (let* ((nxt-pos (next-field (ENT:BLK ent) (+ 1 (PKT:MATCH-POS pkt))))
				(ptr-pos (if (= nxt-pos (BLK:END (ENT:BLK ent)))
					     (- (PKT:MATCH-POS pkt) 4)
					     (+ 1 nxt-pos))))
			   #+TRACE-ON
			   (dprintf "%s: at %ld:%ld pos=%d next=%d ptrpos=%d\n" 'FIND-PREV-ENT
				    (SEG:ID (ENT:SEG ent)) (ENT:ID ent) (PKT:MATCH-POS pkt) nxt-pos ptr-pos)
			   (prev:find-prev-ent
			    (switch-ent ent
					ACCREAD
					(str2long (ENT:BLK ent) ptr-pos)
					ACCNONE)
			    desired-level (BLK:LEVEL (ENT:BLK ent)) key-str k-len)))))))))
