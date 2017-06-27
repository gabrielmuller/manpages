;; "ents.scm" WB-tree File Based Associative String Data Base System.
;; Copyright (C) 1991, 1992, 1993, 2000, 2003, 2006 Free Software Foundation, Inc.
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

(java:import "Scan" "Blkio" "Prev" "Stats" "Blink" "Segs" "Handle"
	     "Wbdefs" "SchlepRT" "Wbsys" "Blk" "Pkt" "Ent" "Seg"
	     "Han" "Lck")

;;;; tables

(define lck-tab #f)
(define buk-tab #f)
(define ent-tab #f)
(define num-ents-ct 0)
(define num-buks 0)
(define blk-size 0)

(define cache-ent-enable? #t)

;;;; DATABASE LEVEL OPERATIONS

;;; This can be bummed to write less than the full BSIZ if we know
;;; what the disk sector size is.
;; fixed order check in ent-write

(define (ents:ent-write ent)
  (define seg (ENT:SEG ent))
  (define blk (ENT:BLK ent))
  ;;(dprintf "Writing block %ld:%ld\n" (SEG:ID seg) (ENT:ID ent))
  (if (not (BLK:TYP? blk SEQ-TYP))
      (check-key-order! blk))
  (BLK:SET-TIME! blk (current-time))
  (cond ((not (SEG:MUTABLE? seg))
	 (edprintf "%s on read only segment %ld?\n" 'ENT-WRITE (SEG:ID seg))
	 #f)
	((blkio:write (SEG:PORT seg) blk (SEG:BSIZ seg) (ENT:ID ent))
	 (ENT:SET-DTY! ent #f)
	 #t)
	(else
	 (ENT:SET-DTY! ent #t)
	 #f)))

;;; FLUSHING needs to be proportional to time (to put some limit on
;;; how long things are left unwritten) plus write-activity.

;;; NOTE: While flushing a buffer, get ACCPEND accmode to it (to
;;; prevent surprise mods)

(define flush-ent-cntr 0)
#-SCM2C
(define flush-ent-lck (make-lck -2))
(pragma.h "extern LCK *flush_ent_lck;")
(pragma.c "LCK flush_ent_lock = {0, -2, PTHREAD_MUTEX_INITIALIZER};"
	  "LCK *flush_ent_lck = &flush_ent_lock;")

(define (ents-flush trynum flushnum)
  (define flushednum 0)
  (cond
   ((and flush-ent-lck
	 (try-lck? flush-ent-lck))
    (do ((i-ct trynum (- i-ct 1)))
	((or (zero? i-ct) (>= flushednum flushnum))
	 (unlck! flush-ent-lck)
	 flushednum)
      (set! flush-ent-cntr (remainder (+ 1 flush-ent-cntr) num-ents-ct))
      (let ((tent (vector-ref ent-tab flush-ent-cntr)))
	(cond ((and (ENT:DTY? tent) (not (ENT:ACC tent)) (ent-flush? tent))
	       (show-buffer-1 tent)
	       (set! flushednum (+ flushednum 1)))))))
   (else 0)))

(define (ent-flush? tent)
  (define e-seg (ent:seg tent))
  (define e-id (ent:id tent))
  (GET-BUK-LCK? (SEG:ID e-seg) e-id)
  (let loop ((ent (GET-BUK (SEG:ID e-seg) e-id)))
    (cond ((not ent)
	   (REL-BUK! (SEG:ID e-seg) e-id) #f)
	  ((not (eq? tent ent)) (loop (ENT:NEXT ent)))
	  ((not (and (ENT:DTY? ent) (eq? ACCNONE (ENT:ACC ent))))
	   (REL-BUK! (SEG:ID e-seg) e-id) #f)
	  ;;TBD- when multiple readers are allowed we can use
	  ;;ACCREAD accmode instead of ACCPEND accmode to exclude writers.
	  ;;trust me. you need this.
	  (else (ENT:SET-ACC! ent ACCPEND)
		(REL-BUK! (SEG:ID e-seg) e-id)
		(ents:ent-write ent)
		(GET-BUK-WAIT (SEG:ID e-seg) e-id)
		(ENT:SET-ACC! ent ACCNONE)
		(REL-BUK! (SEG:ID e-seg) e-id)
		(set! flush-ct (+ flush-ct 1))
		#t))))

;;; release-ent! gives up all claim to ent, which is expected to be of
;;; type accmode
;; fixed warning about dirty dirs -- twice
;; fixed dirty-block writer in UPDATE-ACCESS!

(define (release-ent! ent accmode)
  (define blknum (ENT:ID ent))
  (define seg (ENT:SEG ent))
  (define buk #f)
;;;  (dprintf "%s %ld:%ld %d\n" 'RELEASE-ENT! (SEG:ID seg) blknum accmode)
  (set! buk (GET-BUK-WAIT (SEG:ID seg) blknum))
;;;(if (not (BLK:TYP? (ENT:BLK ent) SEQ-TYP))
;;;	(check-key-order! (ENT:BLK ent)))
  (if (and (not (eq? ACCNONE accmode)) (not (eq? (ENT:ACC ent) accmode)))
      ;;TBD- clean this error up
      (edprintf "%s: unexpected accmode of %ld:%ld is %d not %d\n"
		'RELEASE-ENT! (SEG:ID seg) blknum (ENT:ACC ent) accmode))
  (cond ((eq? ACCNONE accmode))
	((not (ENT:DTY? ent)))
	((and (BLK:TYP? (ENT:BLK ent) DIR-TYP) (LEAF? (ENT:BLK ent)))
	 (wdprintf "Directory block %ld:%ld dirty at RELEASE-ENT! \n" (SEG:ID seg) blknum)
	 (set! dir-dty-ct (+ 1 dir-dty-ct)))
	((BLK:TYP? (ENT:BLK ent) SEQ-TYP)
	 (REL-BUK! (SEG:ID seg) blknum)
	 (ents:ent-write ent)
	 (set! buk (GET-BUK-WAIT (SEG:ID seg) blknum))))
  (if (not (eq? ACCNONE accmode)) (ENT:SET-ACC! ent ACCNONE))
  (cond ((<= (ENT:REF ent) 0)
	 (ENT:SET-REF! ent 0)
	 (edprintf "REF count below 0 in %ld:%ld\n" (SEG:ID seg) blknum))
	(else
	 (ENT:SET-REF! ent (- (ENT:REF ent) 1))))
  (cond ((not seg)
	 (splice-out-ent! seg blknum buk ent))
	(else
	 (ENT:SET-AGE! ent (+ (if (ENT:DTY? ent) 5 0)
			      (* 5 (+ 6 (- LEAF (BLK:LEVEL (ENT:BLK ent)))))))))
  (REL-BUK! (SEG:ID seg) blknum))

(define (ents:ent-update-access? ent old-accmode new-accmode)
  ;; (dprintf "%s %ld:%ld %d %d\n" 'ENT-UPDATE-ACCESS (SEG:ID (ENT:SEG ent)) (ENT:ID ent) old-accmode new-accmode)
  (GET-BUK-WAIT (SEG:ID (ENT:SEG ent)) (ENT:ID ent))
  (cond ((not (eq? (ENT:ACC ent) old-accmode))
	 (REL-BUK! (SEG:ID (ENT:SEG ent)) (ENT:ID ent))
	 (edprintf "unexpected accmode type on %ld:%ld %d (expected %d)\n"
		   (SEG:ID (ENT:SEG ent)) (ENT:ID ent) (ENT:ACC ent) old-accmode)))
  (cond ((eq? ACCNONE old-accmode))
	((not (ENT:DTY? ent)))
	((BLK:TYP? (ENT:BLK ent) SEQ-TYP)
	 (REL-BUK! (SEG:ID (ENT:SEG ent)) (ENT:ID ent))
	 (ents:ent-write ent)
	 (GET-BUK-WAIT (SEG:ID (ENT:SEG ent)) (ENT:ID ent)))
	((and (BLK:TYP? (ENT:BLK ent) DIR-TYP) (LEAF? (ENT:BLK ent)))
	 (wdprintf "Directory block %ld:%ld dirty at ENT-UPD-ACCESS! \n" (SEG:ID (ENT:SEG ent)) (ENT:ID ent))
	 (set! dir-dty-ct (+ 1 dir-dty-ct))))
  (ENT:SET-ACC! ent new-accmode)
  (REL-BUK! (SEG:ID (ENT:SEG ent)) (ENT:ID ent))
  (and ent #t))

;;; ENT-FREE-LIST stuff -----------------------------------------------------------

(define free-buk-cntr 0)
(define free-ents #f)
#-SCM2C
(define free-ent-lck (make-lck -1))
(pragma.h "extern LCK *free_ent_lck;")
(pragma.c "LCK free_ent_lock = {0, -1, PTHREAD_MUTEX_INITIALIZER};"
	  "LCK *free_ent_lck = &free_ent_lock;")

(define (get-free-free-ent)
  (lck! free-ent-lck)
  (and free-ents
       (let ((free-ent free-ents))
	 (set! free-ents (ENT:NEXT free-ents))
	 (unlck! free-ent-lck)
	 free-ent)))

;; this version assumes the caller has already locked the bucket
;; BUK containing ENT

(define (splice-out-ent! seg blk-num buk ent)
  (do ((bent buk (ENT:NEXT bent))
       (lastent #f bent))
      ((or (not bent) (eq? bent ent))
       (cond
	(bent (if lastent
		  (ENT:SET-NEXT! lastent (ENT:NEXT bent))
		  (SET-BUK! (SEG:ID seg) blk-num (ENT:NEXT bent)))
;;;	      (dprintf "%s buk=%ld:%ld ent=%ld:%ld last=%d\n" 'SPLICE-OUT-ENT!
;;;		       (SEG:ID seg) blk-num (SEG:ID (ENT:SEG bent)) (ENT:ID bent)
;;;		       (if lastent (ENT:ID lastent) -1))
	      (recycle-ent! bent))
	(else (wdprintf "%s couldn't %ld:%ld\n"
			'SPLICE-OUT-ENT! (SEG:ID seg) blk-num))))))

(define (recycle-ent! ent)
  (ENT:SET-DTY! ent #f)
  (ENT:SET-PUS! ent 0)
  (ENT:SET-SEG! ent #f)
  (ENT:SET-ID! ent 0)
  (lck! free-ent-lck)
  (ENT:SET-REF! ent 0)
  (ENT:SET-ACC! ent ACCNONE)
  (ENT:SET-NEXT! ent free-ents)
  (set! free-ents ent)
  (unlck! free-ent-lck))

;;; SELECT-IDLE-ENT selects a candidate entry for reuse.  caller needs to call
;;; RECLAIM-ENT next to splice entry out of its bucket.
;;; NOTE: when called, bucket (lseg lblk-num) is lcked.
;;; The target bucket is assumed unlocked if lseg < 0.
;;; (GET-ENT calls this with the bucket locked to prevent someone else from
;;; getting another entry for the same block.)

(define (select-idle-ent lseg lblk-num)
;;;  (dprintf "%s %ld:%ld\n" 'SELECT-IDLE-ENT (SEG:ID lseg) lblk-num)
  (let ((oldest-ent #f)
	(num-scan (max (min num-buks 10) (quotient num-buks 20)))
	(free-base free-buk-cntr))
;;;  (dprintf "%s aging %d buks\n" 'SELECT-IDLE-ENT num-scan)
    (set! free-buk-cntr (remainder (+ num-scan free-buk-cntr) num-buks))
    (unlck! free-ent-lck)
    (do ((i 0 (+ i 1)))
	((or (and (> i num-scan) oldest-ent) (> i num-buks))
;;; This searches num-buks/20 buckets, or some minimum number like 10.
;;;	       (dprintf "reclaiming ent= %ld:%ld age=%d\n"
;;;			(if oldest-ent (SEG:ID (ENT:SEG oldest-ent)) -1)
;;;			(if oldest-ent (ENT:ID oldest-ent) -1)
;;;			(if oldest-ent (ENT:AGE oldest-ent) -999))
	 (if (> i num-buks) (edprintf "No free ents\n"))
	 oldest-ent)
      (let* ((free-num (remainder (+ free-base i) num-buks))
	     (dont-lock? (if (not lseg) #f
			     (= free-num (HASH2INT (SEG:ID lseg) lblk-num)))))
	(and
	 (or dont-lock? (GET-BUK-LCK? 0 free-num))
	 (do ((ent (GET-BUK 0 free-num) (ENT:NEXT ent)))
	     ((not ent)
	      (or dont-lock? (REL-BUK! 0 free-num)))
;;;	       (dprintf "%s i= %d oldest-ent= %ld:%ld ent= %ld:%ld\n"
;;;			'SELECT-IDLE-ENT
;;;			i (if oldest-ent (ENT:SEG oldest-ent) 0)
;;;			(if oldest-ent (ENT:ID oldest-ent) -1)
;;;			(SEG:ID (ENT:SEG ent)) (ENT:ID ent))
	   (cond
	    ((zero? (ENT:REF ent))
	     (ENT:SET-AGE! ent (+ (if (ENT:DTY? ent) 1 2) (ENT:AGE ent)))
	     (and (eq? ACCNONE (ENT:ACC ent)) ;this is redundant but robust
		  (or (not oldest-ent) (> (ENT:AGE ent) (ENT:AGE oldest-ent)))
		  (set! oldest-ent ent))))))))))

;;; RECLAIM-ENT unlinks ENT from its bucket if its not in use.
;;; It writes out the entry-s block if it's dirty
;;; RECLAIM-ENT has 3 cases
;;;  (a) ENT is in use -- LSEG is unlocked, NIL is returned
;;;  (b) ENT is clean -- ENT is unlinked and returned
;;;  (c) ENT is DIRTY -- ENT is written, unlinked, and reclaimed (put on
;;;            free lsit); LSEG is UNLOCKED, NIL is returned.
;;;    possible optimization in case (c): if LSEG = -,
;;;    ENT could be written, unlinked, and returned (like (b))

(define (reclaim-ent ent lseg lblk-num)
  (let* ((seg (ENT:SEG ent))
	 (blk-num (ENT:ID ent))
	 (segs-equal? (and (not (not lseg))
			   (SAME-BUK? (SEG:ID lseg) lblk-num
				      (SEG:ID seg) blk-num)))
	 (buk (if segs-equal?
		  (GET-BUK (SEG:ID seg) blk-num)
		  (GET-BUK-WAIT (SEG:ID seg) blk-num))))
    (cond ((or (not (zero? (ENT:REF ent))) ; ENT in use?
	       (not (eq? ACCNONE (ENT:ACC ent))))
	   (REL-BUK! (SEG:ID seg) blk-num)
	   (or segs-equal? (not lseg) (REL-BUK! (SEG:ID lseg) lblk-num))
	   (wdprintf "%s: couldn't splice-out-ent %ld:%ld\n"
		     'RECLAIM-ENT (SEG:ID lseg) lblk-num)
	   #f)
	  (else
	   (do ((bent buk (ENT:NEXT bent))
		(lastent #f bent))
	       ((or (not bent) (eq? ent bent))
		(cond
		 ((not bent)
		  (REL-BUK! (SEG:ID seg) blk-num)
		  (or segs-equal? (not lseg) (REL-BUK! (SEG:ID lseg) lblk-num))
		  (edprintf "%s: couldn't find ent in bucket %ld:%ld l=%ld:%ld\n"
			    'RECLAIM-ENT (SEG:ID seg) blk-num (SEG:ID lseg) lblk-num)
		  #f)
		 ;;ent and bent are now the same
		 ((not (ENT:DTY? ent))
		  (if lastent		; unlink
		      (ENT:SET-NEXT! lastent (ENT:NEXT ent))
		      (SET-BUK! (SEG:ID seg) blk-num (ENT:NEXT ent)))
		  (ENT:SET-NEXT! ent #f) ;for safety
		  (or segs-equal? (REL-BUK! (SEG:ID seg) blk-num))
;;;		  (dprintf "%s CLEAN: ent= %ld:%ld l=%ld:%ld seq=%d\n" 'RECLAIM-ENT
;;;			   (SEG:ID seg) blk-num (SEG:ID lseg) lblk-num (if segs-equal? 1 0))
		  ent)
		 (else			;ent is DTY
		  (ENT:SET-ACC! ent ACCPEND)
		  (REL-BUK! (SEG:ID seg) blk-num)
		  (or segs-equal? (not lseg) (REL-BUK! (SEG:ID lseg) lblk-num))
		  (ents:ent-write ent)
		  (set! buk (GET-BUK-WAIT (SEG:ID seg) blk-num))
		  (ENT:SET-ACC! ent ACCNONE)
					; if (not lseg) then should return it directly
		  (splice-out-ent! seg blk-num buk ent)
		  (REL-BUK! (SEG:ID seg) blk-num)
;;;		  (dprintf "%s DIRTY: ent= %ld:%ld l=%ld:%ld seq=%d\n" 'RECLAIM-ENT
;;;			   (SEG:ID seg) blk-num (SEG:ID lseg) lblk-num (if segs-equal? 1 0))
		  #f)))))
	  )))

;; TRY-GET-FREE-ENT either returns a free ent OR unlocks (lseg lblk-num)

(define (try-get-free-ent lseg lblk-num)
  (define ent (get-free-free-ent))
  (cond ((not ent)
	 (set! ent (select-idle-ent lseg lblk-num))
	 (if ent
	     (set! ent (reclaim-ent ent lseg lblk-num))
	     (or (not lseg) (REL-BUK! (SEG:ID lseg) lblk-num)))))
  ent)


;;; Special entry points for Jonathan to do non-B-tree stuff.
;;; Also now used in chain-scan.

(define (allocate-ent)
  (let loop ((ent (try-get-free-ent #f -1))
	     (cnt 19))
    (cond (ent
	   (ENT:SET-ACC! ent ACCWRITE)
	   (ENT:SET-DTY! ent #t)
	   (ENT:SET-PUS! ent 0)
	   (ENT:SET-SEG! ent #f)
	   (ENT:SET-ID! ent 0)
	   (ENT:SET-REF! ent 1)
	   (ENT:SET-NEXT! ent #f)
	   ent)
	  ((zero? cnt) #f)
	  (else (loop (try-get-free-ent #f -1) (+ -1 cnt))))))

(define (ent-copy! to-ent from-ent)
  (if (not (eq? (ENT:ACC to-ent) ACCWRITE))
      (edprintf "%s: copying into non-ACCWRITE %ld:%ld\n"
		'ENT-COPY! (SEG:ID (ENT:SEG to-ent)) (ENT:ID to-ent)))
  (ENT:SET-SEG! to-ent (ENT:SEG from-ent))
  (ENT:SET-ID! to-ent (ENT:ID from-ent))
  (subbytes-move! (ENT:BLK from-ent) 0 (SEG:BSIZ (ENT:SEG from-ent)) (ENT:BLK to-ent) 0))

(define (get-ent-copy to-ent seg blk-num)
  (define from-ent (get-ent seg blk-num ACCREAD))
  (cond (from-ent
	 (ent-copy! to-ent from-ent)
	 (release-ent! from-ent ACCREAD)
	 #t)
	(else #f)))

#+UNUSED
(define (write-ent-copy ent)
  (define to-ent (get-ent (ent:seg ent) (ent:id ent) ACCWRITE))
  (cond (to-ent
	 (ent-copy! to-ent ent)
	 (ENT:SET-DTY! to-ent #t)
	 (release-ent! to-ent ACCWRITE)
	 #t)
	(else #f)))

;;; End of Special entry points for Jonathan to do non-B-tree stuff.

;;;; Stuff to deal with the free-list-cache (FLC)

(define (flush-flc! seg fullness)
  (define fstr (make-bytes 4))
  (define tstr (make-bytes 4))
  (lck! (SEG:LCK seg))
  (cond ((<= (SEG:FLC-LEN seg) fullness)
	 (unlck! (SEG:LCK seg)))
	(#t
	 (long2str! fstr 0 (vector-ref (SEG:FLC seg) (- (SEG:FLC-LEN seg) 1)))
	 (SEG:SET-FLC-LEN! seg (- (SEG:FLC-LEN seg) 1))
	 (unlck! (SEG:LCK seg))
;;;#|f|#	 (dprintf "%s %d:%d\n" 'FLUSH-FLC! (SEG:ID seg) (str2long fstr 0))
	 (long2str! tstr 0 (current-time))
	 (bt-put (SEG:FL-HAN seg) fstr 4 tstr 4) ;TBD check for error
	 (flush-flc! seg fullness))))

;;; Assumes that SEG-LCK is locked by this process

(define (initload-flc? seg)
  (case (SEG:FLC-LEN seg)
    ((-1) (let* ((tmp-str (make-bytes 20))
		 (flc-image-len (bt-get (SEG:RT-HAN seg) FLC-byts 3 tmp-str)))
	    (if (negative? flc-image-len) (set! flc-image-len 0)) ;TBD ??
	    (bt-put (SEG:RT-HAN seg) FLC-byts 3 NO-byts 0)
	    (SEG:SET-FLC-LEN! seg (quotient flc-image-len 4))
	    (do ((i (+ -4 flc-image-len) (+ -4 i)))
		((negative? i))
;;;	     (dprintf "%d %ld\n" i (str2long tmp-str i))
	      (vector-set! (SEG:FLC seg) (quotient i 4) (str2long tmp-str i))))
     #t)
    ((-2) (edprintf "%s on read only segment %ld?\n" 'INITLOAD-FLC! (SEG:ID seg))
     #f)
    (else #t)))

;;; Called with SEG-LCK locked.
;;; If you don't know what you are doing. DON'T DO IT!
;;; Compute inverse hash function so that ent can still be found.
(define (amnesia-ent! ent)
  (define segid (SEG:ID (ENT:SEG ent)))
  (let ((hash (HASH2INT segid (ENT:ID ent))))
    (define newid (HASH2INT (+ 1 segid) (ENT:ID ent)))
    (ENT:SET-ID! ent newid)
    (ENT:SET-SEG! ent #f)
    ;; -1 is shortcut for (SEG:ID (ENT:SEG ent))
    (cond ((not (= hash (HASH2INT -1 newid)))
	   (wdprintf "%s: hash mismatch %d >< %ld\n"
		     'amnesia-ent!
		     hash
		     (HASH2INT -1 newid))))
    (ENT:SET-DTY! ent #f) ;so block will not be written out when released.
    (ENT:SET-PUS! ent 0)
    (if (ENT:BLK ent)
	(if (BLK:TYP? (ENT:BLK ent) DIR-TYP)
	    (BLK:SET-TYP! (ENT:BLK ent) IND-TYP))) ; avoid useless warnings or writes
    (ENT:SET-AGE! ent 128)))

(define (blk-free ent)
  (define seg (ENT:SEG ent))
;;;#|f|#  (dprintf "%s %d:%d\n" 'BLK-FREE (SEG:ID seg) (ENT:ID ent))
  (cond ((not (eq? (ENT:ACC ent) ACCWRITE))
	 (edprintf "%s: %ld:%ld without ACCWRITE\n"
		   'BLK-FREE (SEG:ID seg) (ENT:ID ent))
	 #f)
	(else
	 (lck! (SEG:LCK seg))
	 (cond
	  ((not (initload-flc? seg)) (unlck! (SEG:LCK seg)) #f)
	  ((>= (SEG:FLC-LEN seg) (- FLC-LEN 1))
	   (unlck! (SEG:LCK seg))
	   (flush-flc! seg (- FLC-LEN 2))
	   (blk-free ent))
	  (else
	   (vector-set! (SEG:FLC seg) (SEG:FLC-LEN seg) (ENT:ID ent))
	   (SEG:SET-FLC-LEN! seg (+ (SEG:FLC-LEN seg) 1))
	   (amnesia-ent! ent)		;renumber entry to seg -1
	   (unlck! (SEG:LCK seg))
	   #t)))))

(define (flc-fill seg)
  (define fstr (make-bytes 4))
  (define flen 0)
  (define (unlck-and-return status)
    (unlck! (SEG:LCK seg))
    (unlck! (SEG:FCK seg))
    status)
;;;#|f|#  (dprintf "%s %d\n" 'FLC-FILL (SEG:FLC-LEN seg))
  (lck! (SEG:LCK seg))
  (cond ((>= (SEG:FLC-LEN seg) 1)
	 (unlck! (SEG:LCK seg)) SUCCESS) ;FLC has some blks in it.
	((not (try-lck? (SEG:FCK seg)))	 ; prevent multiple fillers
	 (unlck! (SEG:LCK seg))
	 (wdprintf "%s failed -- branch never tried before! Segment %ld %s\n"
		   'FLC-FILL (SEG:ID seg) (SEG:STR seg))
	 RETRYERR)
	((begin
	   (set! flen (bt-next (SEG:FL-HAN seg) NO-byts 0 fstr))
	   (err? flen))			;No blks left in free-list
	 (let ((xnum (+ (SEG:USED seg) (quotient FLC-LEN 2))))
	   (cond ((blkio:file-extend (SEG:PORT seg) (SEG:BSIZ seg) xnum)
		  (do ((i 0 (+ i 1)))
		      ((> i (quotient FLC-LEN 2))) ;this is actually + 1.
		    (vector-set! (SEG:FLC seg) (SEG:FLC-LEN seg) (- xnum i))
		    ;;reverse order so blks are allocated in order
		    (SEG:SET-FLC-LEN! seg (+ (SEG:FLC-LEN seg) 1))
		    (SEG:SET-USED! seg (+ (SEG:USED seg) 1)))
		  (let ((used-str (make-bytes 4))) ; This put should not cause a split!
		    (long2str! used-str 0 (SEG:USED seg))
		    (bt-put (SEG:RT-HAN seg) USED-byts 4 used-str 4))
		  (unlck-and-return SUCCESS))
		 (else
		  (edprintf "No more file space available! Segment %ld %s\n"
			    (SEG:ID seg) (SEG:STR seg))
		  (unlck-and-return NOROOM)))))
	(else
	 (unlck! (SEG:LCK seg))
	 (let ((long-ara (make-vector (+ FLC-LEN 1)))
	       (xstr (make-bytes 256))
	       (respkt (make-vector PKT-SIZE))
	       (result SUCCESS))
	   (subbytes-move! fstr 0 flen xstr 0)
	   (vector-set! long-ara 0 0)	; data count
	   (PKT:SET-SKEY-COUNT! respkt 0)
	   (set! result
		 (bt-scan
		  (SEG:FL-HAN seg) REM-SCAN xstr flen
		  NO-byts END-OF-CHAIN
		  #-(or SCM2JAVA SCM2CS) flc-proc
		  #+SCM2JAVA (get-method "wb.Ents" "flcProc" (arg-class "byte[]" "int" "byte[]" "int" "int[]"))
		  #+SCM2CS "flcProc"
		  long-ara respkt 1))
	   (cond ((or (= result SUCCESS) (= result NOTPRES) (= result TERMINATED))
;;;#|f|#		  (dprintf "%s: %d blks fetched from free list \n" 'FLC-FILL (vector-ref long-ara 0))
		  (lck! (SEG:LCK seg)) ;successful remove from free-list
		  (do ((i (vector-ref long-ara 0) (- i 1)))
		      ((<= i 0))
		    (vector-set! (SEG:FLC seg) (SEG:FLC-LEN seg)
				 (vector-ref long-ara i))
;;;		    (dprintf "%s: put block %d into FLC \n" 'FLC-FILL (vector-ref long-ara i))
		    (SEG:SET-FLC-LEN! seg (+ (SEG:FLC-LEN seg) 1)))
		  (unlck-and-return SUCCESS))
		 (else
		  (unlck! (SEG:FCK seg))
		  result))))))

(define (flc-proc keystr klen vstr vlen long-ara)
  (let ((ct (vector-ref long-ara 0)))
    (if (< ct (quotient FLC-LEN 2))
	(let ((num (str2long keystr 0)))
	  (set! ct (+ ct 1))
;;;	  (dprintf "%s: got block %d ct=%d from freelist \n" 'FLC-PROC num ct)
	  (vector-set! long-ara 0 ct)
	  (vector-set! long-ara ct num)
	  SUCCESS)
	TERMINATED)))

;;; create-new-blk-ent leaves you with ACCWRITE accmode to blk
(define (create-new-blk-ent seg)
;;;#|f|# (dprintf "%s\n" 'CREATE-NEW-BLK-ENT)
  (lck! (SEG:LCK seg))
  (cond ((not (initload-flc? seg)) (unlck! (SEG:LCK seg)) #f)
	((<= (SEG:FLC-LEN seg) 0)
	 (unlck! (SEG:LCK seg))
	 (and (not (realerr? (flc-fill seg)))
	      (create-new-blk-ent seg)))
	(else
	 (SEG:SET-FLC-LEN! seg (- (SEG:FLC-LEN seg) 1))
	 (let ((bnum (vector-ref (SEG:FLC seg) (SEG:FLC-LEN seg))))
	   (unlck! (SEG:LCK seg))
	   (let ((ent (get-ent seg bnum ACCWRITE))) ;no read is done here.
	     (if ent (ENT:SET-DTY! ent #t))
	     ent)))))

;;; End of stuff to deal with the free-list-cache (FLC)

;;; try-get-ent returns an entry with accmode or #f if blk is lcked.
;;; When you are done with the entry you need to release-ent!.
(define (try-get-ent seg blk-num accmode)
;;;  (dprintf "%s %ld:%ld %d\n" 'TRY-GET-ENT (SEG:ID seg) blk-num accmode)
  (let ((buk (GET-BUK-WAIT (SEG:ID seg) blk-num)))
    (let entloop ((ent buk))
      (cond
       ((not ent)
	(REL-BUK! (SEG:ID seg) blk-num)
	(set! tge-fct (+ 1 tge-fct))
	#f)
       ((not (and (eq? seg (ENT:SEG ent))
		  (= blk-num (ENT:ID ent)))) ;chain through buk
	(entloop (ENT:NEXT ent)))
       ((not (= (BLK:ID (ENT:BLK ent)) blk-num))
	(REL-BUK! (SEG:ID seg) blk-num)
	(edprintf "corrutped buffer %ld:%ld <> %ld\n"
		  (SEG:ID (ENT:SEG ent)) (BLK:ID (ENT:BLK ent)) blk-num)
	(set! tge-fct (+ 1 tge-fct))
	#f)
       ((eq? ACCNONE accmode)		; only asking NAME access
	(ENT:SET-REF! ent (+ 1 (ENT:REF ent)))
	(REL-BUK! (SEG:ID seg) blk-num)
	(set! tge-ct (+ 1 tge-ct))
	ent)
       ((eq? ACCNONE (ENT:ACC ent))	; entry not lcked
	(ENT:SET-ACC! ent accmode)
	;;(if (eq? accmode ACCWRITE) (ENT:SET-DTY! ent #t))
	(ENT:SET-REF! ent (+ 1 (ENT:REF ent)))
	(REL-BUK! (SEG:ID seg) blk-num)
	(set! tge-ct (+ 1 tge-ct))
	ent)
       (else				; entry not available
	(REL-BUK! (SEG:ID seg) blk-num)
	(set! tge-fct (+ 1 tge-fct))
	#f)))))

(define (chain-find-ent han accmode key-str k-len pkt)
  (define ent
    (if (and cache-ent-enable? (HAN:LAST han))
	(try-get-ent (HAN:SEG han) (HAN:LAST han) accmode)
	#f))
  (cond
   ((and ent
	 (LEAF? (ENT:BLK ent))
	 (= (BLK:TOP-ID (ENT:BLK ent)) (HAN:ID han))
	 (blk:find-pos (ENT:BLK ent) key-str k-len pkt)
	 (or (eq? (PKT:MATCH-TYPE pkt) MATCH)
	     (and (or (eq? (PKT:MATCH-TYPE pkt) PASTP)
		      (eq? (PKT:MATCH-TYPE pkt) QPASTP))
		  (> (PKT:MATCH-POS pkt) BLK-DATA-START))))
    ;;(dprintf "%s: returned blk %ld:%ld\n" 'CHAIN-FIND-ENT (SEG:ID (ENT:SEG ent)) (ENT:ID ent))
    (set! tce-ct (+ tce-ct 1))
    ent)
   (else
    (if ent (release-ent! ent accmode))
    (set! tce-fct (+ tce-fct 1))
    (set! ent (get-ent (HAN:SEG han) (HAN:ID han) ACCNONE))
    (cond ((or (not (root? (ENT:BLK ent)))
	       (BLK:TYP? (ENT:BLK ent) SEQ-TYP))
	   (edprintf "%s: not a B-tree root %ld:%ld\n"
		     'BT-OPEN (SEG:ID (ENT:SEG ent)) (ENT:ID ent))
	   (release-ent! ent ACCNONE)
	   (set! ent #f))
	  (else
	   (set! ent (find-ent ent LEAF -1 key-str k-len))))
    (cond ((not ent) #f)
	  ((eq? ACCREAD accmode) #f)
	  ((ents:ent-update-access? ent ACCREAD accmode))
	  (else (release-ent! ent ACCREAD)
		(set! ent #f)))
    (if ent (set! ent (chain-find ent accmode key-str k-len pkt)))
    (and ent (HAN:SET-LAST! han (ENT:ID ent)))
    ent)))

;;; I havent put the call to PREV:PREV-K-ENT inside here,
;;; as both paths need to call it - rjz

(define (chain-find-prev-ent han accmode key-str k-len pkt)
  (define ent
    (if (and cache-ent-enable? (HAN:LAST han))
	(try-get-ent (HAN:SEG han) (HAN:LAST han) accmode)
	#f))
  (cond
   ((and ent
	 (LEAF? (ENT:BLK ent))
	 (= (BLK:TOP-ID (ENT:BLK ent)) (HAN:ID han))
	 (blk:find-pos (ENT:BLK ent) key-str k-len pkt)
	 (or (eq? (PKT:MATCH-TYPE pkt) MATCH)
	     (eq? (PKT:MATCH-TYPE pkt) MATCHEND)
	     (and (or (eq? (PKT:MATCH-TYPE pkt) PASTP)
		      (eq? (PKT:MATCH-TYPE pkt) QPASTP))
		  (> (PKT:MATCH-POS pkt) BLK-DATA-START))))
    ;;(dprintf "%s: returned blk %ld:%ld\n" 'CHAIN-FIND-PREV-ENT (SEG:ID (ENT:SEG ent)) (ENT:ID ent))
    (set! tce-ct (+ tce-ct 1))
    ent)
   (else
    (if ent (release-ent! ent accmode))
    (set! tce-fct (+ tce-fct 1))
    (set! ent (prev:find-prev-ent
	       (get-ent (HAN:SEG han) (HAN:ID han) ACCNONE)
	       LEAF -1 key-str k-len))
    (cond ((not ent) #f)
	  ((eq? accmode ACCREAD) #f)
	  ((ents:ent-update-access? ent ACCREAD accmode))
	  (else (release-ent! ent ACCREAD)
		(set! ent #f)))
    ;;(if ent (set! ent (prev:prev-k-ent ent key-str k-len LEAF pkt)))
    ent)))

;(REL-BUK! (SEG:ID seg) blk-num)
;(edprintf "all ents in use!\n")

(define (get-ent seg blk-num accmode)
  ;;(dprintf "%s %ld:%ld %d\n" 'GET-ENT (SEG:ID seg) blk-num accmode)
  (cond
   ((negative? blk-num)
    (edprintf "negative block number %ld\n" blk-num) #f)
   ((>= blk-num (SEG:USED seg))
    (edprintf "bad block number %ld:%ld (>= %ld)\n"
	      (SEG:ID seg) blk-num (SEG:USED seg))
    #f)
   (else
    (let entloop ((ent (GET-BUK-WAIT (SEG:ID seg) blk-num)))
      (cond
       ((not ent)			;not here; get from disk
	(set! ent (try-get-free-ent seg blk-num))
	(cond
	 (ent
	  (ENT:SET-NEXT! ent (GET-BUK (SEG:ID seg) blk-num))
	  (SET-BUK! (SEG:ID seg) blk-num ent)
	  (ENT:SET-ACC! ent ACCPEND)
	  (ENT:SET-SEG! ent seg)
	  (ENT:SET-ID! ent blk-num)
	  (ENT:SET-AGE! ent -127)     ;not looked at till release-ent!
	  (ENT:SET-DTY! ent #f)
	  (ENT:SET-PUS! ent 0)
	  (ENT:SET-REF! ent 1)
	  (REL-BUK! (SEG:ID seg) blk-num)
	  ;;(dprintf "Reading block %ld:%ld\n" (SEG:ID seg) blk-num)
	  (cond
	   ((eq? accmode ACCWRITE)
	    (ENT:SET-ACC! ent ACCWRITE)
	    (ENT:SET-DTY! ent #t)
	    (init-leaf-blk! (ENT:BLK ent) blk-num IND-TYP)
	    (set! ge-ct (+ 1 ge-ct))
	    ent)
	   ((blkio:read (SEG:PORT seg) (ENT:BLK ent) (SEG:BSIZ seg) blk-num)
	    (ENT:SET-ACC! ent accmode) ;lines before here don't need to lck buk
	    (if (not (= (BLK:ID (ENT:BLK ent)) blk-num))
		(edprintf "corrupted blk %ld:%ld <> %ld\n"
			  (SEG:ID (ENT:SEG ent)) blk-num (BLK:ID (ENT:BLK ent))))
	    (set! ge-ct (+ 1 ge-ct))
	    ent)
	   (else			;read bad; errmsg in blk-read
	    (ENT:SET-REF! ent 0)
	    (ENT:SET-ACC! ent ACCNONE)
	    (set! ge-fct (+ 1 ge-fct))
	    #f)))
	 (else (entloop (GET-BUK-WAIT (SEG:ID seg) blk-num))))) ; try again
       ((not (and (eq? seg (ENT:SEG ent))
		  (= blk-num (ENT:ID ent)))) ;chain through buk
	(entloop (ENT:NEXT ent)))
       ((not (= (BLK:ID (ENT:BLK ent)) blk-num))
	(REL-BUK! (SEG:ID seg) blk-num)
	(edprintf "corrupted buffer %ld:%ld <> %ld\n"
		  (SEG:ID (ENT:SEG ent)) (BLK:ID (ENT:BLK ent)) blk-num)
	(set! ge-fct (+ 1 ge-fct))
	#f)
       ((eq? ACCNONE accmode)		; only asking NAME access
	(ENT:SET-REF! ent (+ 1 (ENT:REF ent)))
	;; (ENT:ACC ent) is actually ACCREAD
	(REL-BUK! (SEG:ID seg) blk-num)
	(set! ge-ct (+ 1 ge-ct))
	ent)
       ((eq? ACCNONE (ENT:ACC ent))	; entry not lcked
	(ENT:SET-ACC! ent accmode)
	;;(if (eq? accmode ACCWRITE) (ENT:SET-DTY! ent #t))
	(ENT:SET-REF! ent (+ 1 (ENT:REF ent)))
	(REL-BUK! (SEG:ID seg) blk-num)
	(set! ge-ct (+ 1 ge-ct))
	ent)
       (else				; entry not available
	(REL-BUK! (SEG:ID seg) blk-num)
	(set! ge-fct (+ 1 ge-fct))
	#f))))))

(define (switch-ent old-ent oldacc new-num newacc)
;;;  (dprintf "%s %ld:%ld %d %d %d\n" 'SWITCH-ENT
;;;	   (SEG:ID (ENT:SEG old-ent)) (ENT:ID old-ent) oldacc new-num newacc)
  (let ((new-ent (get-ent (ENT:SEG old-ent) new-num ACCNONE)))
    (release-ent! old-ent oldacc)
    (if new-ent (ents:ent-update-access? new-ent ACCNONE newacc)) ;doesn't check that access changed
    new-ent))

(define (check-blk! blk)
  (let ((b-end (BLK:END blk)))
    (let lp ((b-pos BLK-DATA-START))
      (let ((s-pos (next-field blk (+ 1 b-pos))))
	(cond
	 ((= s-pos b-end) #f)
	 ((< s-pos b-end) (lp (next-cnvpair blk b-pos)))
	 (else
	  (edprintf "%s: blk %ld past end %d\n"
		    'CHECK-BLK! (BLK:ID blk) s-pos)
	  #f))))))

(define (check-key-order! blk)
  (define split-str (make-bytes 256))
  (define spos (split-key-pos blk))
  (and (positive? spos) (recon-this-key blk spos split-str 0 256)))

(define (do-seg-buffers seg func)
  (let lp ((i num-buks) (ent #f))	;was (ent free-ents)
    (cond ((not ent)
	   (if (zero? i)
	       SUCCESS
	       (lp (- i 1) (vector-ref buk-tab (- i 1)))))
	  ((or (not seg) (eq? seg (ENT:SEG ent)))
	   (let ((ans (func ent)))
	     (if (success? ans)
		 (lp i (ENT:NEXT ent))
		 ans)))
	  (else (lp i (ENT:NEXT ent))))))

(define (check-buffer ent)
  (cond ((not (zero? (ENT:REF ent)))
	 ;;(and (not (zero? (ENT:ID ent))))
	 (edprintf "Entry still referenced: %ld:%ld\n"
		   (SEG:ID (ENT:SEG ent)) (ENT:ID ent))
	 (ENT:SET-REF! ent 0)))
  (cond ((not (eq? ACCNONE (ENT:ACC ent)))
	 (edprintf "Entry still lcked: %ld:%ld\n"
		   (SEG:ID (ENT:SEG ent)) (ENT:ID ent))
	 (ents:ent-update-access? ent (ENT:ACC ent) ACCNONE)))
  SUCCESS)

(define (check-access!)
  ;;(ents-flush 30 3) ;TBD remove when flush works on alarm int.
  (check-lcks)
  (do-seg-buffers
   #f
   #-(or SCM2JAVA SCM2CS) check-buffer
   #+SCM2JAVA (get-method "wb.Ents" "checkBuffer" (arg-class "Ent"))
   #+SCM2CS  "checkBuffer"
   ))


;;; This routine needs to deal with lck issues.
;;; TBD needs to give error if lcked.
(define (flush-buffer ent)
  (cond ((not (eq? ACCNONE (ENT:ACC ent))) TERMINATED)
	((ENT:DTY? ent) (if (ents:ent-write ent) SUCCESS RETRYERR))
	(else SUCCESS)))

(define (purge-buffer ent)
  (cond ((ENT:DTY? ent)
	 (if (or (eq? (ENT:ACC ent) ACCWRITE)
		 (eq? (ENT:ACC ent) ACCPEND))
	     (dprintf "  Purging %s entry: %ld:%ld\n"
		      (if (eq? (ENT:ACC ent) ACCWRITE) "ACCWRITE" "ACCPEND")
		      (SEG:ID (ENT:SEG ent)) (ENT:ID ent)))
	 (ents:ent-write ent)))
  (amnesia-ent! ent)
  SUCCESS)
