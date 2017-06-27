;; "segs.scm" WB-tree File Based Associative String Data Base System.
;; Copyright (C) 1991, 1992, 1993, 2000, 2007, 2010 Free Software Foundation, Inc.
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

;; routines in this file return success codes

(pragma.c "#include \"wbsys.h\"")

(java:import "Ents" "Blkio" "Blink" "Wbdefs" "Handle" "Wbsys" "Blk"
	     "SchlepRT" "Lck" "Ent" "Seg" "Han" "Stats")

;;@body
;;
;;Initializes the WB system.  @0 should be called before any other WB
;;functions.
;;
;;@table @var
;;
;;@item @3
;;The size of each disk cache buffer to be allocated from RAM.
;;It should be an integer multiple of the file-system block size.
;;The minimum is 1.5 kB.
;;
;;@item @1
;;
;;The number of (RAM) disk cache buffers to be allocated.
;;This should be proportional to the size of the database working set.
;;If too small, then the disk will be kept busy writing and reading
;;pages which get flushed to read in other pages.
;;The minimum is 12 times the number of threads.
;;
;;The product of @1 and @3 should be less than the size of RAM on your
;;computer.
;;
;;@item @2
;;
;;The number of hash buckets for the (RAM) disk cache.  It should not
;;be less than @1.
;;The minimum is 2, maybe 3 (due to how get-free-ent works).
;;@end table
;;
;;If not all @1 can be allocated (by malloc) then WB can still run
;;properly.
;;The number of buffers actually allocated is returned if successful;
;;a status code is returned otherwise.
;;
;;If the @var{bsiz} argument to @code{make-seg} is larger than the @3
;;which was passed to @0, then the call to @code{make-seg} will fail.
(define (init-wb MAX-NUM-ENTS-CNT MAX-NUM-BUKS MAX-BLK-SIZE)
  (cond
   ((not (<= 12 MAX-NUM-ENTS-CNT))
    (edprintf "%s: %s (%d) must be %d or greater.\n"
	      'init-wb 'MAX-NUM-ENTS-CNT MAX-NUM-ENTS-CNT 12)
    ARGERR)
   ((not (<= 12 MAX-NUM-BUKS))
    (edprintf "%s: %s (%d) must be %d or greater.\n"
	      'init-wb 'MAX-NUM-BUKS MAX-NUM-BUKS 12)
    ARGERR)
   ((not (<= 128 MAX-BLK-SIZE))
    (edprintf "%s: %s (%d) must be %d or greater.\n"
	      'init-wb 'MAX-BLK-SIZE MAX-BLK-SIZE 128)
    ARGERR)
   ((begin (lck! seg-chain-lck)
	   (try-lck? seg-chain-lck))
    (edprintf "%s: lck (mutex) is not self-blocking!\n" 'init-wb)
    STRANGERR)
   ((not lck-tab)
    ;;(dprintf "        Initializing %s.\n" db-version-str)
    ;;(dprintf "        Copyright (C) 1991, 1992, 1993, 2000, 2003 Free Software Foundation, Inc.\n")
    ;;(dprintf "        See file COPYING for terms applying to this program.\n")
    (blkio:init!)
    (clear-stats)
    (set! seg-cntr 4)
    (set! num-buks MAX-NUM-BUKS)
    (set! blk-size MAX-BLK-SIZE)
    (set! free-buk-cntr 0)
    (set! flush-ent-cntr 0)
    (set! buk-tab (make-vector num-buks #f))
    (set! lck-tab (make-vector num-buks #f))
    (set! ent-tab (make-vector ENT-TAB-INC #f))
    (do ((i num-buks (- i 1)))
	((zero? i))
      (vector-set! lck-tab (- i 1) (make-lck (- i 1))))
    (lck! free-ent-lck)
    (do ((i MAX-NUM-ENTS-CNT (- i 1))
	 (bent #f))
	((zero? i) (set! free-ents bent))
      (let ((newent (ENT:MAKE-ENT num-ents-ct)))
	(cond (newent
	       (ENT:SET-NEXT! newent bent)
	       (set! bent newent)
	       (vector-set! ent-tab num-ents-ct newent)
	       (ENT:SET-TAG! newent num-ents-ct)
	       (set! num-ents-ct (+ 1 num-ents-ct))
	       (if (zero? (remainder num-ents-ct ENT-TAB-INC))
		   (let ((tmp-ent-tab
			  (vector-set-length! ent-tab
					      (+ ENT-TAB-INC num-ents-ct))))
		     (if tmp-ent-tab (set! ent-tab tmp-ent-tab)
			 (set! i 1)))))
	      (else			;no more memory - return
	       (set! i 1)))))
    (unlck! free-ent-lck)
    (unlck! seg-chain-lck)
    num-ents-ct)
   ((> MAX-BLK-SIZE blk-size)
    (edprintf "%s: already initialized with smaller blk-size: %d>%d\n"
	      'INIT-WB MAX-BLK-SIZE blk-size)
    (unlck! seg-chain-lck)
    ARGERR)
   (else
    (dprintf "%s: already initialized\n" 'INIT-WB)
    (unlck! seg-chain-lck)
    NOTPRES)))				; -1

;;@body
;;
;;Frees all memory used by the WB system.  All segments will be closed.
;;
;;To preserve database consistency, it is important to call @0 or
;;@code{close-seg} before program termination if changes have been
;;made to a segment.
(define (final-wb)
  (cond (lck-tab     ;make sure that init has happened.
	 ;;(dprintf "%s: lck-tab is non-null\n" 'final-wb)
	 (do ((seg seg-chain seg-chain))
	     ((not seg))
	   ;;(dprintf "%s: close-seg '%s' %d\n" 'final-wb (SEG:STR seg) (SEG:ID seg))
	   (lck! seg-chain-lck)
	   (close-seg seg #t)
	   (unlck! seg-chain-lck))
	 (do ((i num-ents-ct (- i 1)))
	     ((zero? i))
	   (free! (vector-ref ent-tab (+ -1 i)))
	   (vector-set! ent-tab (+ -1 i) #f)
	   (set! num-ents-ct (+ -1 num-ents-ct)))
	 (do ((i num-buks (- i 1)))
	     ((zero? i))
	   (free! (vector-ref lck-tab (- i 1)))
	   (vector-set! lck-tab (- i 1) #f))
	 (free! ent-tab) (set! ent-tab #f)
	 (free! lck-tab) (set! lck-tab #f)
	 (free! buk-tab) (set! buk-tab #f)
	 ;;(free! flush-ent-lck) (set! flush-ent-lck #f)
	 ;;(free! free-ent-lck) (set! free-ent-lck #f)
	 (blkio:final!)
	 SUCCESS)
	(else
	 (dprintf "%s: already finaled\n" 'final-wb)
	 (blkio:final!)
	 SUCCESS)))

;;@noindent
;;The @var{bsiz} of a segment (given in call to @code{make_seg})
;;is a parameter crucial for performance; balancing CPU time traversing
;;blocks with file-system latency.  @var{bsiz} should be an
;;integer multiple of the file-system block size.

;;@noindent
;;In the 1990s our nominal @var{bsiz} was 2.kiB; now it should
;;probably be 4.kiB, 8.kiB, or 16.kiB.

(define seg-cntr 4)
(define seg-chain #f)
#-SCM2C
(define seg-chain-lck (make-lck -3))
(pragma.h "extern LCK *seg_chain_lck;")
(pragma.c "LCK seg_chain_lock = {0, -3, PTHREAD_MUTEX_INITIALIZER};"
	  "LCK *seg_chain_lck = &seg_chain_lock;")

;;; Create segment-descriptor and splice into seg-chain.
(define (new-seg filename caller)
  (cond
   (lck-tab
    (lck! seg-chain-lck)
    (let loop ((sseg seg-chain))
      (define (make-it)
	(let ((seg (new-segd seg-cntr)))
	  (SEG:SET-STR! seg #+SCM2C (strdup filename) #-SCM2C filename)
	  (cond (seg-chain
		 (let ((prev-seg (SEG:PRV seg-chain)))
		   (SEG:SET-NXT! prev-seg seg)
		   (SEG:SET-PRV! seg prev-seg)
		   (SEG:SET-NXT! seg seg-chain)
		   (SEG:SET-PRV! seg-chain seg)))
		(else
		 (SEG:SET-NXT! seg seg)
		 (SEG:SET-PRV! seg seg)))
	  (SEG:SET-FLC! seg (make-vector FLC-LEN 0))
	  (SEG:SET-FLC-LEN! seg 0)
	  (set! seg-cntr (+ 1 seg-cntr))
	  (set! seg-chain seg)
	  (unlck! seg-chain-lck)
	  seg))
      (cond ((not sseg) (make-it))
	    ((and (SEG:STR sseg) (string=? filename (SEG:STR sseg)))
	     (edprintf "%s: segment already open to \"%s\"\n" caller filename)
	     (unlck! seg-chain-lck)
	     #f)
	    ((eq? seg-chain (SEG:NXT sseg)) (make-it))
	    (else (loop (SEG:NXT sseg))))))
   (else
    (edprintf "%s: WB not initialized (need to call %s)\n" caller 'init-wb)
    #f)))

;;; lock must already be held on seg-chain-lck
(define (free-seg! seg)
  ;;(dprintf "%s: 0x%08lx seg-chain=0x%08lx\n" 'free-seg! seg seg-chain)
  (SEG:SET-PORT! seg #f)
  (free! (SEG:STR seg)) (SEG:SET-STR! seg #f)
  (free! (SEG:FLC seg)) (SEG:SET-FLC! seg #f)
  (SEG:SET-FLC-LEN! seg 0)
  (SEG:SET-USED! seg 0)
  (cond ((not seg-chain)
	 (edprintf "%s: %s already empty!\n" 'free-seg! 'seg-chain))
	((eq? (SEG:PRV seg) seg)
	 (if (not (eq? seg seg-chain))
	     (edprintf "%s: corrupted %s.\n" 'free-seg! 'seg-chain))
	 (set! seg-chain #f))
	(else (let ((next-seg (SEG:NXT seg))
		    (prev-seg (SEG:PRV seg)))
		;;(dprintf "%s: seg=0x%08lx next-seg=0x%08lx seg-chain=0x%08lx\n" 'free-seg! seg next-seg seg-chain)
		(SEG:SET-NXT! prev-seg next-seg)
		(SEG:SET-PRV! next-seg prev-seg)
		(if (eq? seg seg-chain)
		    (set! seg-chain
			  (if (eq? seg-chain next-seg) #f next-seg)))))))

(define (check-file? filename)
  (define file (blkio:open-read-only-file filename 128))
  (define tblk (make-bytes 128))
  (if file
      (let ((allok? (and (blkio:read file tblk 128 0)
			 (zero? (str2long tblk 0))
			 (zero? (str2long tblk 4))
			 (zero? (str2long tblk 8))
			 (= #x77 (byte-ref tblk 23))	 ;#\w
			 (= #x62 (byte-ref tblk 24)))))	 ;#\b
	(blkio:file-close! file 128 (not #t))
	allok?)
      #f))

;;@body
;;
;;Opens the database file @1 and returns a @dfn{seg}, false otherwise.
;;The database will be read-only if the @2 argument is false.  It will
;;be read-write if the @2 argument is true.
(define (open-seg filename mutable?)
  (open-segd filename mutable? (not #t)))

;;@body
;;
;;Opens the database file @1 and returns a @dfn{seg}, false otherwise.
;;The database will be read-only if the @2 argument is false.  It will
;;be read-write if the @2 argument is true.  If the @3 argument is
;;false, then @0 will fail if the database file @1 was not closed
;;cleanly; otherwise it will open, clean or not.
(define (open-segd filename mutable? even-if-dirty?)
  (define bsiz (* 3 128))
  (cond
   ((> bsiz blk-size)
    ;;temporarily set bsiz so that we can get it from superblk
    (edprintf "unsupported %s %d > %d\n" 'bsiz bsiz blk-size)
    #f)
   ((not (check-file? filename))
    #f)
   (else
    (let ((seg (new-seg filename 'open-seg)))
      (define (errout reason)
	(edprintf "File \"%s\" %s\n" filename reason)
	(cond (seg
	       (blkio:file-close! (SEG:PORT seg) bsiz mutable?)
	       (free-seg! seg)))
	#f)
      (if (not seg)
	  seg
	  (let loop ((file (if mutable?
			       (blkio:open-modify-file filename bsiz)
			       (blkio:open-read-only-file filename bsiz))))
	    (cond
	     ((blkio:port-open? file mutable?)
	      (SEG:SET-PORT! seg file)
	      (SEG:SET-BSIZ! seg bsiz)
	      (SEG:SET-USED! seg 2)
	      (SEG:SET-FLC-LEN! seg (if mutable? -1 -2))
					;-1 means to read in "FLC" image.
					;-2 means read only.
	      (let ((han (SEG:RT-HAN seg))
		    (tmp-str (make-bytes 256))) ; maximum of 5 used
		(cond
		 ((err? (bt-open seg 0 han (+ WCB-SAP WCB-SAR))) ; superblock
		  (errout "bt-open 0"))
		 ((not (eq? 2 (bt-get han BSIZ-byts 4 tmp-str)))
		  (errout "BSIZ"))
		 ((not (= bsiz (str2short tmp-str 0)))
		  (blkio:file-close! file bsiz mutable?)
		  (set! bsiz (str2short tmp-str 0))
		  (cond ((> bsiz blk-size)
			 (errout "BSIZ too big."))
			(else
			 (loop (if mutable?
				   (blkio:open-modify-file filename bsiz)
				   (blkio:open-read-only-file filename bsiz))))))
		 (else
		  (let ((dty? (and mutable?
				   (err? (bt-rem han CLN-byts 3 tmp-str)))))
		    (cond
		     ((and dty? (not even-if-dirty?))
		      (errout "not cleanly saved; use wbcheck to repair."))
		     (else
		      (if dty? (wdprintf "File \"%s\" not cleanly saved.\n"
					 filename))
		      (cond
		       ((not (eq? 4 (bt-get han USED-byts 4 tmp-str)))
			(errout "USED"))
		       (else
			(SEG:SET-USED! seg (str2long tmp-str 0))
			(cond
			 ((not (eq? 5 (bt-get han FLD-byts 3 tmp-str)))
			  (errout "FLD"))
			 ((err? (bt-open seg (str2long tmp-str 1)
					 (SEG:FL-HAN seg) WCB-SAR))
			  (errout "FLC"))
			 (else
			  (if (not (eqv? (HAN:TYP (SEG:FL-HAN seg)) FRL-TYP))
			      (dprintf "Older type freelist - still supported.\n"))
			  (HAN:SET-WCB! (SEG:FL-HAN seg) WCB-SAR)
			  seg)))))))))))
	     (else
	      (if (blkio:port-open? file mutable?)
		  (blkio:file-close! file bsiz mutable?))
	      (edprintf "could not open file %s\n" filename)
	      (free-seg! seg)
	      #f))))))))

;;@body
;;Closes database segment @1 and the file containing it.  If @2 is
;;NULL, then if there are any problems freeing buffers, then the close
;;is aborted.  A status code is returned.
(define (close-seg seg hammer?)
  ;;(dprintf "%s: closing segment %ld \"%s\"\n" 'CLOSE-SEG (SEG:ID seg) (SEG:STR seg))
  (cond
   ((or (not seg)
	(not (SEG:PORT seg))
	(not (SEG:STR seg))
	(zero? (SEG:USED seg)))
    ;;(wdprintf "%s: segment %ld already closed\n" 'CLOSE-SEG (SEG:ID seg))
    NOTPRES)
   (else
    (flush-flc! seg 5)
    "leave only enough blocks to fit in flc in superblock."
    (if (>= (SEG:FLC-LEN seg) 0)
	(let* ((tmp-str (make-bytes 20)))
	  (do ((i (+ -1 (SEG:FLC-LEN seg)) (+ -1 i)))
	      ((negative? i))
	    (long2str! tmp-str (* 4 i) (vector-ref (SEG:FLC seg) i)))
	  (bt-put (SEG:RT-HAN seg) FLC-byts 3 tmp-str (* 4 (SEG:FLC-LEN seg)))
	  (SEG:SET-FLC-LEN! seg -1)))
    (let ((mutable? (SEG:MUTABLE? seg))
	  (ans
	   (do-seg-buffers
	    seg
	    #-(or SCM2JAVA SCM2CS) flush-buffer
	    #+SCM2JAVA (get-method "wb.Ents" "flushBuffer" (arg-class "Ent")) 
	    #+SCM2CS "flushBuffer"
	    )))
      (cond
       ((or (success? ans) hammer?)
	(if (not (success? ans)) (set! ans NOTPRES))
	(do-seg-buffers
	 seg
	 #-(or SCM2JAVA SCM2CS) purge-buffer
	 #+SCM2JAVA (get-method "wb.Ents" "purgeBuffer" (arg-class "Ent"))
	 #+SCM2CS "purgeBuffer"
	 )
	;;(lck! (SEG:LCK seg))
	(cond
	 (mutable?
	  (blkio:flush-to-file! (SEG:PORT seg) (not #t))
	  ;; Everything is flushed; now set clean flag
	  (if (err? (bt-write (SEG:RT-HAN seg) CLN-byts 3 NO-byts 0))
	      (wdprintf "mutable file \"%s\" already clean?\n"
			(SEG:STR seg)))))
	(bt-close (SEG:RT-HAN seg))
	(bt-close (SEG:FL-HAN seg))
	;; then close
	(blkio:file-close! (SEG:PORT seg) (SEG:BSIZ seg) mutable?)
	;;(unlck! (SEG:LCK seg))
	(free-seg! seg)))
      ans))))

(define db-version-str "wb-2b3")
(define db-authors-str "A. Jaffer, J. Finger, R. Zito-Wolf")

;;@body
;;
;;The integer @2 specifies the size of B-tree blocks.  @2 should be an
;;integer multiple of the file-system block size.  Nominal value is
;;4096.
;;
;;@0 returns an open new empty mutable database named backed by file
;;@1 if successful; otherwise false is returned.
(define (make-seg filename bsiz)
  (define seg (new-seg filename 'make-seg))
  (cond
   ((not seg) seg)
   ((> bsiz blk-size)
    (edprintf "unsupported %s %d > %d\n" 'bsiz bsiz blk-size)
    (unlck! (SEG:LCK seg))
    #f)
   (else
    (let ((file (blkio:create-file filename bsiz)))
      (cond
       ((blkio:port-open? file #t)
	(SEG:SET-PORT! seg file)
	(SEG:SET-BSIZ! seg bsiz)
	(SEG:SET-USED! seg 3)
	(SEG:SET-STR! seg #+SCM2C (strdup filename) #-SCM2C filename)
	(SEG:SET-FLC-LEN! seg -1)
	(let ((rt-han (SEG:RT-HAN seg))
	      (han (SEG:FL-HAN seg))
	      (tmp-str (make-bytes 5)))
	  (cond
	   ((and (bt-open-new seg 0 rt-han (+ WCB-SAP WCB-SAR) DIR-TYP)
		 (bt-open-new seg 1 han WCB-SAP DIR-TYP)
		 (bt-open-new seg 2 han WCB-SAR FRL-TYP))
	    ;;(unlck! (SEG:LCK seg)) ; Done setting SEG:RT-HAN, SEG:FL-HAN
	    (bt-put rt-han NO-byts 0
		    db-version-str (bytes-length db-version-str))
	    (long2str! tmp-str 0 (SEG:USED seg))
	    (bt-put rt-han USED-byts 4 tmp-str 4)
	    (short2str! tmp-str 0 (SEG:BSIZ seg))
	    (bt-put rt-han BSIZ-byts 4 tmp-str 2)
	    (byte-set! tmp-str 0 4)
	    (long2str! tmp-str 1 1)
	    (bt-put rt-han ROOT-byts 4 tmp-str 5)
	    (long2str! tmp-str 1 2)
	    (bt-put rt-han FLD-byts 3 tmp-str 5)
	    (bt-put rt-han FLC-byts 3 NO-byts 0)
	    (if (> bsiz 128)
		(bt-put rt-han AUTHORS-byts 7
			db-authors-str (bytes-length db-authors-str)))
	    seg)
	   (else (edprintf "couldn't allocate ents for file %s\n" filename)
		 (unlck! (SEG:LCK seg))
		 (close-seg seg #t)
		 #f))))
       (else (edprintf "couldn't create new file %s\n" filename)
	     (unlck! (SEG:LCK seg))
	     #f))))))

;;@cindex wcb
;;@noindent The write-control-bits argument (@var{wcb}) to these
;;functions controls the latency of updates to the file after various
;;operations.  These bits are defined as follows:
;;
;;@multitable @columnfractions .1 .3 .6
;;@item @var{value}
;;@tab C-name
;;@tab Meaning
;;@item 1
;;@tab wcb_sap
;;@tab save block after PUTs
;;@cindex wcb_sap
;;@item 2
;;@tab wcb_sar
;;@tab save block after REMOVEs
;;@cindex wcb_sar
;;@item 4
;;@tab wcb_sac
;;@cindex wcb_sac
;;@tab force block save after cached block changes
;;@item 8
;;@tab wcb_fac
;;@tab flush buffer entirely after cached block changes (not currently
;;implemented)
;;@cindex wcb_fac
;;@end multitable

;;@body Opens bt-handle @3 to seg number @1, block number @2, and
;;returns the type of the block.  If no such block exists or is not a
;;root block, then a (negative) status code is returned.
(define (bt-open seg blk-num han wcb)
  (if (and seg (SEG:STR seg))		;allocated
      (let ((ent (get-ent seg blk-num ACCREAD)))
	(cond ((not ent)
	       ARGERR)
	      ((not (root? (ENT:BLK ent)))
	       (release-ent! ent ACCREAD)
	       (edprintf "%s: not a root %ld:%ld\n"
			 'BT-OPEN (SEG:ID seg) blk-num)
	       ;;(check-access!)
	       ARGERR)
	      (else
	       (bt-open-init-han! han ent wcb)
	       (release-ent! ent ACCREAD)
	       ;;(check-access!)
	       (HAN:TYP han))))
      ARGERR))

(define (bt-open-init-han! han ent wcb)
  (define typ (BLK:TYP (ENT:BLK ent)))
  (define seg (ENT:SEG ent))
  (define blk-num (ENT:ID ent))
  (HAN:SET-SEG! han seg)
  (HAN:SET-NUM! han blk-num)
  (HAN:SET-LAST! han blk-num)
  (HAN:SET-TYP! han typ)		;TBD improve. (eh?)
  (if (= typ DIR-TYP)
      (set! wcb (logior wcb (+ WCB-SAP WCB-SAR))))
  (HAN:SET-WCB! han wcb))

;;; Does calling GET-BUK-WAIT admit the possiblity of deadly embrace?
(define (bt-open-new seg blk-num han wcb typ)
  ;;(dprintf "%s %ld:%ld\n" 'BT-OPEN-NEW (SEG:ID seg) blk-num)
  (cond ((negative? blk-num)
	 (edprintf "negative block number %ld\n" blk-num)
	 #f)
	(else
	 (do ((ent (try-get-free-ent #f -1)
		   (try-get-free-ent #f -1)))
	     (ent
	      (init-leaf-blk! (ENT:BLK ent) blk-num typ)
	      (BLK:SET-TIME! (ENT:BLK ent) (current-time))
	      (ENT:SET-NEXT! ent (GET-BUK-WAIT (SEG:ID seg) blk-num))
	      (SET-BUK! (SEG:ID seg) blk-num ent)
	      (ENT:SET-ACC! ent ACCWRITE)
	      (ENT:SET-SEG! ent seg)
	      (ENT:SET-ID! ent blk-num)
	      (ENT:SET-AGE! ent -127) ;not looked at till release-ent!
	      (ENT:SET-DTY! ent #t)
	      (ENT:SET-PUS! ent 0)
	      (ENT:SET-REF! ent 1)
	      (REL-BUK! (SEG:ID seg) blk-num)
	      (bt-open-init-han! han ent wcb)
	      (ents:ent-write ent)
	      (release-ent! ent ACCWRITE) ; releases BUK
	      ;;(check-access!)
	      #t)))))

;;@body Creates a new root block in seg @1 of type @2, opens bt-handle
;;@3 to it, and returns a status code.  If @1 has insufficient room to
;;create a new tree, then the @var{noroom} status code is returned.
;;
;;@0 can be used to create temporary b-trees.  Temporary trees will be
;;be reclaimed by check program after system crashes.  In order to
;;make a tree persistent, add it to a directory (tree).
(define (bt-create seg typ han wcb)
  (if (and seg han)
      (let ((ent (create-new-blk-ent seg)))
	(cond (ent
	       (let* ((blk-num (ENT:ID ent)))
		 (init-leaf-blk! (ENT:BLK ent) blk-num typ)
		 (ENT:SET-PUS! ent 0)
		 (bt-open-init-han! han ent wcb)
		 (ents:ent-write ent)
		 (release-ent! ent ACCWRITE)
		 ;;(check-access!)
		 SUCCESS))
	      (else NOROOM)))
      ARGERR))

;;@body Closes bt-handle @1 and returns @var{SUCCESS}.
;;
;;Currently, @0 has no effect other than to clear @1.
(define (bt-close han)
  (cond (han
	 (HAN:SET-SEG! han #f)
	 (HAN:SET-NUM! han 0)
	 (HAN:SET-TYP! han 0)
	 (HAN:SET-LAST! han 0)
	 SUCCESS)
	(else ARGERR)))
