;; "scan.scm" WB-tree File Based Associative String Data Base System.
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

(pragma.c "#include \"wbsys.h\"")

(java:import "Ents" "Stats" "Blink" "Del" "Handle" "Wbdefs" "Wbsys"
	     "Blk" "SchlepRT" "Pkt" "Seg" "Han" "Ent")

;;;@body @0 scans all keys in the range [@3..@5),
;;;performing one of several functions:
;;;
;;;@multitable @columnfractions .2 .15 .66
;;;@item @2
;;;@tab @7
;;;@tab RESULT
;;;@item COUNT-SCAN
;;;@tab NIL
;;;@tab counts all keys in range
;;;@item COUNT-SCAN
;;;@tab given
;;;@tab counts all keys in range satisfying @7
;;;@item REM-SCAN
;;;@tab NIL
;;;@tab deletes all keys in range
;;;@item REM-SCAN
;;;@tab given
;;;@tab deletes all keys in range satisfying @7
;;;@item MODIFY-SCAN
;;;@tab NIL
;;;@tab ARGERR
;;;@item MODIFY-SCAN
;;;@tab given
;;;@tab updates values for keys in range satisfying @7
;;;@end multitable
;;;
;;;@0 returns SUCCESS if scan completed; under any other result code
;;;the scan is resumable. The possible results are:
;;;
;;;@table @asis
;;;@item NOTPRES
;;;meaning the @10 was exceeded;
;;;@item RETRYERR
;;;meaning @7 or delete got a RETRYERRR;
;;;@item TERMINATED
;;;meaning @7 asked to terminate the scan;
;;;@item <other error>
;;;means @7 or DELETE encountered this errror.
;;;@end table
;;;
;;;Each block of data is scanned/deleted/modified in a single operation
;;;that is, the block is found and locked only once, and only written after
;;;all modifications are made. Tho only exception is that MODIFY-SCANs
;;;that increase the size of values  can cause block splits. Such cases
;;;are detected and converted to a PUT plus a NEXT. This has
;;;two consequences: data is written out each time a PUT occurs,
;;;and it is conceivable that @7 may be called more than once on the
;;;key value that caused the split if a RETRYERR occurs in the PUT.
;;;However, SCAN guarantees that only one modification will actually be
;;;made in this case (so that one can write INCREMENT-RANGE, for example).
;;;
;;;@7 is passed pointers to (copies of) the key and value,
;;;plus one user argument:
;;;@example
;;;@7 (keystr klen vstr vlen extra_arg);
;;;@end example
;;;@7 is expected to return either: SUCCESS for DELETE/COUNT,
;;;NOTPRES/NOTDONE for SKIP (ie, DONT DELETE/COUNT), or
;;;any other code to terminate the scan resumably at the current point.
;;;For MODIFY-SCAN, if changing the value, the new value length is returned.
;;;Except for the case mentioned above, the caller can depend on @7
;;;being called exactly once for each key value in the specified range,
;;;and only on those values.
;;;
;;;If @5 <= @3, then no scan will occur (even if @3 is found).
;;;To make possible bounded-time operation @0 will
;;;access at most @10 blocks at a time; if you dont care,
;;;give it -1 for @10.
;;;
;;;The number of keys deleted/counted/modified is returned in the
;;;@code{skey-count} field of @9; the key to resume at is returned in
;;;@3 (@emph{which therefore needs to be 256 bytes long}); and the
;;;new key length is returned in the @code{skey-len} field of @9. If
;;;returns SUCCESS, @code{skey-len} is zero.  NOTE that
;;;@code{skey-count} is cumulative, so the caller needs to initialize
;;;it to 0 when starting a new @0.
;;;
;;;@emph{WARNING:} when @0 returns other than SUCCESS, it modifies
;;;the @3 string so that the string args are correctly set up for the
;;;next call (the returned value is the new length for @3).
;;;Therefore, @emph{@3 must be a maximum-length string!}
(define (bt-scan han operation kstr1 len1 kstr2 len2
		 func long-tab respkt blk-limit)
  (define pkt (make-vector PKT-SIZE))
  (define opkt (make-vector PKT-SIZE))
  (define ent #f)
  (define vstr (make-bytes 256))
  (define accmode (if (eq? operation COUNT-SCAN) ACCREAD ACCWRITE))
  (define result SUCCESS)
  ;;(dprintf "%s %ld:%ld %.*s::%.*s\n" 'BT-SCAN (SEG:ID (HAN:SEG han)) (HAN:ID han) (max 0 len1) kstr1 (max 0 len2) kstr2)
  (cond
   ((key-error? kstr1 len1 -2 'bt-scan-1) KEYERR)
   ((key-error? kstr2 len2 -1 'bt-scan-2) KEYERR)
   ((han-error? han 'bt-scan) ARGERR)
   ((and (eq? operation MODIFY-SCAN) (not func))
    (edprintf "%s: MODIFY-SCAN requires func be specified\n" 'BT-SCAN)
    ARGERR)
   (else
    (set! ent (chain-find-ent han accmode kstr1 len1 pkt))
    (cond
     ((and ent (blk:find-pos (ENT:BLK ent) kstr2 len2 opkt))
      (cond
       ((eq? operation COUNT-SCAN)	;here we deal with a copy of ent
	(let ((nent (allocate-ent)))	;to avoid ACCREAD contention.
	  (ent-copy! nent ent)
	  (release-ent! ent accmode)	;accmode = ACCREAD here.
	  (set! result (chain-scan nent operation pkt opkt kstr1
				   func long-tab vstr respkt (HAN:WCB han)))
	  (recycle-ent! nent)))
       (else
	(set! result (chain-scan ent operation pkt opkt kstr1 func long-tab vstr respkt (HAN:WCB han)))
	(release-ent! ent accmode)
	(cond ((> result 0)		; check for MODIFY special case
	       (set! result (bt-put han kstr1 (PKT:SKEY-LEN respkt) vstr result))
	       (cond ((= result SUCCESS)
		      (PKT:SET-SKEY-COUNT! respkt (+ (PKT:SKEY-COUNT respkt) 1))
		      (PKT:SET-SKEY-LEN! respkt (increment-string kstr1 (PKT:SKEY-LEN respkt) 256))
		      (set! result NOTPRES)))))))
      (if (and (= result NOTPRES)	; ie, is there more to do?
	       (not (= 0 blk-limit)))
	  (bt-scan han operation kstr1 (PKT:SKEY-LEN respkt)
		   kstr2 len2 func long-tab respkt (- blk-limit 1))
	  result))
     (else
      (if ent (release-ent! ent accmode))
      (set! rem-fct (+ 1 rem-fct))
      UNKERR)))))

;; this function increments a string lexicographically
(define (increment-string str len maxlen)
  (cond ((< len maxlen)
	 (byte-set! str len 0)
	 (+ len 1))
	(else (let ((oldval (byte-ref str (- len 1))))
		(byte-set! str (- len 1) (+ 1 oldval))
		len))))

;;; Each call to CHAIN-SCAN scans
;;; all the keys within the specified range WITHIN block ENT.
;;; If the scan actually reaches  the end of range, it sets SKEY-LEN=0
;;; and returns SUCCESS. If there's more to the range,
;;; it sets KEY-STR to the key to continue deleting
;;; from (ie, the split key of ENT), SKEY-LEN to its length, and
;;; returns NOTPRES (NOTDONE). The caller must then call CHAIN-FIND
;;; to find the START and END keys and call again.
(define (chain-scan ent operation pkt opkt key-str func long-tab vstr respkt wcb)
  (let ((blk (ENT:BLK ent))
	(result SUCCESS))
					; check for special case of
					; unconditional delete of entire block
    (cond ((and (eq? operation REM-SCAN)
		(not func)
		(> (PKT:MATCH-POS opkt) (PKT:MATCH-POS pkt))
		(= (PKT:MATCH-POS pkt) BLK-DATA-START)
		(at-split-key-pos? blk (PKT:MATCH-POS opkt)))
	   ;;(dprintf "%s: Udelete(blk %d)\n" 'CHAIN-SCAN (BLK:ID blk))
	   (let ((key-len (recon-this-key blk (PKT:MATCH-POS opkt) ; delete data
					  key-str 0 256)))
	     (subbytes-move! key-str 0 key-len blk (+ BLK-DATA-START 2))
	     (SET-FIELD-LEN! blk (+ BLK-DATA-START 1) key-len)
	     (BLK:SET-END! blk (+ BLK-DATA-START 2 key-len)))
	   (PKT:SET-SKEY-COUNT! respkt (+ (PKT:SKEY-COUNT respkt) 1)) ; estimate only!
	   (set! rem-ct (+ 1 rem-ct))
	   (ENT:SET-DTY! ent #t)
	   (PKT:SET-MATCH-POS! opkt BLK-DATA-START))
	  (else			  ; else scan/delete/modify a subrange
	   (let ((oldct (PKT:SKEY-COUNT respkt))
		 (ckstr (make-bytes 256))
		 (clen 0))
	     (if func
		 (set! clen (recon-this-key blk (PKT:MATCH-POS pkt) ckstr 0 256)))
	     (PKT:SET-MATCH-TYPE! pkt MATCH) ; by definition
	     (set! result
		   (scan-loop (ENT:BLK ent) operation pkt opkt func long-tab respkt
			      ckstr clen vstr (SEG:BSIZ (ENT:SEG ent))))
	     (if (and (not (eq? operation COUNT-SCAN))
		      (> (PKT:SKEY-COUNT respkt) oldct))
		 (ENT:SET-DTY! ent #t)))
	   ))
					; delete blk if empty
    (if (and (eq? operation REM-SCAN)
	     (BLK-EMPTY? blk)
	     (not (END-OF-CHAIN? blk)))
	(del:delete-bck ent)
	(if (ENT:DTY? ent)
	    (if (or (and (eq? operation REM-SCAN)
			 (or (logtest WCB-SAR wcb)
			     (> (BLK:LEVEL blk) LEAF)))
		    (and (eq? operation MODIFY-SCAN) (logtest WCB-SAP wcb)))
		(ents:ent-write ent))))
					;further scanning needed?
    (cond ((not (= result SUCCESS))
	   (PKT:SET-SKEY-LEN! respkt (recon-this-key blk (PKT:MATCH-POS pkt)
						     key-str 0 256))
	   ;;(dprintf "%s: returning result %d\n" 'CHAIN-SCAN result)
	   result)
	  ((and (eq? (PKT:MATCH-TYPE opkt) PASTEND)
		(not (END-OF-CHAIN? blk)))
	   (PKT:SET-SKEY-LEN! respkt (recon-this-key blk (PKT:MATCH-POS pkt)
						     key-str 0 256))
	   ;;(dprintf "%s: new starting key len=%d\n" 'CHAIN-SCAN (PKT:SKEY-LEN respkt))
	   NOTPRES)
	  (else
	   (PKT:SET-SKEY-LEN! respkt 0)
	   SUCCESS))))

;; SCAN-LOOP returns SUCCESS if it reaches the end of the range,
;; else an ERROR code if terminated before that point, either
;; by an error or by FUNC returning TERMINATED.
;; SCAN-LOOP returns a value>0 to signal the case of
;; a MODIFY that requires a block-split. That value is the
;; length of the new value (which must be >0 to have caused an
;; increase in block size). SCAN-LOOP NEVER returns NOTPRES.
;; Note that (PKT:MATCH-POS pkt) is always the current scan point.

(define (scan-loop blk operation pkt opkt func long-tab respkt
		   ckstr clen vstr blksize)
  ;;  (dprintf "%s called: blk %d pos %d\n" 'SCAN-LOOP (blk:id blk) (PKT:MATCH-POS pkt))
  (if (> (PKT:MATCH-POS opkt) (PKT:MATCH-POS pkt))
      (let ((old-bend (BLK:END blk))
	    (next-pos (NEXT-CNVPAIR blk (PKT:MATCH-POS pkt)))
	    (result SUCCESS))
	(if func
	    (let* ((vpos (next-field blk (+ 1 (PKT:MATCH-POS pkt))))
		   (vlen (FIELD-LEN blk vpos)))
	      (subbytes-move! blk (+ vpos 1) (+ vpos vlen 1) vstr 0)
	      (set! result (func ckstr clen vstr vlen long-tab))))
	(cond ((>= result SUCCESS)	; ie, if (= result SUCCESS)
	       (cond ((eq? operation REM-SCAN)
		      (blk:remove-key-and-val blk (PKT:MATCH-POS pkt) blksize)
		      (PKT:SET-SKEY-COUNT! respkt (+ (PKT:SKEY-COUNT respkt) 1))
		      (set! rem-ct (+ 1 rem-ct))
		      (cond
		       ((= (PKT:MATCH-POS opkt) next-pos)
			(PKT:SET-MATCH-POS! opkt (PKT:MATCH-POS pkt)))
		       (else
			(PKT:SET-MATCH-POS! opkt (- (PKT:MATCH-POS opkt)
						(- old-bend (BLK:END blk))))))
		      (set! next-pos (PKT:MATCH-POS pkt)))
		     ((eq? operation COUNT-SCAN)
		      (PKT:SET-SKEY-COUNT! respkt (+ (PKT:SKEY-COUNT respkt) 1))
		      (PKT:SET-MATCH-POS! pkt next-pos))
		     ((blk:change-existing-value blk (PKT:MATCH-POS pkt)
					     ckstr clen vstr result blksize)
		      (PKT:SET-SKEY-COUNT! respkt (+ (PKT:SKEY-COUNT respkt) 1))
		      (set! next-pos (- next-pos (- old-bend (BLK:END blk))))
		      (PKT:SET-MATCH-POS! opkt (- (PKT:MATCH-POS opkt)
					      (- old-bend (BLK:END blk))))
		      (PKT:SET-MATCH-POS! pkt next-pos)
		      (set! result SUCCESS))
		     (else
		      (dprintf "%s: hit modify special case\n" 'SCAN-LOOP))
		     ))
	      ((= result NOTPRES)       ; not deleting, just advance scan ptr
	       (PKT:SET-MATCH-POS! pkt next-pos)
	       ))
	(cond ((or (= result SUCCESS) (= result NOTPRES))
	       (cond (func		; update key to pass to FUNC
		      (set! clen (+ (field-len blk next-pos)
				    (field-len blk (+ 1 next-pos))))
		      (subbytes-move! blk (+ next-pos 2)
				       (+ next-pos 2 (field-len blk (+ 1 next-pos)))
				       ckstr (field-len blk next-pos))
		      ))
	       (scan-loop blk operation pkt opkt func long-tab respkt
			  ckstr clen vstr blksize))
	      (else result)))
      SUCCESS))
