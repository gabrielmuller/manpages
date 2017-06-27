;; "del.scm" WB-tree File Based Associative String Data Base System.
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

(java:import "Ents" "Prev" "Stats" "Blink" "Wbdefs" "Wbsys" "Blk"
	     "SchlepRT" "Pkt" "Seg" "Ent")

(define del:defer-block-deletes? #f)

;; fixes:
;; 1. 1/22 blk-delete should not be called if END-OF-CHAIN
;; 2.      IND-REM-V&K needed to return B-POS
;; 3.      CHAIN-KEY-REM also neede to check for being already at root level
;; 4. 1/23 fixed BLK-DELETE? to set accmode to ACCNONE while calling PREV-BLK-ENT!
;; 5.      fixed CHAIN-KEY-REM to give error message if key not found in index

;; BLK-DELETE assumes caller has ACCWRITE to blk and will
;; release if after blk-delete returns

;; sorry, waiting on parent-update is losing since
;; deletes that  lock the entire path to the root will almost certainly
;; NEVER succeed!

(define (del:delete-bck ent)
  (define blk (ENT:BLK ent))
  (define win? (not del:defer-block-deletes?))
;;;  (dprintf "%s called, blk=%ld:%ld\n" 'BLK-DELETE (SEG:ID (ENT:SEG ent)) (ENT:ID ent))
  (cond
   (win?
					; 1. get and lock PREV
    (ents:ent-update-access? ent ACCWRITE ACCNONE) ; KLUGE!!
    (let ((prent (prev-blk-ent ent (BLK:LEVEL blk))))
      (set! win? (ents:ent-update-access? ent ACCNONE ACCWRITE)) ;need to back out if #f
      (and win? prent		      ; if no PRENT, no prev to unlink
	   (set! win? (ents:ent-update-access? prent ACCREAD ACCWRITE)))
					; TBD: double-check that PRENT is still
					;PREV of ENT; if not, retry PREV-BLK
      (set! win? (and win? (= 1 (ENT:REF ent)))) ; dont delete blk w/pending parent-update
      (cond
       (win?					     ; 2. lock parent
	(if (not (at-root-level? (ENT:SEG ent) blk)) ; no parents to fix!
	    (let ((skey-pos (split-key-pos blk)))
	      (and
	       (positive? skey-pos)
	       (let* ((top-num (BLK:TOP-ID blk))
		      (seg (ENT:SEG ent))
		      (level (BLK:LEVEL blk))
		      (key-str (make-bytes 256))
		      (k-len (recon-this-key blk skey-pos key-str 0 256)))
					; 2: fix parent
		 (set! win?
		       (parent-delete-update? seg top-num level (ENT:ID ent)
					      key-str k-len))))))
					; if all goes ok, we can make the mods
	(set! win? (and win? (= 1 (ENT:REF ent))))
	(cond
	 (win?			       ; 3-4:  unlink block from chain
	  (cond
	   (prent (BLK:SET-NXT-ID! (ENT:BLK prent) (BLK:NXT-ID blk))
		  (ENT:SET-DTY! prent #t)
		  (ents:ent-write prent)))
	  (set! win? (blk-free ent))
	  (if (not win?)		; 5 reclaim block
	      (edprintf "%s: could not free %ld:%ld\n"
			'BLK-DELETE (SEG:ID (ENT:SEG ent)) (ENT:ID ent)))))))
      (if prent (release-ent! prent (ENT:ACC prent))))))
  (cond (win? (set! block-deletes (+ block-deletes 1)))
	(else (set! deferred-deletes (+ 1 deferred-deletes))
	      (dprintf "Can't delete block %lu\n" (ENT:ID ent))))
  win?)

;;; return #t if operation was succsessful; #f if not
;;; Note the deletion of blk OLD-ID by removing its KEY+ID from parent.
;;; Note this routine does not check if the key has already been
;;; (perhaps by another process) deleted from the parent.

(define (parent-delete-update? seg top-id level old-id key-str k-len)
  (define pkt (make-vector PKT-SIZE))
  (define ans -1)
  (define ans-str (make-bytes 4))      ;this is for index blocks only.
;;;  (dprintf "%s called, blk=%ld:%ld, level=%d, key=%.*s\n"
;;;	   'PARENT-DELETE-UPDATE? (SEG:ID seg) old-id level k-len key-str)
  (let ((ent (find-ent (get-ent seg top-id ACCNONE) (+ 1 level) -1 key-str k-len)))
    (cond ((not ent) #f)
	  ((ents:ent-update-access? ent ACCREAD ACCWRITE)
	   (set! ent (chain-find ent ACCWRITE key-str k-len pkt)))
	  (else (release-ent! ent ACCREAD)
		(set! ent #f)))
    (cond (ent (set! ans (del:chain-rem ent key-str k-len ans-str pkt WCB-SAR))
	       (if (>= ans 0)
		   (if (not (= old-id (str2long ans-str 0)))
		       (edprintf "%s: bad value %lu in deleted down pointer %lu told\n"
				 'PARENT-DELETE-UPDATE?
				 (str2long ans-str 0) old-id)))
	       (release-ent! ent ACCWRITE)))
    (cond
     ((or ent (>= ans 0)) #t)
     (else
      (wdprintf "%s blk=%ld:%ld, level=%d, key=%.*s\n"
		'PARENT-DELETE-UPDATE? (SEG:ID seg) old-id level k-len key-str)
      #f))))

;; called with ACCREAD on ENT, releases ent before returning
;;; DEL:CHAIN-REM can call BLK-DELETE
;;;   BLK-DELETE calls BLK-FREE
;;;     BLK-FREE calls AMNESIA-ENT! which sets the segment number to -1
;;; DEL:CHAIN-REM calls RELEASE-ENT!
;;;; Chad Gadya!

(define (del:chain-rem ent key-str k-len ans-str pkt wcb)
;;;  (dprintf "%s called, blk=%ld:%ld, key=%.*s\n" 'DEL:CHAIN-REM
;;;	   (SEG:ID (ENT:SEG ent)) (ENT:ID ent) k-len key-str)
  (cond ((eq? (PKT:MATCH-TYPE pkt) MATCH)
	 (let ((alen SUCCESS))
	   (if ans-str
	       (set! alen
		     (get-this-val (ENT:BLK ent) (PKT:MATCH-POS pkt) ans-str)))
	   (blk:remove-key-and-val (ENT:BLK ent)
				   (PKT:MATCH-POS pkt)
				   (SEG:BSIZ (ENT:SEG ent)))
	   (ENT:SET-DTY! ent #t)
	   (cond
	    ((and (blk-empty? (ENT:BLK ent))
		  (not (END-OF-CHAIN? (ENT:BLK ent))))
	     (del:delete-bck ent))
	    (else
;;;		 (dprintf "%s: blk=%d nonleaf=%d SAR=%d\n" 'DEL:CHAIN-REM
;;;			  (BLK:ID (ENT:BLK ent)) (> (BLK:LEVEL (ENT:BLK ent)) LEAF)
;;;			  (logtest WCB-SAR wcb))
	     (if (or (logtest WCB-SAR wcb)
		     (> (BLK:LEVEL (ENT:BLK ent)) LEAF))
		 (ents:ent-write ent))))
	   alen))
	(else
;;;	      (dprintf "%s: key %.*s not found in blk %d\n" 'DEL:CHAIN-REM
;;;		       k-len key-str (ENT:ID ent))
	 NOTPRES)))
