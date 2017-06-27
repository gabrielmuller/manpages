;; "handle.scm" WB-tree File Based Associative String Data Base System.
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

(java:import "Ents" "Prev" "Stats" "Blink" "Del" "Scan" "Wbdefs"
	     "Wbsys" "SchlepRT" "Pkt" "Ent" "Han")

(define clever-cache-enable? #t)

(define (han-error? han caller)
  (cond ((not han)
	 (edprintf "%s: called with NULL handle.\n" caller)
	 #t)
	((not (HAN:SEG han))
	 (edprintf "%s: called with handle having NULL segment.\n" caller)
	 #t)
	(else #f)))

(define (key-error? key-str k-len xcpt caller)
  (cond ((= xcpt k-len) #f)
	((not (<= 0 k-len 255))
	 (edprintf
	  "%s: key-length, %d, was out of range 0 <= 255.\n" caller k-len)
	 #t)
	((and (positive? k-len) (eqv? 255 (byte-ref key-str 0)))
	 (edprintf "%s: first byte of key must not be 255.\n" caller)
	 #t)
	(else #f)))

;;@noindent
;;Note: most of the data-manipulating commands here
;;can return @var{notpres}, with the followng meanings:
;;@cindex notpres
;;
;;@multitable @columnfractions .2 .8
;;@item @code{bt-get}
;;@tab @var{key} was not found.
;;@item @code{bt-next}
;;@tab no @emph{next} @var{key} (eg, given @var{key} was last key).
;;@item @code{bt-prev}
;;@tab no @emph{prev} @var{key} (eg, given @var{key} was first key).
;;@item @code{bt-rem}
;;@tab @var{key} was not found.
;;@c @item @code{bt-rem-range}
;;@c @tab ??
;;@item @code{bt-put}
;;@tab @emph{unused} (could be symmetric with write).
;;@item @code{bt-write}
;;@tab @var{key} @emph{was} found, so no write was done.
;;@end multitable

;;@body @2 is a string of length @3.  @0 stores into the string @4 the
;;value associated with @2 in tree @1.  @0 returns the length of the
;;string stored into @4 or an error code.
(define (bt-get han key-str k-len ans-str)
  (define pkt (make-vector PKT-SIZE))
  (define ent #f)
;;;  (dprintf "%s %ld:%ld %.*s\n" 'BT-GET (SEG:ID (HAN:SEG han)) (HAN:ID han)
;;;	   (max 0 k-len) key-str)
  (cond
   ((key-error? key-str k-len 0 'bt-get) KEYERR)
   ((han-error? han 'bt-get) ARGERR)
   (else
    (set! ent (chain-find-ent han ACCREAD key-str k-len pkt))
    (cond
     ((not ent) (set! get-fct (+ 1 get-fct)) UNKERR)
     ((not (eq? (PKT:MATCH-TYPE pkt) MATCH))
      (set! get-ct (+ 1 get-ct))
      (release-ent! ent ACCREAD)
      NOTPRES)
     (else
      (let ((alen (get-this-val (ENT:BLK ent) (PKT:MATCH-POS pkt) ans-str)))
	(set! get-ct (+ 1 get-ct))
	(release-ent! ent ACCREAD)
	alen))))))

;;@body @2 is a string of length @3.  @0 stores into the string @4 the
;;next key after @2 in tree @1.  @0 returns the length of the string
;;stored into @4 or an error code.
(define (bt-next han key-str k-len ans-str)
  (define pkt (make-vector PKT-SIZE))
  (define ent #f)
;;;(dprintf "%s %ld:%ld %.*s\n" 'BT-NEXT (SEG:ID (HAN:SEG han)) (HAN:ID han)
;;;	   (max 0 k-len) key-str)
  (cond
   ((key-error? key-str k-len -2 'bt-next) KEYERR)
   ((han-error? han 'bt-next) ARGERR)
   (else
    (set! ent (chain-find-ent han ACCREAD key-str k-len pkt))
    (cond
     ((not ent)
      (set! next-fct (+ 1 next-fct)) UNKERR)
     (else
      (set! next-ct (+ 1 next-ct))
      (let ((res (chain-next ent key-str k-len ans-str pkt)))
	(if clever-cache-enable? (HAN:SET-LAST! han (PKT:BLK-TO-CACHE pkt)))
	res))))))

;;@body @2 is a string of length @3.  @0 stores into the string @4 the
;;last key before @2 in tree @1.  @0 returns the length of the string
;;stored into @4 or an error code.
(define (bt-prev han key-str k-len ans-str)
  (define pkt (make-vector PKT-SIZE))
  (define ent #f)
;;;(dprintf "%s %ld:%ld %.*s\n" 'BT-PREV (SEG:ID (HAN:SEG han)) (HAN:ID han)
;;;	   (max 0 k-len) key-str)
  (cond
   ((key-error? key-str k-len -1 'bt-prev) KEYERR)
   ((han-error? han 'bt-prev) ARGERR)
   (else
    (set! ent (chain-find-prev-ent han ACCREAD key-str k-len pkt))
    (and ent (set! ent (prev:prev-k-ent ent key-str k-len LEAF pkt)))
    (cond
     ((not ent) (set! prev-fct (+ 1 prev-fct)) UNKERR)
     (else
      (set! prev-ct (+ 1 prev-ct))
      (cond
       ((zero? (PKT:MATCH-POS pkt)) (release-ent! ent ACCREAD) NOTPRES)
       (else
	(let ((k-len2 (recon-this-key (ENT:BLK ent)
				      (PKT:MATCH-POS pkt) ans-str 0 256)))
	  (HAN:SET-LAST! han (ENT:ID ent))
	  (release-ent! ent ACCREAD)
	  k-len2))))))))

;;@body @2 is a string of length @3.  @0 stores into the string @4 the
;;value associated with @2 in tree @1; then removes that association
;;from tree @1.  @0 returns the length of the string stored into @4 or
;;an error code.
;;
;;If @4 is 0, @0 removes the @2 association from tree @1 and returns
;;@var{SUCCESS} if successful; an error code if not.
(define (bt-rem han key-str k-len ans-str)
  (define pkt (make-vector PKT-SIZE))
  (define ent #f)
;;;(dprintf "%s %ld:%ld %.*s\n" 'BT-REM (SEG:ID (HAN:SEG han)) (HAN:ID han)
;;;	   (max 0 k-len) key-str)
  (cond
   ((key-error? key-str k-len 0 'bt-rem) KEYERR)
   ((han-error? han 'bt-rem) ARGERR)
   (else
    (set! ent (chain-find-ent han ACCWRITE key-str k-len pkt))
    (cond
     ((not ent) (set! rem-fct (+ 1 rem-fct)) UNKERR)
     (else
      (set! rem-ct (+ 1 rem-ct))
      (let ((ans (del:chain-rem ent key-str k-len ans-str pkt (HAN:WCB han))))
	(release-ent! ent ACCWRITE)
	ans))))))

;;; To make possible bounded-time operation, @0 will purge at most
;;; BLK-LIMIT blocks at a time; passing BLK-LIMIT of -1 imposes no
;;; limit.

;;@body @2 must be a maximum-length (256 byte) string containing a key
;;@3 bytes long.  @4 is a string of length @5.
;;
;;@0 removes [@2 @dots{} @4) and their values.  If @4 <= @2 no
;;deletion will occur (even if @2 is found).  @0 returns SUCCESS if
;;the operation is complete, an error status code if not.
(define (bt-rem-range han key-str k-len key2-str k2-len)
  (define respkt (make-vector PKT-SIZE))
  (PKT:SET-SKEY-COUNT! respkt 0)
  (bt-scan han REM-SCAN key-str k-len key2-str k2-len #f #f respkt -1))

;;@body @2 is a string of length @3.  @4 is a string of length @5.  @0
;;makes the value associated with @2 be @4 in tree @1.  @0 returns a
;;status code for the operation.
(define (bt-put han key-str k-len val-str v-len)
  (define ent #f)
  (define pkt (make-vector PKT-SIZE))
  ;;(dprintf "%s %ld:%ld %.*s %.*s\n" 'BT-PUT (SEG:ID (HAN:SEG han)) (HAN:ID han) (max 0 k-len) key-str v-len val-str)
  (cond
   ((key-error? key-str k-len 0 'bt-put) KEYERR)
   ((han-error? han 'bt-put) ARGERR)
   ((not (<= 0 v-len 255)) ARGERR)
   (else
    (set! ent (chain-find-ent han ACCWRITE key-str k-len pkt))
    (cond
     ((not ent) UNKERR)
     (else
      (let ((res?
	     (chain-put ent key-str k-len val-str v-len pkt #f (HAN:WCB han))))
	(cond (res?
	       (if clever-cache-enable?
		   (HAN:SET-LAST! han (PKT:BLK-TO-CACHE pkt)))
	       (set! put-ct (+ 1 put-ct))
	       SUCCESS)
	      (else
	       (set! put-fct (+ 1 put-fct))
	       UNKERR))))))))

;;@body @2 is a string of length @3.  @4 is a string of length @5.  If
;;@1 currently contains an association for @2, then @0 does not modify
;;the tree and returns the @var{notpres} status code.
;;
;;Otherwise, @0 makes the value associated with @2 be @4 in tree @1.
;;@0 returns a status code for the operation.
(define (bt-write han key-str k-len val-str v-len)
  (define ent #f)
  (define pkt (make-vector PKT-SIZE))
  (cond
   ((key-error? key-str k-len 0 'bt-write) KEYERR)
   ((han-error? han 'bt-write) ARGERR)
   ((not (<= 0 v-len 255)) ARGERR)
   (else
    (set! ent (chain-find-ent han ACCWRITE key-str k-len pkt))
    (cond
     ((not ent) UNKERR)
     ((eq? (PKT:MATCH-TYPE pkt) MATCH) ;;DTY has not been set.
      (release-ent! ent ACCWRITE)
      NOTPRES)
     (else
      (let ((res?
	     (chain-put ent key-str k-len val-str v-len pkt #f (HAN:WCB han))))
	(cond (res?
	       (if clever-cache-enable?
		   (HAN:SET-LAST! han (PKT:BLK-TO-CACHE pkt)))
	       (set! put-ct (+ 1 put-ct))
	       SUCCESS)
	      (else
	       (set! put-fct (+ 1 put-fct))
	       UNKERR))))))))
