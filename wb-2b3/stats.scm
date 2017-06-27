;; "stats.scm" WB-tree File Based Associative String Data Base System.
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

(java:import "Ents" "Seg" "Blink" "Del" "Handle" "Wbdefs" "Wbsys" "Blk" "Ent")

;;; PERFORMANCE STATISTICS

(define next-ct 0)
(define next-fct 0)
(define prev-ct 0)
(define prev-fct 0)
(define get-ct 0)
(define get-fct 0)
(define put-ct 0)
(define put-fct 0)
(define rem-ct 0)
(define rem-fct 0)
(define ge-ct 0)
(define ge-fct 0)
(define tge-ct 0)
(define tge-fct 0)
(define tce-ct 0)
(define tce-fct 0)

(define chains-to-next 0)		; counters for special-case occurrences
(define deferred-inserts 0)
(define split-index-inserts 0)
(define index-screw-case  0)
(define block-splits 0)
(define block-deletes 0)
(define deferred-deletes 0)
(define dir-dty-ct 0)

(define read-ct 0)			; buffer i/o counters
(define write-ct 0)
(define read-fl-ct 0)
(define write-fl-ct 0)
(define flush-ct 0)

(define (clear-stats)
  (set! next-ct 0)
  (set! next-fct 0)
  (set! prev-ct 0)
  (set! prev-fct 0)
  (set! get-ct 0)
  (set! get-fct 0)
  (set! put-ct 0)
  (set! put-fct 0)
  (set! rem-ct 0)
  (set! rem-fct 0)
  (set! ge-ct 0)
  (set! ge-fct 0)
  (set! tge-ct 0)
  (set! tge-fct 0)
  (set! tce-ct 0)
  (set! tce-fct 0)

  (set! chains-to-next 0)
  (set! deferred-inserts 0)
  (set! split-index-inserts 0)
  (set! index-screw-case  0)
  (set! block-splits 0)
  (set! block-deletes 0)
  (set! deferred-deletes 0)
  (set! dir-dty-ct 0)

  (set! read-ct 0)
  (set! write-ct 0)
  (set! read-fl-ct 0)
  (set! write-fl-ct 0)
  (set! flush-ct 0)
  SUCCESS)

(define (cstats)
  (stats)
  (clear-stats))

(define (stats)
  (dprintf "\n" )
  (dprintf "OPERATIONS SUMMARY:\n" )
  (dprintf
	   "            NEXT     PREV      GET      PUT      REM   GETENT   CACHE    CACHE\n")
  (dprintf
	   "                                                                AVAIL?   VALID?\n")
  (dprintf "   succ %8lu %8lu %8lu %8lu %8lu %8lu %8lu %8lu\n"
	   next-ct prev-ct get-ct put-ct rem-ct ge-ct tge-ct tce-ct)
  (dprintf "   fail %8lu %8lu %8lu %8lu %8lu %8lu %8lu %8lu\n\n"
	   next-fct prev-fct get-fct put-fct rem-fct ge-fct tge-fct tce-fct)

  (dprintf "INTERNAL OPERATIONS SUMMARY:\n" )
  (dprintf "   chains-to-next = %d\n" chains-to-next)
  (dprintf "   %d block splits, %d deferred parent updates, %d undone\n"
	   block-splits deferred-inserts  deferred-inserts)
  (dprintf "   %d split index inserts; %d index-insert screw cases \n"
	   split-index-inserts index-screw-case )
  (dprintf "   %d block deletes; %d deferred block deletes\n\n"
	   block-deletes deferred-deletes)

  (dprintf "I/O SUMMARY: %lu READS, %lu WRITES, %lu FLUSH (of dirty bufs) calls.\n"
	   read-ct write-ct flush-ct)
  (dprintf "             %lu FREELIST READS, %lu FREELIST WRITES.\n"
	   read-fl-ct write-fl-ct)
  (dprintf "             %lu DIRS LEFT DTY.\n"
	   dir-dty-ct)
  (let ((ops (max (+ next-fct prev-fct get-fct put-fct rem-fct
		 next-ct prev-ct get-ct put-ct rem-ct ) 1)))
    (dprintf "   AVG BLKS REFERENCED PER OPERATION: %7lu%%\n"
	   (quotient (* 100 (+ ge-ct tce-ct)) ops))
    (dprintf "   AVG DISK I/Os PER OPERATION: %7lu%%; "
	     (quotient (* 100 (+ read-ct write-ct read-fl-ct write-fl-ct)) ops))
    (dprintf "   WRITE/READ RATIO: %7lu%%\n"
	     (quotient (* 100 (+ write-ct write-fl-ct)) (max 1 (+  read-ct read-fl-ct))))
    (if (> put-ct 0)
	(dprintf "   AVG DISK WRITES PER PUT: %7lu%%\n"
		 (quotient (* 100 write-ct) put-ct)))
    )
  (dprintf "\n" )
  (dprintf "MODES IN EFFECT:\n" )
  (dprintf "%lu buffers [hashed over %d buckets]; blksize (max)=%d; FLC-LEN=%d\n"
	   num-ents-ct NUM-BUKS blk-size FLC-LEN)
  (dprintf "defer-insert-updates?= %2s; defer-block-deletes=%2s\n"
	   (if defer-insert-updates? "#t" "#f") (if del:defer-block-deletes? "#t" "#f") )
  (dprintf "cache-ent-enable?=%2s; clever-cache-enable?=%2s\n"
	   (if cache-ent-enable? "#t" "#f") (if clever-cache-enable? "#t" "#f") )
  (dprintf "\n" )
  SUCCESS)

(define (show-buffer! ent)
  (dprintf "SEG = %ld ID= %lu ACC= %d REF= %d DTY= %d AGE= %d\n"
	   (SEG:ID (ENT:SEG ent)) (ENT:ID ent) (ENT:ACC ent) (ENT:REF ent) (ENT:DTY? ent) (ENT:AGE ent)))

(define buf-verbose? #t)

(define (show-buffer-1 ent)
  (cond ((or buf-verbose? (> (SEG:ID (ENT:SEG ent)) -1))
	 (dprintf " %3ld:%-6lu %6lu %8d %6u %7d %3d %4d"
		  (SEG:ID (ENT:SEG ent))
		  (ENT:ID ent)
		  (HASH2INT (SEG:ID (ENT:SEG ent)) (ENT:ID ent))
		  (ENT:ACC ent)
		  (ENT:REF ent)
		  0
		  (ENT:DTY? ent)
		  (ENT:AGE ent))
	 (if (> (SEG:ID (ENT:SEG ent)) -1)
	     (dprintf " %5d %4c\n"
		      (- (BLK:LEVEL (ENT:BLK ent)) LEAF)
		      (BLK:TYP (ENT:BLK ent)))
	     (dprintf "\n"))
	 ))
  SUCCESS)

(define (show-buffers)
  (dprintf "\n" )
  (dprintf " SEG:ID        BUK      ACC    REF READERS DTY  AGE LEVEL TYPE\n")
  (do-seg-buffers
   #f
   #-(or SCM2JAVA SCM2CS) show-buffer-1
   #+SCM2JAVA (get-method "wb.Stats" "showBuffer1" (arg-class "Ent"))
   #+SCM2CS "showBuffer1"
   ))

(define (sb) (show-buffers))
