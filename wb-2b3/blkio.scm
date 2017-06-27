;; "blkio.scm" WB-tree File Based Associative String Data Base System.
;; Copyright (C) 1991, 1992, 1993, 2000, 2003, 2008, 2009 Free Software Foundation, Inc.
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

(require 'i/o-extensions)

(define io-diag? #f)

(define (blkio:init!) #f)

(define (blkio:final!) #f)

(define (blkio:create-file name bsiz)
  (open-file name "w+"))

(define (blkio:open-modify-file name bsiz)
  (open-file name open_both))

(define (blkio:open-read-only-file name bsiz)
  (open-input-file name))

(define (blkio:port-open? fd mutable?)
  (if mutable? (input-port? fd) (output-port? fd)))

(define (blkio:file-close! fd bsiz mutable?)
  (close-port fd))

(define (blkio:flush-to-file! fd metadata?)
  (force-output fd))

(define (blkio:read fd blk bsiz blknum)
  (file-position fd (* bsiz blknum))
  (cond ((= bsiz (subbytes-read! blk 0 bsiz fd))
	 (if io-diag? (dprintf "rd:%*s%c %10ld\n"
			      (+ 1 (- (BLK:LEVEL blk) LEAF)) ""
			      (BLK:TYP blk) blknum))
	 (if (BLK:TYP? blk FRL-TYP)
	     (set! read-fl-ct (+ read-fl-ct 1))
	     (set! read-ct (+ read-ct 1)))
	 #t)
	(else (edprintf " couldn't read blk %ld\n" blknum)
	      #f)))

(define (blkio:write fd blk bsiz blknum)
  (file-position fd (* bsiz blknum))
  (cond ((= bsiz (subbytes-write blk 0 bsiz fd))
	 (if io-diag? (dprintf "wr:%*s%c %10ld\n"
			      (+ 1 (- (BLK:LEVEL blk) LEAF)) ""
			      (BLK:TYP blk) blknum))
	 (if (BLK:TYP? blk FRL-TYP)
	     (set! write-fl-ct (+ write-fl-ct 1))
	     (set! write-ct (+ write-ct 1)))
	 #t)
	(else (edprintf " couldn't write blk %ld\n" blknum)
	      #f)))

;;; Don't need to write block when extending.
(define (blkio:file-extend fd bsiz blknum)
  (define worked? (file-position fd (* bsiz blknum)))
;;;(cond ((= bsiz (subbytes-write blk 0 bsiz fd))
  (cond (worked?
	 (if io-diag?
	     (dprintf "Extending file to block %ld\n" blknum))
	 #t)
	(else
	 (edprintf " couldn't extend file to block %ld\n" blknum)
	 #f)))
