;;; schleprt.scm: Runtime support for Schlep subset of Scheme
;;; Copyright (C) 1991-2006 Aubrey Jaffer and Radey Shouman
;;; Copyright (C) 2008, 2009 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;;;	  http://people.csail.mit.edu/jaffer/Schlep

(require 'stdio)
(require 'byte)
;;; Stubs for schlep
(define (pragma.h . strs) #f)
(define (pragma.c . strs) #f)
(define (pragma.java . strs) #f)
(define (java:import . strs) #f)
;;; This is where all diagnostic and error messages will appear
(define diagout stderr)
;;(define diagout (current-output-port))
(define (dprintf . args)
  (apply fprintf diagout args)
  (force-output diagout))
(define (edprintf . args)
  (fprintf diagout ">>>>ERROR<<<< ")
  (apply fprintf diagout args)
  (force-output diagout))
(define (wdprintf . args)
  (fprintf diagout "WARNING: ")
  (apply fprintf diagout args)
  (force-output diagout))
;;; sop to C
(define (free! x)
  (if (not x) (edprintf "%s: object already freed\n" 'FREE!)))
