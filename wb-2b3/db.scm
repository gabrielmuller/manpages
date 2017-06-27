;; "db.scm" WB-tree File Based Associative String Data Base System.
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

;;;; Advertised Java and C# API

(pragma.c "#include \"wbsys.h\"")

(java:import "SchlepRT" "Pkt" "Wbsys" "Blink" "Segs" "Handle" "Ents"
	     "Wbdefs" "Han" "Scan")

;;@
;;All of the methods listed here which take byte-array arguments can
;;also take string arguments, which get converted to UTF-8
;;byte-arrays.

(define (create-bt seg typ wcb)
  (let* ((a-han (MAKE-HAND))
	 (ans (bt-create seg typ a-han wcb)))
    (if (err? ans) #f a-han)))

(define (open-bt seg blknum wcb)
  (let* ((a-han (MAKE-HAND))
	 (ans (bt-open seg blknum a-han wcb)))
    (if (err? ans) #f a-han)))

;;@body
;;Closes @var{han}
(define (close-bt! han)
  (bt-close han)
  (free! han))

;;@
;;For @code{create-db} and @code{open-db}, the implicit @var{WCB}
;;argument is the combination of @samp{WCB-SAP} and @samp{WCB-SAR}.

;;@body
;;Returns a B-tree whose name has been entered in the root directory
;;if successful; otherwise null.
;;
;;@var{typ} should be either
;;
;;@itemize @bullet
;;@item @code{'D'} (directory) or
;;@item @code{'T'} (regular tree).
;;@end itemize
;;
;;B-trees with @var{typ} @code{#\D} which are pointed to by special
;;entries in the root block (1) protect all their special entries from
;;garbage collection by the @code{wbcheck} program.  @code{'T'} is for
;;regular (data) arrays.
(define (create-db seg typ name-str)
  (define tmp-str (make-bytes 256))
  (define a-han (create-bt seg typ 0))
  (define d-han (open-bt seg 1 (+ WCB-SAP WCB-SAR)))
  (cond ((or (not a-han) (not d-han)) #f)
	(else
	 (long2str! tmp-str 1 (HAN:ID a-han))
	 (byte-set! tmp-str 0 4)
	 (bt-put d-han name-str (bytes-length name-str) tmp-str 5)
	 (close-bt! d-han)
	 a-han)))

;;@body
;;Returns the B-tree whose name has been entered in the root directory;
;;or null if not found.
(define (open-db seg name-str)
  (define tmp-str (make-bytes 256))
  (define d-han (open-bt seg 1 (+ WCB-SAP WCB-SAR)))
  (if (not d-han)
      #f
      (let ((tlen (bt-get d-han name-str (bytes-length name-str) tmp-str)))
	(close-bt! d-han)
	(cond ((err? tlen) #f)
	      ((eqv? tlen 5) (open-bt seg (str2long tmp-str 1) 0))
	      (else #f)))))

;;@body
;;@var{k} is the number of dirty block buffers to write to disk;
;;@var{attempts} is the number of times to try.  Note that blocks in any
;;segment may be written by @code{flush-ents}.  @code{flush-ents}
;;returns the number of blocks written.
(define (flush-ents attempts k)
  (ents-flush attempts k))

;;@section Record Operations

;;; get returns a string of the value or #f

;;@body
;;@var{han} is a handle to an open bt.  @var{key} is a string less
;;than 255.B in length.
;;
;;@code{bt:get} returns a string of the value associated with @var{key}
;;in the bt which @var{han} is open to.  @code{bt:get} returns null if
;;@var{key} is not associated in the bt.
(define (bt:get han key)
  (let* ((tmp-str (make-bytes 256))
	 (tlen (bt-get han key (bytes-length key) tmp-str)))
    (if (err? tlen) #f (subbytes tmp-str 0 tlen))))

;;; next returns a string of the next key or #f if at end.
;;; (bt:next blk #f) returns the first key.

;;@body
;;@var{han} is a handle to an open bt.  @var{key} is a string less
;;than 255.B in length.
;;
;;@code{bt:next} returns the next @var{key} in bt @var{han} or null if
;;none.
(define (bt:next han key)
  (define tmp-str (make-bytes 256))
  (let ((tlen (if key
		  (bt-next han key (bytes-length key) tmp-str)
		  (bt-next han no-byts START-OF-CHAIN tmp-str))))
    (if (err? tlen) #f (subbytes tmp-str 0 tlen))))

;;; prev returns a string of the previous key or #f if at end.
;;; (bt:prev blk #f) returns the last key.

;;@body
;;@var{han} is a handle to an open bt.  @var{key} is a string less
;;than 255.B in length.
;;
;;@code{bt:prev} returns the previous @var{key} in bt @var{han} or null
;;if none.
(define (bt:prev han key)
  (define tmp-str (make-bytes 256))
  (if (and key (zero? (bytes-length key)))
      (edprintf "%s of \"\".\n" 'bt:prev))
  (let ((tlen (if key
		  (bt-prev han key (bytes-length key) tmp-str)
		  (bt-prev han no-byts END-OF-CHAIN tmp-str))))
    (if (err? tlen) #f (subbytes tmp-str 0 tlen))))

;;; put adds an key value pair to the database whose root is blk

;;@body
;;@var{han} is a handle to an open, mutable bt.  @var{key} and
;;@var{val} are strings less than 255.B in length.
;;
;;@code{bt:put!} associates @var{key} with @var{val} in the bt
;;@var{han}.  A status code is returned.
(define (bt:put! han key val-str)
  (bt-put han
	  key (bytes-length key)
	  val-str (bytes-length val-str)))

;;; rem! removes key and value.  returns #t if found, #f if not.

;;@body
;;@var{han} is a handle to an open, mutable bt.  @var{key} is a
;;string less than 255.B in length.
;;
;;@code{bt:rem!} removes @var{key} and it's associated value from bt
;;@var{han}.
(define (#+SCHLEP bt:del #-SCHLEP bt:rem! han key)
  (success? (bt-rem han key (bytes-length key) #f)))

;;@section Mutual Exclusion
;;
;;These 2 calls can be used for locking and synchronizing processes.

;;@body
;;Associates @var{key} with @var{val} in the bt @var{han} only if
;;@var{key} was previously empty.  Returns true for success, false for
;;failure.
(define (#+SCHLEP bt:insert #-SCHLEP bt:put han key val-str)
  (success? (bt-write han
		      key (bytes-length key)
		      val-str (bytes-length val-str))))

;;@body
;;Removes @var{key} and it's associated value from bt @var{han} only
;;if @var{key} is present.  Returns @var{key}'s value for success, null
;;for failure (not present).
(define (bt:rem han key)
  (let* ((tmp-str (make-bytes 256))
	 (tlen (bt-rem han key (bytes-length key) tmp-str)))
    (if (err? tlen) #f (subbytes tmp-str 0 tlen))))

;;@section Multiple Operations

;;@body
;;Removes @var{key}s (and their associated values) between (including)
;;@var{key1} and (not including) @var{key2} from bt @var{han}.  A
;;status code is returned.
(define (#+SCHLEP bt:delete #-SCHLEP bt:rem* han key key2)
  (define tmpstr (make-bytes 256))
  (define klen (if key (bytes-length key) START-OF-CHAIN))
  (define klen2 (if key2 (bytes-length key2) END-OF-CHAIN))
  (if (positive? klen) (subbytes-move! key 0 klen tmpstr 0))
  (bt-rem-range han tmpstr klen key2 klen2))

;;; TBD, create second version of bt:scan which returns count (no blklimit).

;;@body
;;@code{btScan} scans all keys in the range [@var{kstr1}..@var{kstr2}),
;;performing one of several functions:
;;
;;@multitable @columnfractions .2 .15 .66
;;@item @var{operation}
;;@tab @var{func}
;;@tab RESULT
;;@item COUNT-SCAN
;;@tab NIL
;;@tab counts all keys in range
;;@item COUNT-SCAN
;;@tab given
;;@tab counts all keys in range satisfying @var{func}
;;@item REM-SCAN
;;@tab NIL
;;@tab deletes all keys in range
;;@item REM-SCAN
;;@tab given
;;@tab deletes all keys in range satisfying @var{func}
;;@item MODIFY-SCAN
;;@tab NIL
;;@tab ARGERR
;;@item MODIFY-SCAN
;;@tab given
;;@tab updates values for keys in range satisfying @var{func}
;;@end multitable
;;
;;@code{btScan} returns null if there was an error; an empty byte-vector
;;if scan completed; or the next key to be scanned if @var{blklimit} was
;;not @samp{-1}.
;;
;;Each block of data is scanned/deleted/modified in a single operation
;;that is, the block is found and locked only once, and only written after
;;all modifications are made. Tho only exception is that MODIFY-SCANs
;;that increase the size of values  can cause block splits. Such cases
;;are detected and converted to a PUT plus a NEXT. This has
;;two consequences: data is written out each time a PUT occurs,
;;and it is conceivable that @var{func} may be called more than once on the
;;key value that caused the split if a RETRYERR occurs in the PUT.
;;However, SCAN guarantees that only one modification will actually be
;;made in this case (so that one can write INCREMENT-RANGE, for example).
;;
;;@var{func} is passed pointers to (copies of) the key and value,
;;plus one user argument:
;;@example
;;@var{func} (keystr, klen, vstr, vlen, extra_arg);
;;@end example
;;@var{func} is expected to return either: SUCCESS for DELETE/COUNT,
;;NOTPRES/NOTDONE for SKIP (ie, DONT DELETE/COUNT), or any other code to
;;terminate the scan resumably at the current point.  For MODIFY-SCAN,
;;if changing the value, the new value length is returned.  Except for
;;the case mentioned above, the caller can depend on @var{func} being
;;called exactly once for each key value in the specified range, and
;;only on those values.
;;
;;If @var{kstr2} <= @var{kstr1}, then no scan will occur (even if
;;@var{kstr1} is found).  To make possible bounded-time operation
;;@code{btScan} will access at most @var{blkLimit} blocks at a time; if
;;you dont care, give it -1 for @var{blkLimit}.
(define (bt:scan bthan op kstr1 kstr2 #-SCHLEP scmfunc #+SCHLEP func blklimit)
  (define len1 0)
  (define len2 0)
  (cond (kstr1 (set! len1 (bytes-length kstr1)))
	(else (set! len1 START-OF-CHAIN)
	      (set! kstr1 no-byts)))
  (if (and kstr2 (zero? len2)) (edprintf "%s end key is \"\".\n" 'bt:scan))
  (cond (kstr2 (set! len2 (bytes-length kstr2)))
	(else (set! len2 END-OF-CHAIN)
	      (set! kstr2 no-byts)))
  (let ((key1 (make-bytes 256))
	(respkt (make-vector PKT-SIZE))
	#-SCHLEP
	(func
	 (and scmfunc
	      (lambda (key klen val vlen extra)
		(let ((res (scmfunc (subbytes key 0 klen)
				    (subbytes val 0 vlen))))
		  (cond ((number? res) res)
			((not res) NOTPRES)
			((boolean? res) SUCCESS)
			((not (string? res)) TYPERR)
			(else
			 (subbytes-move! res 0 (bytes-length res) val 0)
			 (bytes-length res))))))))
    (PKT:SET-SKEY-COUNT! respkt 0)
    (PKT:SET-SKEY-LEN! respkt len1)
    (subbytes-move! kstr1 0 len1 key1 0)
    (let ((res
	   (bt-scan bthan op key1 len1 kstr2 len2 func #f respkt blklimit)))
      #-SCHLEP
      (list res
	    (PKT:SKEY-COUNT respkt)
	    (subbytes key1 0 (PKT:SKEY-LEN respkt)))
      #+SCHLEP
      (if (err? res) #f (subbytes key1 0 (PKT:SKEY-LEN respkt))))))

#+SCM2JAVA
(pragma.java "
/* Wrapper functions for db access that take strings instead
   of byte arrays */

public static Han createDb(Seg seg, int typ, String nameStr)
{
    return createDb(seg, typ, stringToBytes(nameStr));
}

public static Han createDb(Seg seg, char typ, String nameStr)
{
    return createDb(seg, (int)typ, stringToBytes(nameStr));
}

public static Han openDb(Seg seg, String nameStr)
{
    return openDb(seg, stringToBytes(nameStr));
}

public static void bt_Delete(Han han, String keyStr, String key2Str)
{
    bt_Delete(han, stringToBytes(keyStr), stringToBytes(key2Str));
}

public static String bt_Get(Han han, String keyStr)
{
    byte[] byts = bt_Get(han, stringToBytes(keyStr));
    return bytesToString(byts);
}

public static String bt_Next(Han han, String keyStr)
{
    byte[] byts = bt_Next(han, stringToBytes(keyStr));
    return bytesToString(byts);
}

public static String bt_Prev(Han han, String keyStr)
{
    byte[] byts =  bt_Prev(han, stringToBytes(keyStr));
    return bytesToString(byts);
}

public static void bt_Put(Han han, String keyStr, String key2Str)
{
    bt_Put(han, stringToBytes(keyStr), stringToBytes(key2Str));
}

public static String bt_Rem(Han han, String keyStr)
{
    byte[] byts =  bt_Rem(han, stringToBytes(keyStr));
    return bytesToString(byts);
}
")
