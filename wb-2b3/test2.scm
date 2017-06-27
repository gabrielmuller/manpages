;; "test2.scm" WB-tree File Based Associative String Data Base System.
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

(require 'stdio)
(slib:load-source (in-vicinity (program-vicinity) "wbsys"))

;; FUNC is called with (keystr klen vstr vlen user-arg)
;; FUNC can return SUCCESS for DELETE, NOTPRES/NOTDONE for SKIP,
;; TERMINATE to terminate the scan (no skip), RETRYERR to stop here
;; resumably (also no skip), or some real error code.

;; SCAN-FUNC-1 always returns SUCCESS;
;; SCAN-FUNC-2 returns SUCCESS on every other call, except that if the
;;             key begins with "7" it return TERMINATE;
;; SCAN-FUNC-3 sets value to "1" of value starts with "0", and
;;;            "0" otherwise; it always returns a length of 1;
;;; SCAN-FUNC-4 sets the value to "abc...xyz" and returns SUCCESS (>=0)
;;;            half the time; else NOTPRES.

(define (scan-func-1 keystr klen vstr vlen user-arg)
  (if user-arg
      (dprintf "SCAN-FUNC-1 called klen=%d vlen=%d, key='%.*s', val='%.*s'\n"
	       klen vlen
	       klen keystr vlen vstr))
  SUCCESS)

(define xxx #t)

(define (scan-func-2 keystr klen vstr vlen user-arg)
  (set! xxx (not xxx))
  (let ((result (if (char=? (string-ref keystr 0) (string-ref "7" 0))
		    TERMINATED
		    (if xxx SUCCESS NOTPRES))))
    (dprintf "SCAN-FUNC-2 called klen=%d vlen=%d, key='%.*s', val='%.*s' result=%d\n"
	     klen vlen
	     klen keystr vlen vstr result)
    result))

(define (scan-func-3 keystr klen vstr vlen user-arg)
  (dprintf "SCAN-FUNC-3 called klen=%d vlen=%d, key='%.*s', val='%.*s'\n"
	   klen vlen
	   klen keystr vlen vstr)
  (if (and (> vlen 0) (char=? (string-ref vstr 0) #\0))
      (string-set! vstr 0 #\1)
      (string-set! vstr 0 #\0))
  1)

(define (scan-func-4 keystr klen vstr vlen user-arg)
  (set! xxx (not xxx))
  (let ((result (if (char=? (string-ref keystr 0) (string-ref "7" 0))
		    TERMINATED
		    (if xxx 26 NOTPRES))))
    (dprintf "SCAN-FUNC-4 called klen=%d vlen=%d, key='%.*s', val='%.*s' result=%d\n"
	     klen vlen
	     klen keystr vlen vstr result)
    (if (> result -1)
	(subbytes-move! "abcdefghijklmnopqrstuvwxyz" 0 26 vstr 0))
    result))

;;; test functions
;; this function both adds and removes in non-optimal orders

(define (fl-test4)
  (set! buf-verbose? #f)
  (cstats)
  (radd! 100 "abcdefghijklmnop")
  (cstats) (show-buffers)
  (remove! 1000 999 -1)
  (cstats) (show-buffers)
  (radd! 100 "abcdefghijklmnop")
  (cstats) (show-buffers)
  (remove! 1000 1099 -1)
  (cstats) (show-buffers)
  )

(define (scan-range han key-str k-len key2-str k2-len func user-arg)
  (scan-internal COUNT-SCAN han key-str k-len key2-str k2-len func user-arg #t))

(define (delete-range han key-str k-len key2-str k2-len func user-arg)
  (scan-internal REM-SCAN han key-str k-len key2-str k2-len func user-arg #t))

(define (update-range han key-str k-len key2-str k2-len func user-arg)
  (scan-internal MODIFY-SCAN han key-str k-len key2-str k2-len func user-arg #t))

(define (scan-internal operation han key-str k-len key2-str k2-len func user-arg verbose)
  (let ((respkt (make-vector PKT-SIZE))
	(xstr (make-bytes 256))
	(result #f))
    (PKT:SET-SKEY-COUNT! respkt 0)
    (if (>= k-len 0) (subbytes-move! key-str 0 k-len xstr 0))
    (set! result (bt-scan han operation xstr k-len key2-str k2-len
			  func user-arg respkt 999))
    (if verbose
	(dprintf
	 "SCAN(%s): found %d keys, result=%d, klen=%d, retkey='%.*s'\n"
	 (cond ((eq? operation COUNT-SCAN) "COUNT")
	       ((eq? operation MODIFY-SCAN) "MODIFY")
	       ((not func) "UDELETE")
	       (else "DELETE"))
	 (KEY-COUNT respkt) result (KEY-LEN respkt)
	 (KEY-LEN respkt) xstr))
    (if (= result SUCCESS) (KEY-COUNT respkt) result)))

(define (scount)
  (dprintf "**** %d items in range. **** \n"
	     (scan-internal COUNT-SCAN current-bt "" -2 "" -1 #f #f #f)))

(define foostr "67")

;; NOTE: the expected numbers of records for each (SCOUNT) are:
;; 5, 105, 86, 67, 58,... (If rerun, first count will be 58 not 5.)

(define (scan-test )
  (scan-range current-bt "a" 1 "z" 1 scan-func-1 #f)
  (scount)
  (add! 100 1 1 "abcdefg")
  (scan-range current-bt "a" 1 "z" 1 scan-func-1 #f)
  (scan-range current-bt "a" 1 "z" 1 scan-func-2 #f)
  (scount)
  (delete-range current-bt "16" 2 "33" 2 scan-func-1 #f)
  ;;(scanhan current-bt)
  (scount)
  (delete-range current-bt "1" 1 "43" 2 #f #f)
  (scount)
  (delete-range current-bt "43" 2 "59" 2 scan-func-2 #f)
  (scount)
  (scan-range current-bt foostr 2 "753" 3 scan-func-2 #f) ; test TERMINATE at "7"
  (scount)
  (update-range current-bt "74" 2 "94" 2 scan-func-3 #f)
  (scount)
  (update-range current-bt "84" 2 "99" 2 scan-func-4 #f)
  (scount)
  (update-range current-bt "84" 2 "94" 2 scan-func-3 #f)
  (update-range current-bt "20" 2 "94" 2 scan-func-4 #f)
  (scount)
  (scan-range current-bt "" -2 "" -1 scan-func-1 #t)
  )
