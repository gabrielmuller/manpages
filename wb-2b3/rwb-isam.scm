;;; "rwb-isam.scm" Relational WB database with sequential indexes.
; Copyright 1996, 2000, 2001, 2003 Aubrey Jaffer
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

;;;; *catalog* is informed of 'rwb-isam binding by "scm/mkimpcat.scm".
(require 'wb)
(require 'byte)
(require 'byte-number)
(require 'relational-database)		;for make-relational-system

(init-wb 75 150 2048)

;@
(define rwb-isam
  ;; foiled indentation so etags will recognize definitions
  (let ((make-handle list)
	(handle->base-id car)
	(handle->bt cadr)
	(catalog-id 0)
	(free-id "")
	(root-name "rwb")
	(key:s255 (bytes 255))
	(key:col1 (bytes 1))
	(key:col0 (bytes 0))
	(key:null (bytes 0))

	(key:col-field bytes))

;;;The least-upper-bound of a composite key.
(define (key:incr key)
  (string-append key key:s255))

;;;Return key sans prefix and column suffix if first column.
(define (key:match-prefix? prefix ckey)
  (define sdx (+ -1 (string-length ckey)))
  (define prelen (string-length prefix))
  (and (<= prelen sdx)
       (string=? prefix (substring ckey 0 prelen))
       (substring ckey prelen sdx)))

;;;Detects when all match-keys given are false.
(define (list-all-false? lst)
  (cond ((null? lst) #t)
	((car lst) #f)
	(else (list-all-false? (cdr lst)))))

;;; These two NTHCDR procedures replicate those in "comlist.scm".
(define (nthcdr k list)
  (do ((i k (+ -1 i))
       (lst list (cdr lst)))
      ((<= i 0) lst)))

(define (butnthcdr k lst)
  (cond ((or (zero? k) (null? lst)) '())
	(else (let ((ans (list (car lst))))
		(do ((lst (cdr lst) (cdr lst))
		     (tail ans (cdr tail))
		     (k (+ -2 k) (+ -1 k)))
		    ((or (negative? k) (null? lst)) ans)
		  (set-cdr! tail (list (car lst))))))))

;;;; Create, open, write, sync, or close database.

;;; Because B-trees grow in depth only very slowly, we might as well
;;; put everything into one B-tree named "rwb".

(define (make-base filename dim types)
  (define seg (make-seg filename 2048))
  (cond ((not seg)
	 (slib:error 'make-base "couldn't make new base" filename)
	 #f)
	((or (wb:err? (bt:put! (create-db seg #\T root-name) free-id "1"))
	     (wb:err? (bt:put! (open-bt seg 0 1) "base-table" "wb-table")))
	 (slib:error 'make-base "couldn't modify new base" filename)
	 #f)
	(else seg)))

(define (open-base filename writable?)
  (open-seg filename writable?))

(define (write-base seg filename)
  (cond ((and filename (equal? filename (SEG:STR seg)))
	 (let ((status (close-seg seg #f)))
	   (cond ((wb:err? status) #f)
		 (else
		  (set! seg (open-seg filename #t))
		  (cond ((not seg) #f)
			(else #t))))))
	(else
	 ;;(slib:error 'write-base "WB can't change database filename" filename)
	 #f)))

(define (sync-base seg)
  (and seg (write-base seg (SEG:STR seg))))

(define (close-base seg)
  (not (wb:err? (close-seg seg #f))))

;;;; Make, open, and destroy tables.

(define (make-table seg dim types)
  (and (SEG:MUTABLE? seg)
       (let* ((root (open-db seg root-name))
	      (tns (bt:rem root free-id))
	      (base-id (and (string? tns) (string->number tns))))
	 (cond ((not tns)
		(slib:error 'make-table 'free-id "in use?")
		#f)
	       ((not base-id)
		(bt:put root free-id tns)
		(slib:error 'make-table "free-id corrupted" base-id)
		#f)
	       ((not (bt:put root free-id (number->string (+ 1 base-id))))
		(slib:error 'make-table "free-id lock broken")
		#f)
	       (else base-id)))))

;;; OPEN-TABLE allocates a new handle (in call to open-db) so each
;;; table handle will have its own last-block-used

(define (open-table seg base-id dim types)
  (define (base-id->prefix base-id)
    (define nstr (number->string base-id))
    (string-append (string #\T (integer->char (string-length nstr)))
		   nstr
		   (string (integer->char 1) #\D)))
  (make-handle (base-id->prefix base-id)
		  (open-db seg root-name)))

(define (kill-table seg base-id dim types)
  (let* ((handle (open-table seg base-id dim types))
	 (prefix (handle->base-id handle)))
    (not (wb:err? (bt:rem* (handle->bt handle)
			   prefix
			   (key:incr prefix))))))

;;;; Conversions from Scheme objects into and from strings.

(define (object->wb-string type)
  (case type
    ((string) identity)
    ((symbol) symbol->string)
    ((integer number ordinal) number->string)
    ((boolean) (lambda (b) (if b "T" "F")))
    ((c64) (lambda (x) (string-append (ieee-double->bytes (real-part x))
				      (ieee-double->bytes (imag-part x)))))
    ((c32) (lambda (x) (string-append (ieee-float->bytes (real-part x))
				      (ieee-float->bytes (imag-part x)))))
    ((r64) (lambda (x) (ieee-double->bytes x)))
    ((r32) (lambda (x) (ieee-float->bytes x)))
    ((s64) (lambda (n) (integer->bytes n -8)))
    ((s32) (lambda (n) (integer->bytes n -4)))
    ((s16) (lambda (n) (integer->bytes n -2)))
    (( s8) (lambda (n) (integer->bytes n -1)))
    ((u64) (lambda (n) (integer->bytes n 8)))
    ((u32) (lambda (n) (integer->bytes n 4)))
    ((u16) (lambda (n) (integer->bytes n 2)))
    (( u8) (lambda (n) (integer->bytes n 1)))
    ((atom) (lambda (obj) (if (not obj) "#f" (symbol->string obj))))
    ((expression) (lambda (obj) (call-with-output-string
				    (lambda (port) (write obj port)))))
    (else #f)))

(define (wb-string->object type)
  (case type
    ((string) identity)
    ((symbol) string->symbol)
    ((integer number ordinal) string->number)
    ((boolean) (lambda (str) (not (equal? str "F"))))
    ((c64) (lambda (str) (make-rectangular
			  (bytes->ieee-double (substring str 0 8))
			  (bytes->ieee-double (substring str 8 16)))))
    ((c32) (lambda (str) (make-rectangular
			  (bytes->ieee-float (substring str 0 4))
			  (bytes->ieee-float (substring str 4 8)))))
    ((r64) (lambda (str) (bytes->ieee-double str)))
    ((r32) (lambda (str) (bytes->ieee-float str)))
    ((s64) (lambda (str) (bytes->integer str -8)))
    ((s32) (lambda (str) (bytes->integer str -4)))
    ((s16) (lambda (str) (bytes->integer str -2)))
    (( s8) (lambda (str) (bytes->integer str -1)))
    ((u64) (lambda (str) (bytes->integer str 8)))
    ((u32) (lambda (str) (bytes->integer str 4)))
    ((u16) (lambda (str) (bytes->integer str 2)))
    (( u8) (lambda (str) (bytes->integer str 1)))
    ((atom) (lambda (str) (if (string-ci=? "#f" str) #f (string->symbol str))))
    ((expression) (lambda (str) (call-with-input-string str read)))
    (else #f)))

(define (supported-type? type)
  (case type
    ((ordinal atom integer number boolean string symbol expression
	      c64 c32 r64 r32 s64 s32 s16 s8 u64 u32 u16 u8) #t)
    (else #f)))

(define (supported-key-type? type)
  (case type
    ((atom ordinal integer number symbol string boolean
	   r64 r32 s64 s32 s16 s8 u64 u32 u16 u8) #t)
    (else #f)))

;;;; Keys

;;;Keys are composed of one to many fields.
;;;
;;;* The binary number formats r64, r32, s64, s32, s16, s8, u64, u32,
;;;  u16, and u8 have fixed widths and are encoded so that the key
;;;  sort order is the same as numerical order.
;;;
;;;* Booleans occupy one byte: 'T' or 'F'.
;;;
;;;* Strings, symbols, and atoms (symbol or #f) are variable width
;;;  fields terminated by a null byte.  They sort in lexicographic
;;;  (dictionary) order.  A #f atom is represented by the null string.
;;;
;;;* The integer, number, and ordinal formats are strings of decimal
;;;  digits preceeded by a length byte.  Nonnegative integers sort
;;;  correctly.
;;;
;;;Use of null bytes in string, symbol, or atom key-fields will break
;;;this encoding.

(define (string-number-keyifier n)
  (define str (number->string n))
  (string-append (bytes (string-length str)) str))

(define (string-keyifier str)
  (string-append str key:null))

(define (key:shorten-1 str)
  (substring str 0 (+ -1 (string-length str))))

;;; unitary composite-key maker
(define (make-keyifier-1 type)
  (case type
    ((string) string-keyifier)
    ((symbol) (lambda (s) (string-keyifier (symbol->string s))))
    ((atom) (lambda (obj) (string-keyifier (if obj (symbol->string obj) ""))))
    ((boolean) (lambda (b) (if b "T" "F")))
    ((integer number ordinal) string-number-keyifier)
    ;; binary number formats
    ((r64) (lambda (x) (ieee-byte-collate! (ieee-double->bytes x))))
    ((r32) (lambda (x) (ieee-byte-collate! (ieee-float->bytes x))))
    ((s64) (lambda (n) (integer-byte-collate! (integer->bytes n -8))))
    ((s32) (lambda (n) (integer-byte-collate! (integer->bytes n -4))))
    ((s16) (lambda (n) (integer-byte-collate! (integer->bytes n -2))))
    (( s8) (lambda (n) (integer->bytes n -1)))
    ((u64) (lambda (n) (integer-byte-collate! (integer->bytes n 8))))
    ((u32) (lambda (n) (integer-byte-collate! (integer->bytes n 4))))
    ((u16) (lambda (n) (integer-byte-collate! (integer->bytes n 2))))
    (( u8) (lambda (n) (integer->bytes n 1)))
    (else (slib:error 'make-keyifier-1 'unsupported-type type))))

;;; composite-key maker
(define (key-polymerase prinum types)
  (set! types (butnthcdr prinum types))
  ;; Special case when there is just one primary key.
  (if (= 1 prinum)
      (let ((proc (make-keyifier-1 (car types))))
	(lambda (lst) (proc (car lst))))
      (let ((procs (map make-keyifier-1 types)))
	(lambda (lst)
	  (apply string-append (map (lambda (p v) (p v)) procs lst))))))

(define (key:width type)
  (case type
    ((r64 s64 u64) 8)
    ((r32 s32 u32) 4)
    ((s16 u16) 2)
    ((s8 u8 boolean) 1)
    ((integer number ordinal)
     (lambda (key pos) (+ 1 (byte-ref key pos))))
    ((string symbol atom)		;null terminated
     (lambda (key pos)
       (do ((i pos (+ 1 i)))
	   ((zero? (byte-ref key i)) (- i pos -1)))))
    (else #f)))

(define (exokeyase type)
  (case type
    ((string) key:shorten-1)
    ((symbol) (lambda (str) (string->symbol (key:shorten-1 str))))
    ((atom) (lambda (str) (if (string=? "" str) #f
			      (string->symbol (key:shorten-1 str)))))
    ((boolean) (lambda (str) (not (string=? "F" str))))
    ((integer number ordinal)
     (lambda (str) (string->number (substring str 1 (string-length str)))))
    ;; binary number formats
    ((r64) (lambda (str) (bytes->ieee-double (ieee-byte-decollate! str))))
    ((r32) (lambda (str) (bytes->ieee-float (ieee-byte-decollate! str))))
    ((s64) (lambda (str) (bytes->integer (integer-byte-collate! str) -8)))
    ((s32) (lambda (str) (bytes->integer (integer-byte-collate! str) -4)))
    ((s16) (lambda (str) (bytes->integer (integer-byte-collate! str) -2)))
    (( s8) (lambda (str) (bytes->integer str -1)))
    ((u64) (lambda (str) (bytes->integer (integer-byte-collate! str) 8)))
    ((u32) (lambda (str) (bytes->integer (integer-byte-collate! str) 4)))
    ((u16) (lambda (str) (bytes->integer (integer-byte-collate! str) 2)))
    (( u8) (lambda (str) (bytes->integer str 1)))
    (else #f)))

;;; extracts one key-field from composite-key
(define (make-key-extractor primary-limit types index)
  (define (wither type)
    (or (key:width type)
	(slib:error 'make-key-extractor 'unsupported-type type)))
  (let ((proc (exokeyase (list-ref types (+ -1 index))))
	(skips (map wither (butnthcdr index types))))
    (lambda (key)
      (let loop ((pos 0) (skips skips))
	(define flen (car skips))
	(if (procedure? flen) (set! flen (flen key pos)))
	(if (null? (cdr skips))
	    (proc (substring key pos (+ pos flen)))
	    (loop (+ pos flen) (cdr skips)))))))

;;; composite-key to list
(define (make-key->list primary-limit types)
  (define (wither type)
    (or (key:width type)
	(slib:error 'make-key->list 'unsupported-type type)))
  (define typs (butnthcdr primary-limit types))
  (let ((procs (map exokeyase typs))
	(skips (map wither typs)))
    (lambda (key)
      (let loop ((pos 0) (skips skips) (procs procs))
	(define flen (car skips))
	(if (procedure? flen) (set! flen (flen key pos)))
	;;(print 'key->list pos flen typs key)
	(cons ((car procs) (substring key pos (+ pos flen)))
	      (if (null? (cdr skips))
		  '()
		  (loop (+ pos flen) (cdr skips) (cdr procs))))))))

;;;; for-each-key, ordered-for-each-key, and map-key

(define (make-key-match? key-dimension column-types match-keys)
  (if (list-all-false? match-keys)
      (lambda (ckey) #t)
      (let ((keyploder (make-key->list key-dimension column-types)))
	(lambda (ckey)
	  (define (key-match? match-keys keys)
	    (cond ((null? match-keys) #t)
		  ((not (car match-keys))
		   (key-match? (cdr match-keys) (cdr keys)))
		  ((equal? (car match-keys) (car keys))
		   (key-match? (cdr match-keys) (cdr keys)))
		  ((not (procedure? (car match-keys)))
		   #f)
		  (((car match-keys) (car keys))
		   (key-match? (cdr match-keys) (cdr keys)))
		  (else #f)))
	  (key-match? match-keys (keyploder ckey))))))

(define (map-key handle operation key-dimension column-types match-keys)
  (define lst (list 'dummy))
  (let ((tail lst))
    (ordered-for-each-key handle
			  (lambda (k)
			    (set-cdr! tail (list (operation k)))
			    (set! tail (cdr tail)))
			  key-dimension column-types match-keys)
    (cdr lst)))

;;;; Indexed Sequential Access Methods

(define (ordered-for-each-key handle operation key-dimension column-types match-keys)
  (let ((bt (handle->bt handle))
	(prefix (handle->base-id handle))
	(key-match? (make-key-match? key-dimension column-types match-keys)))
    (case (- (length column-types) key-dimension)
      ((0) (let ((prefix+ (key:incr prefix))
		 (maproc (lambda (ckey val)
			   (define fkey (key:match-prefix? prefix ckey))
			   ;;(print 'ordered-for-each-key ckey fkey)
			   (and fkey (key-match? fkey) (operation fkey))
			   #f)))
	     (do ((res (bt:scan bt 0 prefix prefix+ maproc 1)
		       (bt:scan bt 0 (caddr res) prefix+ maproc 1)))
		 ((not (= -1 (car res)))))))
      (else (let ((prelen (string-length prefix)))
	      (do ((nkey (bt:next bt prefix)
			 (bt:next bt (key:incr (key:shorten-1 nkey)))))
		  ((or (not nkey)
		       (not (string=? prefix (substring nkey 0 prelen))))
		   #f)
		;;(print 'ordered-for-each-key nkey (key:match-prefix? prefix nkey))
		(let ((fkey (key:match-prefix? prefix nkey)))
		  (and fkey (key-match? fkey) (operation fkey)))))))))

(define (make-nexter handle key-dimension column-types index)
  (define bt (handle->bt handle))
  (define prefix (handle->base-id handle))
  (define key->list (make-key->list key-dimension column-types))
  (define list->key (key-polymerase key-dimension column-types))
  (lambda keys
    (define nkey
      (bt:next bt (string-append prefix
				 (list->key (butnthcdr index keys))
				 key:s255)))
    (and nkey (let ((ckey (key:match-prefix? prefix nkey)))
		(and ckey (key->list ckey))))))

(define (make-prever handle key-dimension column-types index)
  (define bt (handle->bt handle))
  (define ldx (- (length column-types) key-dimension))
  (define prefix (handle->base-id handle))
  (define key->list (make-key->list key-dimension column-types))
  (define list->key (key-polymerase key-dimension column-types))
  (lambda keys
    (define pkey
      (bt:prev bt (string-append prefix
				 (list->key (butnthcdr index keys)))))
    (and pkey (let ((ckey (key:match-prefix? prefix pkey)))
		(and ckey (key->list ckey))))))

;;;; getters and putters

;;;Records are stored as multiple copies of the key to which a
;;;one-byte code is appended, identifying the field.  If all fields
;;;are primary keys, then KEY:COL0 (a 0 byte) is appended.

(define (make-getter-1 prinum types index)
  (define type (list-ref types (- index prinum)))
  (let ((proc (or (wb-string->object type)
		  (slib:error 'make-getter-1 'unsupported-type type)))
	(ci (key:col-field (- index prinum))))
    (lambda (handle key)
      (define val
	(db:get
	 (handle->bt handle)
	 (string-append (handle->base-id handle) key ci))) ; (print 'ckey )
      (and val (proc val)))))

;;;If more than one non-primary value is stored, then use SCAN to
;;;extract the values.

(define (make-getter prinum types)
  (define (wbstr->obj type)
    (or (wb-string->object type)
	(slib:error 'make-getter 'unsupported-type type)))
  (case (- (length types) prinum)
    ((0) (lambda (handle key)
	   (and (db:get (handle->bt handle)
			(string-append (handle->base-id handle) key key:col0))
		'())))
    ((1) (let ((proc (wbstr->obj (list-ref types prinum))))
	   (lambda (handle key)
	     (define val
	       (db:get
		(handle->bt handle)
		(string-append (handle->base-id handle) key key:col1)))
	     (and val (list (proc val))))))
    (else (let ((procs (map wbstr->obj (nthcdr prinum types))))
	    (lambda (handle key)
	      (define lst (list 'dummy))
	      (define idx 1)
	      (let ((bt (handle->bt handle))
		    (prefix (string-append (handle->base-id handle) key))
		    (tail lst))
		(define (loop procs)
		  (define val (db:get bt (string-append prefix (bytes idx))))
		  (cond (val (set-cdr! tail (list ((car procs) val)))
			     (set! tail (cdr tail))
			     (set! idx (+ 1 idx))
			     (if (null? (cdr procs))
				 (cdr lst)
				 (loop (cdr procs))))
			(else #f)))
		(loop procs)))))))

(define (make-putter prinum types)
  (define (obj->wbstr type)
    (or (object->wb-string type)
	(slib:error 'make-putter 'unsupported-type type)))
  (case (- (length types) prinum)
    ((0) (lambda (handle ckey restcols)
	   (bt:put! (handle->bt handle)
		    (string-append (handle->base-id handle) ckey key:col0)
		    "")))
    ((1) (let ((proc (obj->wbstr (list-ref types prinum))))
	   (lambda (handle ckey restcols)
	     (db:put! (handle->bt handle)
		       (string-append (handle->base-id handle) ckey key:col1)
		       (proc (car restcols))))))
    (else (let ((procs (map obj->wbstr (nthcdr prinum types))))
	    (lambda (handle ckey restcols)
	      (define i 0)
	      (for-each
	       (lambda (proc val)
		 (set! i (+ 1 i))
		 (db:put! (handle->bt handle)
			   (string-append (handle->base-id handle)
					  ckey
					  (key:col-field i))
			   (proc val)))
	       procs restcols))))))

;;;; other table methods.

(define (present? handle key)
  (let* ((kc (string-append (handle->base-id handle) key))
	 (kcl (string-length kc))
	 (n (bt:next (handle->bt handle) kc)))
    (and n
	 (<= (+ 1 kcl) (string-length n) (+ 2 kcl))
	 (string=? kc (substring n 0 kcl)))))

(define (delete handle key)
  (let ((prefix (string-append (handle->base-id handle) key)))
    (not (wb:err? (bt:rem* (handle->bt handle)
			   prefix
			   (key:incr prefix))))))

(define (delete* handle key-dimension column-types match-keys)
  (let ((prefix (string-append (handle->base-id handle) match-keys)))
    (not (wb:err? (bt:rem* (handle->bt handle)
			   prefix
			   (key:incr prefix))))))

    (lambda (operation-name)
      ;;(require 'trace)
      #+foo			  ; To trace methods use this wrapper:
      ((lambda (proc)
	 (if (procedure? proc)
	     (lambda args
	       (let ((ans (apply proc args)))
		 (if (procedure? ans)
		     (tracef ans operation-name)
		     ans)))
	     proc))
       )
      (case operation-name
	((make-base) make-base)
	((open-base) open-base)
	((write-base) write-base)
	((sync-base) sync-base)
	((close-base) close-base)
	((make-table) make-table)
	((open-table) open-table)
	((kill-table) kill-table)
	((make-keyifier-1) make-keyifier-1)
	((make-list-keyifier) key-polymerase)
	((make-key->list) make-key->list)
	((make-key-extractor) make-key-extractor)
	((supported-type?) supported-type?)
	((supported-key-type?) supported-key-type?)
	((present?) present?)
	((make-putter) make-putter)
	((make-getter) make-getter)
	((make-getter-1) make-getter-1)
	((delete) delete)
	((delete*) delete*)
	((for-each-key) ordered-for-each-key)
	((map-key) map-key)
	((ordered-for-each-key) ordered-for-each-key)
	((make-nexter) make-nexter)
	((make-prever) make-prever)
	((catalog-id) catalog-id)
	(else #f))
      )))

(set! *base-table-implementations*
      (cons (list 'rwb-isam (make-relational-system rwb-isam))
	    *base-table-implementations*))
;;(trace bt:scan bt:get make-getter map-key ordered-for-each-key make-key-extractor make-key->list) (set! *qp-width* 333) ;;(trace-all "rwb-isam.scm")
