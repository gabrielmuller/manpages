;;; "wbtab.scm" database tables using WB b-trees.
; Copyright 1996, 2000, 2001, 2003, 2008 Aubrey Jaffer
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

;;;; *catalog* is informed of 'wb-table binding by "scm/mkimpcat.scm".
(require 'wb)
(require 'byte)
(require 'byte-number)
(require 'relational-database)		;for make-relational-system

(init-wb 75 150 2048)

;@
(define wb-table
  ;; foiled indentation so etags will recognize definitions
  (let ((make-handle list)
	(handle->base-id car)
	(handle->bt cadr)
	(catalog-id 0)
	(free-id "")
	(root-name "rwb")

;;;k30 is a key-coding system where adjacent key fields are separated
;;;by a byte with value in the range 0 (^@=#\nul) to 31 (^_=#\us).
;;;Strings are prefixed with 30 and extend to the the next byte
;;;smaller than 32, or end of the key.  Numbers are prefixed by the
;;;string-length of the string representation of the number up to 30.
;;;Unsigned integers with less than 31 digits will thus sort in
;;;numerical order.  Larger numbers and strings will sort
;;;lexicographically.
;;;
;;;Use of bytes with values less than 32 in key fields will wedge k30.

	(k30:true (bytes 1 (char->integer #\T)))
	(k30:false (bytes 1 (char->integer #\F)))
	(k30:s31 (bytes 31))
	(k30:s30 (bytes 30))
	(k30:s1 (bytes 1 (char->integer #\1)))
	(k30:s0 (bytes 0)))

;;;A suffix encoding the field number (with number length prepended)
;;;is appended to composite keys.  COL-FIELD computes this field.
;;;k30:s0 and k30:s1 are constants for cases 0 and 1 respectively.
;;;Note that ks30:s0 has no digits!

(define (k30:incr-key prefix)
  (string-append prefix k30:s31))

(define (col-field i)
  (let ((str (number->string i)))
    (string-append (bytes (string-length str)) str)))

(define (k30:number-keyifier n)
  (define str (number->string n))
  (string-append (bytes (min 30 (string-length str))) str))

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
			   (k30:incr-key prefix))))))

;;;; Conversions from Scheme objects into and from strings.

(define (object->wb-string type)
  (case type
    ((string) identity)
    ((symbol) symbol->string)
    ((integer number ordinal) number->string)
    ((boolean) (lambda (b) (if b "Y" "N")))
    ((c64) (lambda (x) (string-append (ieee-double->bytes (real-part x))
				      (ieee-double->bytes (imag-part x)))))
    ((c32) (lambda (x) (string-append (ieee-float->bytes (real-part x))
				      (ieee-float->bytes (imag-part x)))))
    ((r64) (lambda (x) (ieee-double->bytes x)))
    ((r32) (lambda (x) (ieee-single->bytes x)))
    ((s64) (lambda (n) (integer->bytes n -8)))
    ((s32) (lambda (n) (integer->bytes n -4)))
    ((s16) (lambda (n) (integer->bytes n -2)))
    (( s8) (lambda (n) (integer->bytes n -1)))
    ((u64) (lambda (n) (integer->bytes n 8)))
    ((u32) (lambda (n) (integer->bytes n 4)))
    ((u16) (lambda (n) (integer->bytes n 2)))
    (( u8) (lambda (n) (integer->bytes n 1)))
    ((atom) (lambda (obj)
	      (cond ((number? obj) (number->string obj))
		    ((not obj) "#f")
		    (else (symbol->string obj)))))
    ((expression) (lambda (obj) (call-with-output-string
				    (lambda (port) (write obj port)))))
    (else #f)))

(define (wb-string->object type)
  (case type
    ((string) identity)
    ((symbol) string->symbol)
    ((integer number ordinal) string->number)
    ((boolean) (lambda (str) (not (equal? str "N"))))
    ((c64) (lambda (str) (make-rectangular
			  (bytes->ieee-double (substring str 0 8))
			  (bytes->ieee-double (substring str 8 16)))))
    ((c32) (lambda (str) (make-rectangular
			  (bytes->ieee-float (substring str 0 4))
			  (bytes->ieee-float (substring str 4 8)))))
    ((r64) (lambda (str) (bytes->ieee-double str)))
    ((r32) (lambda (str) (bytes->ieee-single str)))
    ((s64) (lambda (str) (integer->bytes str -8)))
    ((s32) (lambda (str) (integer->bytes str -4)))
    ((s16) (lambda (str) (integer->bytes str -2)))
    (( s8) (lambda (str) (bytes->integer str -1)))
    ((u64) (lambda (str) (integer->bytes str 8)))
    ((u32) (lambda (str) (integer->bytes str 4)))
    ((u16) (lambda (str) (integer->bytes str 2)))
    (( u8) (lambda (str) (bytes->integer str 1)))
    ((atom) (lambda (str)
	      (cond ((string->number str))
		    ((string-ci=? "#f" str) #f)
		    (else (string->symbol str)))))
    ((expression) (lambda (str) (call-with-input-string str read)))
    (else #f)))

(define (supported-type? type)
  (case type
    ((ordinal atom integer number boolean string symbol expression
	      c64 c32 r64 r32 s64 s32 s16 s8 u64 u32 u16 u8) #t)
    (else #f)))

(define (supported-key-type? type)
  (case type
    ((atom integer number symbol string boolean) #t)
    (else #f)))

;;;; Keys

;;; unitary composite-key maker
(define (make-keyifier-1 type)
  (case type
    ((string) (lambda (s) (string-append k30:s30 s)))
    ((symbol) (lambda (s) (string-append k30:s30 (symbol->string s))))
    ((integer number ordinal) k30:number-keyifier)
    ((boolean) (lambda (b) (if b k30:true k30:false)))
    ((atom) (lambda (obj)
	      (cond ((not obj) k30:false)
		    ((number? obj) (k30:number-keyifier obj))
		    (else (string-append k30:s30 (symbol->string obj))))))
    (else (slib:error 'make-keyifier-1 'unsupported-type type))))

;;; composite-key maker
(define (make-list-keyifier prinum types)
  (set! types (butnthcdr prinum types))
  ;; Special case when there is just one primary key.
  (if (= 1 prinum)
      (let ((proc (make-keyifier-1 (car types))))
	(lambda (lst) (proc (car lst))))
      (let ((procs (map make-keyifier-1 types)))
	(lambda (lst)
	  (apply string-append (map (lambda (p v) (p v)) procs lst))))))

(define (k30:width type key pos kend)
  (define flen (byte-ref key pos))
  (set! pos (+ 1 pos))
  (cond ((= flen 30)
	 (do ((i pos (+ 1 i)))
	     ((or (>= i kend)
		  (<= 0 (byte-ref key i) 30))
	      (set! flen (- i pos))))))
  flen)

;;; extracts one key-field from composite-key
(define (make-key-extractor primary-limit types index)
  (define (wbstr->obj type)
    (or (wb-string->object type)
	(slib:error 'make-key-extractor 'unsupported-type type)))
  (let ((proc (wbstr->obj (list-ref types (+ -1 index)))))
    (lambda (key)
      (define kend (string-length key))
      (let loop ((pos 0) (argind (+ -1 index)) (types types))
	(if (positive? argind)
	    (loop (+ 1 pos (k30:width (car types) key pos kend))
		  (+ -1 argind)
		  (cdr types))
	    (proc (substring key
			     (+ 1 pos)
			     (+ 1 pos (k30:width (car types) key pos kend))
			     )))))))

;;; composite-key to list
(define (make-key->list prinum types)
  (define (wbstr->obj type)
    (or (wb-string->object type)
	(slib:error 'make-key->list 'unsupported-type type)))
  (let ((procs (map wbstr->obj (butnthcdr prinum types))))
    (lambda (key)
      (define kend (string-length key))
      (let loop ((pos 0) (argind (+ -1 prinum)) (types types) (procs procs))
	(define flen (k30:width (car types) key pos kend))
	(cons ((car procs) (substring key (+ 1 pos) (+ 1 flen pos)))
	      (if (zero? argind)
		  '()
		  (loop (+ 1 flen pos) (+ -1 argind) (cdr types) (cdr procs))))))))

;;;; for-each-key, ordered-for-each-key, and map-key

(define (list-of-false? lst)
  (cond ((null? lst) #t)
	((car lst) #f)
	(else (list-of-false? (cdr lst)))))

(define (make-key-match? key-dimension column-types match-keys)
  (if (list-of-false? match-keys)
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

(define (ordered-for-each-key
	 handle operation key-dimension column-types match-keys)
  (let* ((bt (handle->bt handle))
	 (prefix (handle->base-id handle))
	 (pl (string-length prefix))
	 (prefix+ (k30:incr-key prefix))
	 (key-match? (make-key-match? key-dimension column-types match-keys))
	 (maproc
	  (lambda (k v)
	    (let ((i (+ -1 (string-length k))))
	      (cond ((and (char=? #\1 (string-ref k i))
			  (= 1 (byte-ref k (+ -1 i))))
		     (and (key-match? (substring k pl (+ -1 i)))
			  (operation (substring k pl (+ -1 i)))))
		    ((= 0 (byte-ref k i))
		     (and (key-match? (substring k pl i))
			  (operation (substring k pl i))))))
	    #f)))
    (do ((res (bt:scan bt 0 prefix prefix+ maproc 1)
	      (bt:scan bt 0 (caddr res) prefix+ maproc 1)))
	((not (= -1 (car res)))))))

(define (map-key handle operation key-dimension column-types match-keys)
  (define lst (list 'dummy))
  (let ((tail lst))
    (ordered-for-each-key handle
			  (lambda (k)
			    (set-cdr! tail (list (operation k)))
			    (set! tail (cdr tail)))
			  key-dimension column-types match-keys)
    (cdr lst)))

;;;; getters and putters

(define (make-getter prinum types)
  (define (wbstr->obj type)
    (or (wb-string->object type)
	(slib:error 'make-getter 'unsupported-type type)))
  (case (- (length types) prinum)
    ((0) (lambda (handle key)
	   (and (bt:get (handle->bt handle)
			(string-append (handle->base-id handle) key k30:s0))
		'())))
    ((1) (let ((proc (wbstr->obj (list-ref types prinum))))
	   (lambda (handle key)
	     (define val
	       (bt:get
		(handle->bt handle)
		(string-append (handle->base-id handle) key k30:s1)))
	     (and val (list (proc val))))))
    (else (let ((procs (reverse (map wbstr->obj (nthcdr prinum types)))))
	    (lambda (handle key)
	      (let* ((bt (handle->bt handle))
		     (prefix (string-append (handle->base-id handle) key))
		     (prefix+ (k30:incr-key prefix))
		     (lst '())
		     (maproc (lambda (k v) (set! lst (cons v lst)) #t)))
		(do ((res (bt:scan bt 0 prefix prefix+ maproc 1)
			  (bt:scan bt 0 (caddr res) prefix+ maproc 1)))
		    ((not (= -1 (car res)))
		     (and (not (zero? (cadr res)))
			  (do ((ps procs (cdr ps))
			       (ls lst (cdr ls))
			       (rl '() (cons ((car ps) (car ls)) rl)))
			      ((null? (cdr ls))
			       (cons ((car ps) (car ls)) rl))))))))))))

(define (make-putter prinum types)
  (define (obj->wbstr type)
    (or (object->wb-string type)
	(slib:error 'make-putter 'unsupported-type type)))
  (case (- (length types) prinum)
    ((0) (lambda (handle ckey restcols)
	   (bt:put! (handle->bt handle)
		    (string-append (handle->base-id handle) ckey k30:s0)
		    "")))
    ((1) (let ((proc (obj->wbstr (list-ref types prinum))))
	   (lambda (handle ckey restcols)
	     (bt:put! (handle->bt handle)
		      (string-append (handle->base-id handle) ckey k30:s1)
		      (proc (car restcols))))))
    (else (let ((procs (map obj->wbstr (nthcdr prinum types))))
	    (lambda (handle ckey restcols)
	      (define i 0)
	      (for-each
	       (lambda (proc val)
		 (set! i (+ 1 i))
		 (cond ((wb:err?
			 (bt:put! (handle->bt handle)
				  (string-append (handle->base-id handle)
						 ckey
						 (col-field i))
				  (proc val)))
			(slib:error 'putter "couldn't put"
				    (string-append (handle->base-id handle)
						   ckey
						   (col-field i))
				    (proc val)))))
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
			   (k30:incr-key prefix))))))

(define (delete* handle key-dimension column-types match-keys)
  (let ((prefix (string-append (handle->base-id handle) match-keys)))
    (not (wb:err? (bt:rem* (handle->bt handle)
			   prefix
			   (k30:incr-key prefix))))))

  (lambda (operation-name)
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
;;(trace bt:scan bt:get map-key ordered-for-each-key make-key-extractor make-key->list) (set! *qp-width* 333) ;;(trace-all "rwb-isam.scm")

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
      ((make-list-keyifier) make-list-keyifier)
      ((make-key->list) make-key->list)
      ((make-key-extractor) make-key-extractor)
      ((supported-type?) supported-type?)
      ((supported-key-type?) supported-key-type?)
      ((present?) present?)
      ((make-putter) make-putter)
      ((make-getter) make-getter)
      ((delete) delete)
      ((delete*) delete*)
      ((for-each-key) ordered-for-each-key)
      ((map-key) map-key)
      ((ordered-for-each-key) ordered-for-each-key)
      ((catalog-id) catalog-id)
      (else #f)))))

(set! *base-table-implementations*
      (cons (list 'wb-table (make-relational-system wb-table))
	    *base-table-implementations*))
