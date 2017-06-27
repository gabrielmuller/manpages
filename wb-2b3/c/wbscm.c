/* wbscm.c: Scheme interface to WB-tree functions
 * Copyright (C) 1991, 1992, 1993, 2000 Free Software Foundation, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

#include "scm.h"
/* #include "setjump.h" */
#undef file_position
#include "wbsys.h"
FILE *diagout;

static char s_iwb[] = "init-wb";
SCM iwb(max_ents, max_buks, max_size)
     SCM max_ents, max_buks, max_size;
{
  ASRTER(INUMP(max_ents), max_ents, ARG1, s_iwb);
  ASRTER(INUMP(max_buks), max_buks, ARG2, s_iwb);
  ASRTER(INUMP(max_size), max_size, ARG3, s_iwb);
  diagout = stdout;
  return MAKINUM(init_wb(INUM(max_ents), INUM(max_buks), INUM(max_size)));
}

SCM fwb()
{
  return MAKINUM(final_wb());
}
void final_db()
{
  final_wb();
}

long tc16_hand, tc16_segd;

#define HANP(x) (tc16_hand==TYP16(x))
#define SEGP(x) (tc16_segd==TYP16(x))

typedef struct lhand {
  HAND hand;
  SCM seg;
} LHAND;

#define LSEG(x) ((SEGD *)CDR(x))
#define LHAN(x) ((LHAND *)CDR(x))
#define LHANHAN(x) (&(LHAN(x)->hand))
#define LHANSEG(x) (LHAN(x)->seg)

int prinseg(exp, port, writing)
     SCM exp; SCM port; int writing;
{
  lputs("#<seg ", port);
  scm_intprint(LSEG(exp)->ID, 10, port);
  lputc('>', port);
  return 1;
}
int prinhan(exp, port, writing)
     SCM exp; SCM port; int writing;
{
  lputs("#<han ", port);
  scm_intprint(CDR(exp), -16, port);
  lputc('>', port);
  return 1;
}

SCM markhan(ptr)
     SCM ptr;
{
  return LHANSEG(ptr);
}
sizet freehan(p)
     CELLPTR p;
{
  must_free(CHARS((SCM)p), sizeof(LHAND));
  return sizeof(LHAND);
}
sizet freeseg(p)
     CELLPTR p;
{
  close_seg(LSEG((SCM)p), 1);
  return sizeof(SEGD);
}

static smobfuns segdsmob = {mark0, freeseg, prinseg, 0L};
static smobfuns handsmob = {markhan, freehan, prinhan, 0L};

SCM makhand(lseg)
     SCM lseg;
{
  SCM s;
  DEFER_INTS;
  s = must_malloc_cell(sizeof(LHAND)+0L, tc16_hand, "WB handle");
  LHAN(s)->seg = lseg;
  ALLOW_INTS;
  return s;
}

SCM makseg(lseg)
     SEGD *lseg;
{
  SCM z;
  if (!lseg) return BOOL_F;
  NEWCELL(z);
  DEFER_INTS;
  SETCHARS(z, lseg);
  CAR(z) = tc16_segd;
  ALLOW_INTS;
  return z;
}

static char s_han_seg[]="han:seg";
SCM lhan_seg(han)
     SCM han;
{
  ASRTER(NIMP(han) && HANP(han), han, ARG1, s_han_seg);
  return LHANSEG(han);
}

static char s_han_id[]="han:id";
SCM lhan_id(han)
     SCM han;
{
  ASRTER(NIMP(han) && HANP(han), han, ARG1, s_han_id);
  return ulong2num(LHANHAN(han)->ID);
}

static char s_seg_str[]="seg:str";
SCM lseg_str(seg)
     SCM seg;
{
  ASRTER(NIMP(seg) && SEGP(seg), seg, ARG1, s_seg_str);
  return makfrom0str(seg_str(LSEG(seg)));
}

static char s_seg_mutable_P[]="seg:mutable?";
SCM lseg_mutable_P(seg)
     SCM seg;
{
  ASRTER(NIMP(seg) && SEGP(seg), seg, ARG1, s_seg_mutable_P);
  return seg_mutable_P(LSEG(seg)) ? BOOL_T : BOOL_F;
}

static char s_open_seg[]="open-seg";
SCM lopen_seg(filename, mode)
     SCM filename, mode;
{
  SEGD *seg;
  ASRTER(NIMP(filename) && STRINGP(filename), filename, ARG1, s_open_seg);
  seg = open_seg(UCHARS(filename), !(BOOL_F==mode || INUM0==mode));
  return makseg(seg);
}

static char s_make_seg[]="make-seg";
SCM lmake_seg(filename, bsiz)
     SCM filename, bsiz;
{
  SEGD *seg;
  ASRTER(NIMP(filename) && STRINGP(filename), filename, ARG1, s_make_seg);
  ASRTER(INUMP(bsiz), bsiz, ARG2, s_make_seg);
  seg = make_seg(UCHARS(filename), INUM(bsiz));
  return makseg(seg);
}

static char s_close_seg[]="close-seg";
SCM lclose_seg(seg, hammer)
     SCM seg, hammer;
{
  ASRTER(NIMP(seg) && SEGP(seg), seg, ARG1, s_close_seg);
  return MAKINUM(close_seg(LSEG(seg), NFALSEP(hammer)));
}

static char s_open_bt[]="open-bt";
SCM lopen_bt(seg, blknum, wcb)
     SCM seg, blknum, wcb;
{
  SCM bthan;
  ASRTER(NIMP(seg) && SEGP(seg), seg, ARG1, s_open_bt);
  ASRTER(INUMP(blknum), blknum, ARG2, s_open_bt);
  ASRTER(INUMP(wcb), wcb, ARG3, s_open_bt);
  bthan=makhand(seg);
  if (!err_P(bt_open(LSEG(seg), INUM(blknum), LHANHAN(bthan), INUM(wcb))))
    return bthan;
  else return BOOL_F;
}

static char s_create_bt[]="create-bt";
SCM lcreate_bt(seg, typ, wcb)
     SCM seg, typ, wcb;
{
  SCM bthan;
  ASRTER(NIMP(seg) && SEGP(seg), seg, ARG1, s_create_bt);
  ASRTER(ICHRP(typ), typ, ARG2, s_create_bt);
  ASRTER(INUMP(wcb), wcb, ARG3, s_create_bt);
  bthan=makhand(seg);
  if (!err_P(bt_create(LSEG(seg), ICHR(typ), LHANHAN(bthan), INUM(wcb))))
    return bthan;
  else return BOOL_F;
}

static char s_close_bt[]="close-bt!";
SCM lclose_bt(bthan)
     SCM bthan;
{
  ASRTER(NIMP(bthan) && HANP(bthan), bthan, ARG1, s_close_bt);
  bt_close(LHANHAN(bthan));
  return UNSPECIFIED;
}

int wrapproc(keystr, klen, vstr, vlen, long_tab)
     unsigned char *keystr;
     int klen;
     unsigned char *vstr;
     int vlen;
     unsigned long *long_tab;
{
  /* put in dynwinds = ... to return unkerr and not allow reentry to wrapproc */
  SCM res = apply((SCM)long_tab,
		  makfromstr(keystr, klen),
		  cons(makfromstr(vstr, vlen), listofnull));
  if INUMP(res) return INUM(res);
  if (BOOL_F==res) return notpres;
  if (BOOL_T==res) return success;
  if (IMP(res) || !STRINGP(res)) return typerr;
  {
    int i = LENGTH(res);
    if (i > 255) return typerr;
    while (i--) vstr[i] = CHARS(res)[i];
    return LENGTH(res);
  }
}

/* lscan(bthan, op, key1, key2, scmproc, blklim)
 returns a list of the success code, record count, and updated key. */


static char s_bt_scan[]="bt:scan";
SCM lscan(bthan, op, args)
     SCM bthan, op, args;
{
  SCM key1, key2, scmproc, blklim;
  char ikey[256];
  int ipkt[pkt_size], res, klen2;
  pkt_set_skey_count(ipkt, 0);
  ASRTER(4==ilength(args), args, WNA, s_bt_scan);
  ASRTER(NIMP(bthan) && HANP(bthan), bthan, ARG1, s_bt_scan);
  ASRTER(INUMP(op), op, ARG2, s_bt_scan);
  key1 = CAR(args); args = CDR(args);
  if FALSEP(key1) {
      key1 = nullstr;
      pkt_set_skey_len(ipkt, LENGTH(key1));
    } else {
    ASRTER(NIMP(key1) && STRINGP(key1), key1, ARG3, s_bt_scan);
    pkt_set_skey_len(ipkt, LENGTH(key1));
    memcpy(ikey, CHARS(key1), LENGTH(key1));
  }
  key2 = CAR(args); args = CDR(args);
  if FALSEP(key2) {key2 = nullstr; klen2 = end_of_chain;}
  else {
    ASRTER(NIMP(key2) && STRINGP(key2), key2, ARG4, s_bt_scan);
    klen2 = LENGTH(key2);
  }
  scmproc = CAR(args); args = CDR(args);
  ASRTER(FALSEP(scmproc) || (NIMP(scmproc) && BOOL_T==procedurep(scmproc)),
	 scmproc, ARG5, s_bt_scan);
  blklim = CAR(args); args = CDR(args);
  ASRTER(INUMP(blklim), blklim, ARG5, s_bt_scan);
  res = bt_scan((HAND*)CHARS(bthan), INUM(op),
		ikey, pkt_skey_len(ipkt),
		UCHARS(key2), klen2,
		FALSEP(scmproc) ? 0L : wrapproc, scmproc,
		ipkt, INUM(blklim));
  return cons2(MAKINUM(res),
	       MAKINUM(pkt_skey_count(ipkt)),
	       cons(makfromstr(ikey, pkt_skey_len(ipkt)), EOL));
}

static char s_bt_get[]="bt:get";
SCM lbt_get(bthan, key)
     SCM bthan, key;
{
  unsigned char buff[256];
  ASRTER(NIMP(bthan) && HANP(bthan), bthan, ARG1, s_bt_get);
  ASRTER(NIMP(key) && STRINGP(key), key, ARG2, s_bt_get);
  {
    int tlen = bt_get(LHANHAN(bthan), UCHARS(key), LENGTH(key), buff);
    if (tlen >= 0) return makfromstr(buff, tlen);
    return BOOL_F;
  }
}

static char s_bt_next[]="bt:next";
SCM lbt_next(bthan, key)
     SCM bthan, key;
{
  unsigned char buff[256];
  int klen;
  ASRTER(NIMP(bthan) && HANP(bthan), bthan, ARG1, s_bt_next);
  if FALSEP(key) {key=nullstr; klen = start_of_chain;}
  else {
    ASRTER(NIMP(key) && STRINGP(key), key, ARG2, s_bt_next);
    klen = LENGTH(key);
  }
  /* if (!klen) {key=nullstr; klen = start_of_chain; printf("NEXT key=0\n");} */
  {
    int tlen = bt_next(LHANHAN(bthan), UCHARS(key), klen, buff);
    if (tlen >= 0) return makfromstr(buff, tlen);
    return BOOL_F;
  }
}

static char s_bt_prev[]="bt:prev";
SCM lbt_prev(bthan, key)
     SCM bthan, key;
{
  unsigned char buff[256];
  int klen;
  ASRTER(NIMP(bthan) && HANP(bthan), bthan, ARG1, s_bt_prev);
  if FALSEP(key) {key=nullstr; klen = end_of_chain;}
  else {
    ASRTER(NIMP(key) && STRINGP(key), key, ARG2, s_bt_prev);
    klen = LENGTH(key);
  }
  /* if (!klen) {key=nullstr; klen = end_of_chain; printf("PREV key=0\n");} */
  {
    int tlen = bt_prev(LHANHAN(bthan), UCHARS(key), klen, buff);
    if (tlen >= 0) return makfromstr(buff, tlen);
    return BOOL_F;
  }
}

static char s_bt_rem[]="bt:rem!";
SCM lbt_rem(bthan, key)
     SCM bthan, key;
{
  ASRTER(NIMP(bthan) && HANP(bthan), bthan, ARG1, s_bt_rem);
  ASRTER(NIMP(key) && STRINGP(key), key, ARG2, s_bt_rem);
  if (!bt_rem(LHANHAN(bthan), UCHARS(key), LENGTH(key), 0L))
    return BOOL_T;
  else return BOOL_F;
}

static char s_bt_read[]="bt:rem";
SCM lbt_read(bthan, key)
     SCM bthan, key;
{
  unsigned char buff[256];
  int tlen;
  ASRTER(NIMP(bthan) && HANP(bthan), bthan, ARG1, s_bt_read);
  ASRTER(NIMP(key) && STRINGP(key), key, ARG2, s_bt_read);
  tlen = bt_rem(LHANHAN(bthan), UCHARS(key), LENGTH(key), buff);
  if (tlen >= 0) return makfromstr(buff, tlen);
  return BOOL_F;
}

static char s_bt_rem_star[]="bt:rem*";
SCM lbt_rem_star(bthan, key, key2)
     SCM bthan, key, key2;
{
  char tmpstr[256];
  int klen, klen2;
  ASRTER(NIMP(bthan) && HANP(bthan), bthan, ARG1, s_bt_rem_star);
  if FALSEP(key) {key = nullstr; klen = start_of_chain;}
  else {
    ASRTER(NIMP(key) && STRINGP(key), key, ARG2, s_bt_rem_star);
    klen = LENGTH(key);
  }
  /* if (!klen) {key=nullstr; klen = start_of_chain;} */
  if (klen > 0) memcpy(tmpstr, CHARS(key), klen);
  if FALSEP(key2) {key2 = nullstr; klen2 = end_of_chain;}
  else {
    ASRTER(NIMP(key2) && STRINGP(key2), key2, ARG3, s_bt_rem_star);
    klen2 = LENGTH(key2);
  }
  /* if (!klen2) {key2=nullstr; klen2 = end_of_chain;} */
  if (!bt_rem_range(LHANHAN(bthan), tmpstr, klen, UCHARS(key2), klen2))
    return BOOL_T;
  else return BOOL_F;
}

static char s_bt_put[]="bt:put!";
SCM lbt_put(bthan, key, val)
     SCM bthan, key, val;
{
  ASRTER(NIMP(bthan) && HANP(bthan), bthan, ARG1, s_bt_put);
  ASRTER(NIMP(key) && STRINGP(key), key, ARG2, s_bt_put);
  ASRTER(NIMP(val) && STRINGP(val), val, ARG3, s_bt_put);
  if (!bt_put(LHANHAN(bthan),
	      UCHARS(key), LENGTH(key),
	      UCHARS(val), LENGTH(val)))
    return BOOL_T;
  else return BOOL_F;
}

static char s_bt_write[]="bt:put";
SCM lbt_write(bthan, key, val)
     SCM bthan, key, val;
{
  ASRTER(NIMP(bthan) && HANP(bthan), bthan, ARG1, s_bt_write);
  ASRTER(NIMP(key) && STRINGP(key), key, ARG2, s_bt_write);
  ASRTER(NIMP(val) && STRINGP(val), val, ARG3, s_bt_write);
  if (!bt_write(LHANHAN(bthan),
		UCHARS(key), LENGTH(key),
		UCHARS(val), LENGTH(val)))
    return BOOL_T;
  else return BOOL_F;
}

static char s_create_db[]="create-db";
SCM lcreate_db(seg, typ, name)
     SCM seg, typ, name;
{
  SCM a_han;
  SCM d_han;
  SCM tmp_str=makstr(5);
  ASRTER(NIMP(seg) && SEGP(seg), seg, ARG1, s_create_db);
  ASRTER(ICHRP(typ), typ, ARG2, s_create_db);
  ASRTER(NIMP(name) && STRINGP(name), name, ARG3, s_create_db);
  a_han=lcreate_bt(seg, typ, INUM0);
  d_han=lopen_bt(seg, MAKINUM(1), INUM0);
  if (FALSEP(a_han) || FALSEP(d_han)) return BOOL_F;
  CHARS(tmp_str)[0]=4;
  long2str(UCHARS(tmp_str), 1, han_id(LHANHAN(a_han)));
  lbt_put(d_han, name, tmp_str);
  lclose_bt(d_han);
  return a_han;
}

static char s_open_db[]="open-db";
SCM lopen_db(seg, name)
     SCM seg, name;
{
  SCM d_han, nn;
  ASRTER(NIMP(seg) && SEGP(seg), seg, ARG1, s_open_db);
  ASRTER(NIMP(name) && STRINGP(name), name, ARG2, s_open_db);
  d_han=lopen_bt(seg, MAKINUM(1), INUM0);
  nn = lbt_get(d_han, name);
  if (NIMP(nn) && STRINGP(nn) && (LENGTH(nn)>4) && (CHARS(nn)[0]==4))
    return lopen_bt(seg, MAKINUM(str2long(UCHARS(nn)+1, 0)), INUM0);
  else return BOOL_F;
}

static char s_flush_ents[] = "flush-ents";
SCM lflush_ents(numtry, numflush)
     SCM numtry, numflush;
{
  ASRTER(INUMP(numtry), numtry, ARG1, s_flush_ents);
  ASRTER(INUMP(numflush), numflush, ARG2, s_flush_ents);
  return MAKINUM(ents_flush(INUM(numtry), INUM(numflush)));
}

SCM lcheck_access()
{
  check_access();
  return UNSPECIFIED;
}

SCM lclear()
{
  clear_stats();
  return UNSPECIFIED;
}

SCM lstats()
{
  stats();
  return UNSPECIFIED;
}

SCM lcstats()
{
  cstats();
  return UNSPECIFIED;
}

SCM lsb()
{
  sb();
  return UNSPECIFIED;
}

static char s_s2l[] = "str2long";
SCM s2l(str, pos)
     SCM str, pos;
{
  ASRTER(NIMP(str) && STRINGP(str), str, ARG1, s_s2l);
  ASRTER(INUMP(pos), pos, ARG2, s_s2l);
  ASRTER(LENGTH(str) >= INUM(pos) + 4, pos, OUTOFRANGE, s_s2l);
#ifdef BIGDIG
  {
    unsigned long sl = str2long(CHARS(str), INUM(pos));
    if (!POSFIXABLE(sl)) return long2big(sl);
    return MAKINUM(sl);
  }
#else
  return MAKINUM(str2long(CHARS(str), INUM(pos)));
#endif
}

static char s_l2s[] = "long2str!";
SCM l2s(str, pos, clong)
     SCM str, pos, clong;
{
  unsigned long clng = 0;
  ASRTER(NIMP(str) && STRINGP(str), str, ARG1, s_l2s);
  ASRTER(INUMP(pos), pos, ARG2, s_l2s);
  ASRTER(NUMBERP(clong), clong, ARG3, s_l2s);
  ASRTER(LENGTH(str) >= INUM(pos) + 4, pos, OUTOFRANGE, s_l2s);
#ifdef BIGDIG
  if NINUMP(clong) {
    sizet l;
    ASRTER(NIMP(clong) && TYP16(clong)==tc16_bigpos, clong, ARG1, s_l2s);
    for (l = NUMDIGS(clong);l--;) clng = BIGUP(clng) + BDIGITS(clong)[l];
  }
  else
#else
    ASRTER(INUMP(clong), clong, ARG1, s_l2s);
#endif
  clng = INUM((unsigned long)clong);
  long2str(CHARS(str), INUM(pos), clng);
  return UNSPECIFIED;
}

SCM wb_err_P(x)
     SCM x;
{
  if (INUMP(x)) return err_P(INUM(x)) ? BOOL_T : BOOL_F;
  return BOOL_F;
}

static iproc subr0s[]={
	{"final-wb", fwb},
	{"check-access!", lcheck_access},
	{"clear-stats", lclear},
	{"stats", lstats},
	{"cstats", lcstats},
	{"show-buffers", lsb},
	{0, 0}};

static iproc subr1s[]={
	{s_close_bt, lclose_bt},
	{s_han_seg, lhan_seg},
	{s_han_id, lhan_id},
	{s_seg_str, lseg_str},
	{s_seg_mutable_P, lseg_mutable_P},
	{"wb:err?", wb_err_P},
	{0, 0}};

static iproc subr2s[]={
	{s_open_seg, lopen_seg},
	{s_make_seg, lmake_seg},
	{s_close_seg, lclose_seg},
	{s_bt_get, lbt_get},
	{s_bt_next, lbt_next},
	{s_bt_prev, lbt_prev},
	{s_bt_rem, lbt_rem},
	{s_bt_read, lbt_read},
	{s_open_db, lopen_db},
	{s_flush_ents, lflush_ents},
	{s_s2l, s2l},
	{0, 0}};

static iproc subr3s[]={
	{s_iwb, iwb},
	{s_open_bt, lopen_bt},
	{s_create_bt, lcreate_bt},
	{s_bt_put, lbt_put},
	{s_bt_write, lbt_write},
	{s_create_db, lcreate_db},
	{s_bt_rem_star, lbt_rem_star},
	{s_l2s, l2s},
	{0, 0}};

void init_db()
{
  tc16_hand = newsmob(&handsmob);
  tc16_segd = newsmob(&segdsmob);
  init_iprocs(subr0s, tc7_subr_0);
  init_iprocs(subr1s, tc7_subr_1);
  init_iprocs(subr2s, tc7_subr_2);
  init_iprocs(subr3s, tc7_subr_3);
  make_subr(s_bt_scan, tc7_lsubr_2, lscan);
  add_feature(s_open_db+5);	/* db */
  add_feature(s_iwb+5);		/* wb */
  add_final(final_db);
  scm_ldstr("\n\
\n\
(define (db:put! han ckey val)\n\
  (define len (string-length val))\n\
  (define (put! han ckey val)\n\
    (and (wb:err? (bt:put! han ckey val))\n\
	 (slib:error 'db:put 'could-not-put han ckey val)))\n\
  (cond ((> len 255)\n\
	 (put! han ckey (substring val 0 255))\n\
	 (do ((kdx 1 (+ 1 kdx))\n\
	      (idx 510 (+ 255 idx))\n\
	      (ldx 255 idx))\n\
	     ((>= idx len)\n\
	      (put! han (string-append ckey (bytes kdx))\n\
		    (substring val ldx len)))\n\
	   (put! han (string-append ckey (bytes kdx))\n\
		 (substring val ldx idx))))\n\
	(else (put! han ckey val))))\n\
\n\
(define (db:get han ckey)\n\
  (define val (bt:get han ckey))\n\
  (and val\n\
       (case (string-length val)\n\
	 ((255)\n\
	  (let ((ckey+ (string-append ckey (bytes 255)))\n\
		(strs (list val)))\n\
	    (define bytes:0 (bytes 0))\n\
	    (define tail strs)\n\
	    (define (maproc k v)\n\
	      (set-cdr! tail (list v))\n\
	      (set! tail (cdr tail))\n\
	      #t)\n\
	    (do ((res (bt:scan han 0 (string-append ckey bytes:0) ckey+\n\
			       maproc 1)\n\
		      (bt:scan han 0 (caddr res) ckey+ maproc 1)))\n\
		((not (= -1 (car res)))\n\
		 (apply string-append strs)))))\n\
	 (else val))))\n\
");
}

void init_wbscm()
{
  init_db();
}
