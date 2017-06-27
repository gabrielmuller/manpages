/* WB-tree File Based Associative String Data Base System.
 * Copyright (C) 1991, 1992, 1993, 2000, 2003 Free Software Foundation, Inc.
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

#include "schleprt.h"

typedef int (*int_function)();

#ifdef _MSC_VER
# define PTHREAD_MUTEX_INITIALIZER 0
#else
# ifdef unix
#  include <pthread.h>
# else
#  define PTHREAD_MUTEX_INITIALIZER 0
# endif
#endif

typedef struct slck {
  struct slck *NEXT;
  int NAME;
#ifdef _MSC_VER
  volatile int FLG;
#else
# ifdef unix
  pthread_mutex_t FLG;
# else
  int FLG;
# endif
#endif
} LCK;

extern LCK *last_lck;

LCK *make_lck(int name);
int try_lck_P(LCK *lk);
void lck(LCK *lk);
void unlck(LCK *lk);
void check_lcks(void);

typedef struct shand {
  struct ssegd *SEG;
  long ID, LAST;
  int TYP, WCB;
} HAND;

HAND *make_hand(void);

#define han_id(han) (((HAND *)(han))->ID)
#define han_seg(han) ((SEGD *)(((HAND *)(han))->SEG))
#define han_typ(han) (((HAND *)(han))->TYP)
#define han_last(han) (((HAND *)(han))->LAST)
#define han_wcb(han) (((HAND *)(han))->WCB)

#define han_set_num(han,num) {((HAND *)(han))->ID = (num);}
#define han_set_seg(han,seg) {((HAND *)(han))->SEG = (seg);}
#define han_set_typ(han,typ) {((HAND *)(han))->TYP = (typ);}
#define han_set_last(han,last) {((HAND *)(han))->LAST = (last);}
#define han_set_wcb(han,wcb) {((HAND *)(han))->WCB = (wcb);}

typedef struct ssegd{
  int PORT, BSIZ;
  long USED;
  char *STRN;
  HAND *RT_HAN, *FL_HAN;
  LCK *FLCK;
  LCK *FFCK;
  int FLC_LEN;
  long *FLC;
  struct ssegd *PRV;
  struct ssegd *NXT;
  long ID;
} SEGD;

SEGD* new_segd(int idx);

/*  The SEG (segment) structure */

#define seg_port(seg) ((seg)->PORT)
#define seg_bsiz(seg) ((seg)->BSIZ)
#define seg_used(seg) ((seg)->USED)
#define seg_str(seg) ((seg)->STRN)
#define seg_rt_han(seg) ((seg)->RT_HAN)
#define seg_fl_han(seg) ((seg)->FL_HAN)
#define seg_lck(seg) ((seg)->FLCK)
#define seg_fck(seg) ((seg)->FFCK)
#define seg_flc_len(seg) ((seg)->FLC_LEN)
#define seg_flc(seg) ((seg)->FLC)
#define seg_prv(seg) ((seg)->PRV)
#define seg_nxt(seg) ((seg)->NXT)
#define seg_id(seg) ((seg)?(seg)->ID:-1L)

#define seg_mutable_P(seg) (-2 != seg_flc_len(seg))

#define seg_set_port(seg,port) {(seg)->PORT = port;}
#define seg_set_bsiz(seg,bsiz) {(seg)->BSIZ = bsiz;}
#define seg_set_used(seg,used) {(seg)->USED = used;}
#define seg_set_str(seg,strn) {(seg)->STRN = strn;}
#define seg_set_flc_len(seg,len) {(seg)->FLC_LEN = len;}
#define seg_set_flc(seg,flc) {(seg)->FLC = flc;}
#define seg_set_prv(seg,prv) {(seg)->PRV = (prv);}
#define seg_set_nxt(seg,nxt) {(seg)->NXT = (nxt);}

typedef struct entry {
  struct entry *NEXT;
  long ID;
  unsigned char *BLK;
  int TAG, AGE, DTY, PUS, ACC, REF;
  SEGD *SEG;
} ENTRY;

ENTRY *ent_make_ent(int tag);

#define ent_tag(ent) ((ent)->TAG)
#define ent_next(ent) ((ent)->NEXT)
#define ent_seg(ent) ((ent)->SEG)
#define ent_id(ent) ((ent)->ID)
#define ent_blk(ent) ((ent)->BLK)
#define ent_age(ent) ((ent)->AGE)
#define ent_dty_P(ent) ((ent)->DTY)
#define ent_pus(ent) ((ent->PUS))
#define ent_acc(ent) ((ent)->ACC)
#define ent_ref(ent) ((ent)->REF)

#define ent_set_tag(ent, tag) {(ent)->TAG = tag;}
#define ent_set_next(ent, next) {(ent)->NEXT = next;}
#define ent_set_seg(ent, seg) {(ent)->SEG = seg;}
#define ent_set_id(ent, num) {(ent)->ID = num;}
#define ent_set_age(ent, age) {(ent)->AGE = age;}
#define ent_set_dty(ent, dty) {(ent)->DTY = dty;}
#define ent_set_pus(ent, pus) {(ent)->PUS = pus;}
#define ent_set_acc(ent, acc) {(ent)->ACC = acc;}
#define ent_set_ref(ent, ref) {(ent)->REF = ref;}

/*  BLK parameters */

/*  The IDs are 4 byte numbers identifying this block, the root of */
/*  this tree, and the next in the chain. */

#define blk_id_pos 0
#define blk_top_id_pos 4
#define blk_nxt_id_pos 8
#define blk_time_pos 0xc
/*  blk-end-pos is position (stored in 2 bytes) of first free byte */
#define blk_end_pos 0x10
#define blk_level_pos 0x12
#define blk_typ_pos 0x13
#define blk_data_start 0x14

#define blk_id(blk) (str2long(blk, blk_id_pos))
#define blk_top_id(blk) (str2long(blk, blk_top_id_pos))
#define blk_nxt_id(blk) (str2long(blk, blk_nxt_id_pos))
#define blk_time(blk) (str2long(blk, blk_time_pos))
#define blk_end(blk) (str2short(blk, blk_end_pos))
#define blk_level(b) ((((unsigned char*)(b))[blk_level_pos]))
#define blk_typ(b) ((((unsigned char*)(b))[blk_typ_pos]))
#define blk_typ_P(b, typ) (((((unsigned char*)(b))[blk_typ_pos]))==(typ))

#define blk_set_id(blk, id) {long2str(blk, blk_id_pos, id);}
#define blk_set_top_id(blk, id) {long2str(blk, blk_top_id_pos, id);}
#define blk_set_nxt_id(blk, id) {long2str(blk, blk_nxt_id_pos, id);}
#define blk_set_time(blk, tim) {long2str(blk, blk_time_pos, tim);}
#define blk_set_end(blk, pos) {short2str(blk, blk_end_pos, pos);}
#define blk_set_level(b, level) {b[blk_level_pos] = level;}
#define blk_set_typ(b, typ) {b[blk_typ_pos] = typ;}

/*  Routines for finding the appropriate BLK for an operation. */
/*  PACKETs used to return multiple values from chain-find. */
/*  and various other operations */
#define pkt_size 6

#define pkt_match_type(p) (p[0])
#define pkt_match_pos(p) (p[1])
#define pkt_key_pos(p) (p[2])
#define pkt_prev_match_pos(p) (p[3])
#define pkt_blk_to_cache(p) (p[4])
#define pkt_success_code(p) (p[5])

#define pkt_set_match_type(p, v) {p[0] = v;}
#define pkt_set_match_pos(p, v) {p[1] = v;}
#define pkt_set_key_pos(p, v) {p[2] = v;}
#define pkt_set_prev_match_pos(p, v) {p[3] = v;}
#define pkt_set_blk_to_cache(p, v) {p[4] = v;}
#define pkt_set_success_code(p, v) {p[5] = v;}

#define pkt_pack(p, type, b_pos, k_pos, p_pos) {pkt_set_match_type(p, type);pkt_set_match_pos(p, b_pos);pkt_set_key_pos(p, k_pos);pkt_set_prev_match_pos(p, p_pos);}

/*  Aliased function names for SCAN */
#define pkt_skey_count pkt_match_pos
#define pkt_set_skey_count pkt_set_match_pos
#define pkt_skey_len pkt_key_pos
#define pkt_set_skey_len pkt_set_key_pos

#include "wbdefs.h"
#include "ents.h"
#include "blink.h"
#include "handle.h"
#include "segs.h"
#include "prev.h"
#include "del.h"
#include "stats.h"
#include "blkio.h"
#include "scan.h"
#include "db.h"
