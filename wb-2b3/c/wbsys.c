/* WB-tree File Based Associative String Data Base System.
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

#include "wbsys.h"
#include "wbdefs.h"

#ifdef _MSC_VER
# include <process.h>
# include <windows.h>
# include <Winbase.h>
# include <intrin.h>
# pragma intrinsic (_InterlockedExchange)
#endif

#ifdef PLAN9
int errno;
#else
# include <errno.h>
#endif

LCK *last_lck=0;

LCK *make_lck(int name)
{
  LCK *l = (LCK *)malloc(sizeof (LCK));
  if (!l) {
    dprintf((diagout, ">>>>ERROR<<<< could not allocate lck\n"));
    exit(errno);
  }
  l->NEXT = last_lck;

#ifdef _MSC_VER
  l->FLG = 0;
#else
# ifdef unix
  pthread_mutex_init(&l->FLG, NULL);
# else
  l->FLG = 0;
# endif
#endif

  l->NAME = name;
  last_lck=l;
  return l;
}

/* #define try_lck_P(lk) (!((lk)->FLG--)) */
int try_lck_P(LCK *lk)
{
#ifdef _MSC_VER
  return 0==_InterlockedExchange(&lk->FLG, 1);
#else
# ifdef unix
  return !pthread_mutex_trylock(&lk->FLG);
# else
  return !(lk->FLG--);
# endif
#endif
}

void lck(LCK *lk)
{
#ifdef _MSC_VER
  while (1==_InterlockedExchange(&lk->FLG, 1))
    dprintf((diagout,">>>>ERROR<<<< spinning on lck %d\n",(lk)->NAME));
#else
# ifdef unix
  pthread_mutex_lock(&lk->FLG);
# else
  if (lk->FLG--) dprintf((diagout,">>>>ERROR<<<< lck %d already locked\n",(lk)->NAME));
# endif
#endif
}

void unlck(LCK *lk)
{
#ifdef _MSC_VER
  if (0==_InterlockedExchange(&lk->FLG, 0))
    dprintf((diagout,">>>>ERROR<<<< unlcking unlck %d\n",lk->NAME));
#else
# ifdef unix
  pthread_mutex_unlock(&lk->FLG);
# else
  if (lk->FLG) lk->FLG=0;
  else dprintf((diagout,">>>>ERROR<<<< unlcking unlck %d\n",lk->NAME));
# endif
#endif
}

void check_lcks(void)
{
  LCK *ll = last_lck;
  while(ll) {
    if (!try_lck_P(ll)) {
      if (ll->NAME < 0)
	switch (ll->NAME) {
	  case -1:dprintf((diagout,">>>>ERROR<<<< free-ent-lck left lcked\n"));
	  case -2:dprintf((diagout,">>>>ERROR<<<< flush-buk-lck left lcked\n"));
	  case -3:dprintf((diagout,">>>>ERROR<<<< seg-chain-lck left lcked\n"));
	  default:dprintf((diagout,">>>>ERROR<<<< unknown lck left lcked\n"));
	}
      else dprintf((diagout,">>>>ERROR<<<< lck %d left lcked\n", ll->NAME));
    }
    unlck(ll);
    ll = ll->NEXT;
  }
}


SEGD* new_segd(int idx)
{
  SEGD *seg = (SEGD *)calloc(1,sizeof(SEGD));
  seg->RT_HAN = make_hand();
  seg->FL_HAN = make_hand();
  seg->FLCK = make_lck( 0-(idx));
  seg->FFCK = make_lck(-1-(idx));
  seg->ID = idx;
  return seg;
}

ENTRY *ent_make_ent(int tag)
{
  ENTRY *ent = (ENTRY *)malloc(sizeof (ENTRY));
  if (!ent) {
    dprintf((diagout, "WARNING: could not allocate entry\n"));
    return 0;
  }
  ent->TAG = tag;
  ent->NEXT = 0;
  ent->SEG = 0;
  ent->ID = -1;
  ent->BLK = (unsigned char *)malloc(blk_size);
  if (!ent->BLK) {
    dprintf((diagout, "WARNING: could not allocate blk for entry\n"));
    free(ent);
    return 0;
  }
  ent->AGE = 0;
  ent->DTY = 0;
  ent->PUS = 0;
  ent->ACC = accnone;
  ent->REF = 0;
  return ent;
}

HAND *make_hand(void)
{
  HAND *han;
  han = (HAND *)calloc(1,sizeof(HAND));
  if (!han) {
    dprintf((diagout, ">>>>ERROR<<<< could not allocate handle\n"));
    exit(errno);
  }
/*
  han_set_num(han, 0);
  han_set_seg(han, 0);
  han_set_typ(han, 0);
  han_set_last(han, 0);
*/
  return han;
}
