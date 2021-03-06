/* This file was generated by scm2java from source file "blink.scm" */
/*  "blink.scm" WB-tree File Based Associative String Data Base System. */
/*  Copyright (C) 1991, 1992, 1993, 2000, 2003, 2010 Free Software Foundation, Inc. */
/*  */
/*  This program is free software: you can redistribute it and/or modify */
/*  it under the terms of the GNU Lesser General Public License as */
/*  published by the Free Software Foundation, either version 3 of the */
/*  License, or (at your option) any later version. */
/*  */
/*  This program is distributed in the hope that it will be useful, but */
/*  WITHOUT ANY WARRANTY; without even the implied warranty of */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU */
/*  Lesser General Public License for more details. */
/*  */
/*  You should have received a copy of the GNU Lesser General Public */
/*  License along with this program.  If not, see */
/*  <http://www.gnu.org/licenses/>. */

/*  TBD: */
/*  allow different size blks for index and leaves. */
/*  add multi-record operations */

/*  uncomment for newlines at end of blocks: */

/*  uncomment for extra check in REROOT! and path tracing in BLK:FIND-POS. */
/* (provide 'DEBUG) */



package wb;

import static wb.Ents.*;
import static wb.Stats.*;
import static wb.Wbdefs.*;
import static wb.Wbsys.*;
import static wb.Blk.*;
import static wb.SchlepRT.*;
import static wb.Pkt.*;
import static wb.Ent.*;
import static wb.Seg.*;
public class Blink {

/*  BLK ACCESS AND MODIFICATION ROUTINES */


public static void short2str(byte []str,int pos,int cint)
{
  str[(pos)+1] = (byte) (255&((cint)&255));
  str[(pos)+0] = (byte) (255&((cint)>>8));
  return;
}

public static int str2short(byte []str,int pos)
{
  return ((str[(pos)+1] & 0xFF))|(((str[pos] & 0xFF))<<8);
}


public static void long2str(byte []str,int pos,int clong)
{
  str[(pos)+3] = (byte) (255&((clong)&255));
  str[(pos)+2] = (byte) (255&(((clong)>>8)&255));
  str[(pos)+1] = (byte) (255&(((clong)>>16)&255));
  str[(pos)+0] = (byte) (255&((clong)>>24));
  return;
}

public static int str2long(byte []str,int pos)
{
  return ((str[(pos)+3] & 0xFF))|((((str[(pos)+2] & 0xFF))|((((str[(pos)+1] & 0xFF))|(((str[pos] & 0xFF))<<8))<<8))<<8);
}


public static int setField(byte []blk,int bPos,byte []valStr,int fPos,int fLen)
{
  setFieldLen(blk, bPos, fLen);
  subbytesMove(valStr, fPos, (fPos)+(fLen), blk, 1+(bPos));
  return (fLen)+1+(bPos);
}


public static byte []leafSplitKeyStr = bytes((byte)( -1),(byte)(leaf));


public static void initLeafBlk(byte []nblk,int bnum,int typ)
{
  nblk[(blkSize)-1] = (byte) (255&10);
  blk_SetId(nblk, bnum);
  blk_SetNxtId(nblk, 0);
  blk_SetTopId(nblk, bnum);
  blk_SetTime(nblk, 0);
  blk_SetLevel(nblk, leaf);
  blk_SetTyp(nblk, typ);
  setFieldLen(nblk, blkDataStart, 0);
  setField(nblk, (blkDataStart)+1, leafSplitKeyStr, 0, 2);
  blk_SetEnd(nblk, (blkDataStart)+(((typ)==(seqTyp)
       ?0
       :4)));
  return;
}

/* RBLK= the root block, NBLK= new block to hold root's data, BNUM= its ID */


public static void reroot(byte []rblk,byte []nblk,int bnum,int bsiz)
{
  int rpos = blkDataStart;
  subbytesMove(rblk, 4, bsiz, nblk, 4);
  blk_SetNxtId(rblk, 0);
  blk_SetLevel(rblk, (blk_Level(rblk))+1);
  setFieldLen(rblk, rpos, 0);
  rpos = setField(rblk, (rpos)+1, leafSplitKeyStr, 0, 2);
  rblk[(rpos)-1] = (byte) (255&((blk_Level(rblk))-1));
  rpos = setField(rblk, rpos, nblk, 0, 4);
  setFieldLen(rblk, rpos, 1);
  rpos = setField(rblk, (rpos)+1, leafSplitKeyStr, 0, 1);
  rblk[(rpos)-1] = (byte) (255&(blk_Level(rblk)));
  blk_SetEnd(rblk, rpos);
  return;
}


public static void initNextBlk(byte []blk,byte []nblk)
{
  nblk[(blkSize)-1] = (byte) (255&10);
  blk_SetNxtId(nblk, blk_NxtId(blk));
  blk_SetTopId(nblk, blk_TopId(blk));
  blk_SetLevel(nblk, blk_Level(blk));
  blk_SetTyp(nblk, blk_Typ(blk));
  blk_SetNxtId(blk, blk_Id(nblk));
  setFieldLen(nblk, blkDataStart, 0);
  setField(nblk, (blkDataStart)+1, noByts, 0, 0);
  blk_SetEnd(nblk, (blkDataStart)+2);
  return;
}


public static int splitKeyPos(byte []blk)
{
  int bEnd = blk_End(blk);
  {
    int bPos = blkDataStart;
Llp: while (true) {
    {
      int sPos = nextField(blk, 1+(bPos));
      if ((sPos)==(bEnd))
	return bPos;
      else if ((sPos)<(bEnd))
	{
	  bPos = nextCnvpair(blk, bPos);
	  continue Llp;
	}
      else {
	System.err.print(">>>>ERROR<<<< "+("splitKeyPos")+": blk past end "+(blk_Id(blk))+" "+(sPos)+"\n");
	return 0;
      }
    }
    }
  }
}


/*  Pass in len; -1 to seek END-OF-CHAIN; -2 for START-OF-CHAIN. */
/*  If key-str = END-OF-CHAIN, then return PASTEND @ split-pos. */
/*  If key-str = START-OF-CHAIN, then return QPASTP @ blk-data-start. */
/*  Otherwise, can return any of 5 match conditions. */

/*  As we go through blk looking for key, KEY-POS (k-pos) is the */
/*  number of characters matching between key and blk. */


public static boolean blk_FindPos(byte []blk,byte []keyStr,int kLen,int []pkt)
{
  if ((kLen)<0)
    {
      if ((kLen)==(endOfChain))
	{
	  int skpos = splitKeyPos(blk);
	  pkt_Pack(pkt, (endOfChain_P(blk)
	      ?qpastp
	      :pastend), skpos, 0, blkPrevKey(blk, skpos));
	}
      else pkt_Pack(pkt, qpastp, blkDataStart, 0, 0);
      return (true);
    }
  else {
    int kPos = 0;
    int bEnd = blk_End(blk);
    {
      int bPos = blkDataStart;
      int pPos = 0;
Lchknxt: while (true) {
      if ((fieldLen(blk, bPos))<(kPos))
	{
	  pkt_Pack(pkt, qpastp, bPos, kPos, pPos);
	  return (true);
	}
      else if ((fieldLen(blk, bPos))>(kPos))
	{
	  int sPos = nextField(blk, (bPos)+1);
	  if ((sPos)<(bEnd))
	    {
	      int T_bPos = nextCnvpair(blk, bPos);
	      pPos = bPos;
	      bPos = T_bPos;
	      continue Lchknxt;
	    }
	  else if ((sPos)==(bEnd))
	    {
	      pkt_Pack(pkt, pastend, bPos, kPos, pPos);
	      return (true);
	    }
	  else {
	    System.err.print(">>>>ERROR<<<< "+("blkFindPos")+"1: blk past end "+(blk_Id(blk))+" "+(sPos)+"\n");
	    return false;
	  }
	}
      else {
	int i = (bPos)+2;
	int fLen = fieldLen(blk, (bPos)+1);
Lmchlp: while (true) {
	if ((kPos)>=(kLen))
	  if ((fLen)>0)
	    {
	      pkt_Pack(pkt, pastp, bPos, kPos, pPos);
	      return (true);
	    }
	  else {
	    int sPos = nextField(blk, (bPos)+1);
	    if ((sPos)<(bEnd))
	      {
		pkt_Pack(pkt, match, bPos, kLen, pPos);
		return (true);
	      }
	    else if ((sPos)==(bEnd))
	      {
		pkt_Pack(pkt, matchend, bPos, kPos, pPos);
		return (true);
	      }
	    else {
	      System.err.print(">>>>ERROR<<<< "+("blkFindPos")+"2: blk past end "+(blk_Id(blk))+" "+(sPos)+"\n");
	      return false;
	    }
	  }
	else if (((fLen)<=0
	    || ((blk[i] & 0xFF))<((keyStr[kPos] & 0xFF))))
	  {
	    int sPos = nextField(blk, (bPos)+1);
	    if ((sPos)<(bEnd))
	      {
		int T_bPos = nextCnvpair(blk, bPos);
		pPos = bPos;
		bPos = T_bPos;
		continue Lchknxt;
	      }
	    else if ((sPos)==(bEnd))
	      {
		pkt_Pack(pkt, pastend, bPos, kPos, pPos);
		return (true);
	      }
	    else {
	      System.err.print(">>>>ERROR<<<< "+("blkFindPos")+"3: blk past end "+(blk_Id(blk))+" "+(sPos)+"\n");
	      return false;
	    }
	  }
	else if (((blk[i] & 0xFF))>((keyStr[kPos] & 0xFF)))
	  {
	    pkt_Pack(pkt, ((kPos)>(fieldLen(blk, bPos))
		?pastp
		:qpastp), bPos, kPos, pPos);
	    return (true);
	  }
	else {
	  kPos = (kPos)+1;
	  {
	    i = 1+(i);
	    fLen = (fLen)-1;
	    continue Lmchlp;
	  }
	}
	}
      }
      }
    }
  }
}

/*  Can return QPASTP or PASTP @ any key or MATCH at non-split key. */


public static Ent chainFind(Ent ent,int accmode,byte []keyStr,int kLen,int []pkt)
{
LchainFind:while (true) {
  {
    byte []blk = ent_Blk(ent);
    if (!(blk_FindPos(blk, keyStr, kLen, pkt)))
      {
	releaseEnt(ent, accmode);
	return null;
      }
    else if (!(((pkt_MatchType(pkt))==(matchend)
	    || (pkt_MatchType(pkt))==(pastend))))
      return ent;
    else if (endOfChain_P(blk))
      {
	System.err.print(">>>>ERROR<<<< "+("chainFind")+": matched or past end of chain "+(seg_Id(ent_Seg(ent)))+":"+(ent_Id(ent))+"\n");
	pkt_SetMatchType(pkt, qpastp);
	return ent;
      }
    else {
      chainsToNext = 1+(chainsToNext);
      ent = switchEnt(ent, accmode, blk_NxtId(blk), accmode);
      if (a2b(ent))
	continue LchainFind;
      else return null;
    }
  }
}
}

/*  find-ent is always called with ent = (get-ent <seg> <blk-num> ACCNONE). */
/*  TBD - These calls could be colapsed. */
/*  should be called with LAST-LEVEL=-1 */


public static Ent findEnt(Ent ent,int desiredLevel,int lastLevel,byte []keyStr,int kLen)
{
LfindEnt:while (true) {
  if (a2b(ent))
    if (ents_EntUpdateAccess_P(ent, accnone, accread))
      {
	byte []blk = ent_Blk(ent);
	int blvl = blk_Level(blk);
	if ((blvl)==(desiredLevel))
	  return ent;
	else if ((blvl)<(desiredLevel))
	  {
	    System.err.print(">>>>ERROR<<<< bad blk level "+(blvl)+" (des="+(desiredLevel)+") in "+(seg_Id(ent_Seg(ent)))+":"+(ent_Id(ent))
   +"\n");
	    return null;
	  }
	else if ((lastLevel)>=0
	    && (blvl)!=((lastLevel)-1))
	  {
	    System.err.print(">>>>ERROR<<<< bad blk level "+(blvl)+" last="+(lastLevel)+" in "+(seg_Id(ent_Seg(ent)))+":"+(ent_Id(ent))
   +"\n");
	    return null;
	  }
	else {
	  int []pkt = new int[pktSize];
	  ent = chainFind(ent, accread, keyStr, kLen, pkt);
	  if (a2b(ent))
	    {
	      int pos = nextField(blk, 1+(pkt_MatchPos(pkt)));
	      blk = ent_Blk(ent);
	      switch (pkt_MatchType(pkt)) {
	      case qpastp:
	      case pastp:
		;
		break;
	      case match:
		if ((blk_End(blk))==(pos))
		  {
		    pos = pkt_MatchPos(pkt);
		  }
		else pos = nextField(blk, pos);
		break;
	      default:
		pos = 0;
		break;
	      }
	      if (0==(pos))
		{
		  System.err.print(">>>>ERROR<<<< "+("findEnt")+": bad-MATCH-TYPE "+(pkt_MatchPos(pkt))+" blk "+(seg_Id(ent_Seg(ent)))+":"+(ent_Id(ent))
   +"\n");
		  return null;
		}
	      else {
		pos = nextField(blk, 1+(pkt_MatchPos(pkt)));
		{
		  ent = switchEnt(ent, accread, ((blk_End(blk))==(pos)
		      ?(endOfChain_P(blk)
			?str2long(blk,  -6+(pos))
			:blk_NxtId(blk))
		      :str2long(blk, 1+(pos))), accnone);
		  lastLevel = ((blk_End(blk))==(pos)
		      && !(endOfChain_P(blk))
		    ?(blk_Level(blk))+1
		    :blk_Level(blk));
		  continue LfindEnt;
		}
	      }
	    }
	  else return null;
	}
      }
    else return null;
  else return null;
}
}


public static int blkPrevKey(byte []blk,int pos)
{
  {
    int bPos = blkDataStart;
    int pPos = 0;
    while (!((bPos)>=(pos))) {
      {
	int T_bPos = nextCnvpair(blk, bPos);
	pPos = bPos;
	bPos = T_bPos;
      }
    }
    if ((bPos)>(pos))
      {
	System.err.print(">>>>ERROR<<<< "+("blkPrevKey")+": blk past end "+(blk_Id(blk))+" "+(pPos)+"\n");
	return 0;
      }
    else return pPos;
  }
}

/*  DATA BASE OPERATIONS */


public static int getThisVal(byte []blk,int bPos,byte []ansStr)
{
  bPos = nextField(blk, (bPos)+1);
  {
    int alen = fieldLen(blk, bPos);
    subbytesMove(blk, (bPos)+1, (bPos)+1+(alen), ansStr, 0);
    return alen;
  }
}


public static int getThisKey(byte []blk,int bPos,byte []keyStr,byte []ansStr,Ent ent,int kLen,int []pkt)
{
  {
    int bEnd = blk_End(blk);
    int sPos = nextField(blk, (bPos)+1);
    if ((sPos)<(bEnd))
      {
	int fPos = fieldLen(blk, bPos);
	int fSiz = fieldLen(blk, (bPos)+1);
	int alen = (fPos)+(fSiz);
	if ((keyStr)!=(ansStr))
	  subbytesMove(keyStr, 0, fPos, ansStr, 0);
	subbytesMove(blk, (bPos)+2, (bPos)+2+(fSiz), ansStr, fPos);
	releaseEnt(ent, accread);
	return alen;
      }
    else if ((sPos)!=(bEnd))
      {
	System.err.print(">>>>ERROR<<<< "+("chainNext")+": blk past end "+(blk_Id(blk))+" "+(sPos)+"\n");
	releaseEnt(ent, accread);
	return strangerr;
      }
    else if (endOfChain_P(blk))
      {
	releaseEnt(ent, accread);
	return notpres;
      }
    else {
      ent = switchEnt(ent, accread, blk_NxtId(blk), accread);
      if (a2b(ent))
	ent = chainFind(ent, accread, keyStr, kLen, pkt);
      if (a2b(ent))
	return chainNext(ent, keyStr, kLen, ansStr, pkt);
      else return unkerr;
    }
  }
}


public static int chainNext(Ent ent,byte []keyStr,int kLen,byte []ansStr,int []pkt)
{
  pkt_SetBlkToCache(pkt, ent_Id(ent));
  switch (pkt_MatchType(pkt)) {
  case pastp:
  case qpastp:
    return getThisKey(ent_Blk(ent), pkt_MatchPos(pkt), keyStr, ansStr, ent, kLen, pkt);
  case match:
    return getThisKey(ent_Blk(ent), nextCnvpair(ent_Blk(ent), pkt_MatchPos(pkt)), keyStr, ansStr, ent, kLen, pkt);
  default:
    releaseEnt(ent, accread);
    return notpres;
  }
}

/*  To shrink a block give growth less than 0 and location equals */
/*  position after deleted. */
/*  blk-change-size returns #f if not enough room */

public static boolean blk_ChangeSize(byte []blk,int loc,int growth,int bsiz)
{
  int bEnd = blk_End(blk);
  if (0==(growth))
    return (true);
  else if (((bEnd)+(growth))>(bsiz))
    return false;
  else if (0 > (growth))
    {
      subbytesMoveLeft(blk, loc, bEnd, blk, (loc)+(growth));
      blk_SetEnd(blk, (bEnd)+(growth));
      return (true);
    }
  else {
    subbytesMoveRight(blk, loc, bEnd, blk, (loc)+(growth));
    blk_SetEnd(blk, (bEnd)+(growth));
    return (true);
  }
}


public static boolean blk_RemoveKeyAndVal(byte []blk,int bPos,int bsiz)
{
  int nbPos = nextCnvpair(blk, bPos);
  if ((fieldLen(blk, nbPos))>(fieldLen(blk, bPos)))
    {
      int delkPos = (fieldLen(blk, nbPos))-(fieldLen(blk, bPos));
      setFieldLen(blk, 1+(bPos), (fieldLen(blk, 1+(nbPos)))+(delkPos));
      return blk_ChangeSize(blk, 2+(nbPos), ((bPos)-(nbPos))+(delkPos), bsiz);
    }
  else return blk_ChangeSize(blk, nbPos, (bPos)-(nbPos), bsiz);
}

/*  return #t if operation was succsessful; #f if not */
/*  */
/*  Note the splitting of OBLK into OBLK+NBLK by inserting the split */
/*  key of each block into parent. */
/*  */
/*  Note this routine does not check if the key(s) have already been */
/*  (perhaps by another process) inserted into parent. */
/*  */
/*  unfortunately, the right way to do this requires that the update */
/*  look just like a PUT of the NKEY-STR with value N-ID, albeit one */
/*  that then swaps the values of the new entry and the one */
/*  following... */
/*  */
/*  The SCREW-CASE occurs when the key is inserted at the endof the */
/*  block, so that we have to get access to the next (NON-EMPTY!) */
/*  block to make the swap... */


public static boolean deferInsertUpdates_P = false;


public static boolean parentInsertUpdate(Seg seg,int topId,int level,byte []nkeyStr,int nkLen,int nId)
{
  int []pkt = new int[pktSize];
  {
    Ent ent = findEnt(getEnt(seg, topId, accnone), 1+(level),  -1, nkeyStr, nkLen);
    Ent xent = null;
    boolean screwCase_P = false;
    byte []blkidstr = new byte[4];
    byte []blk = null;
    if (a2b(ent))
      {
	long2str(blkidstr, 0, nId);
	if (ents_EntUpdateAccess_P(ent, accread, accwrite))
	  {
	    ent = chainFind(ent, accwrite, nkeyStr, nkLen, pkt);
	    blk = ent_Blk(ent);
	  }
	else {
	  releaseEnt(ent, accread);
	  ent = null;
	}
	if (a2b(ent)
	    && atSplitKeyPos_P(blk, pkt_MatchPos(pkt)))
	  {
	    screwCase_P = (true);
	    xent = nextNonemptyEnt(ent_Seg(ent), blk_NxtId(blk));
	    if (!(a2b(xent)))
	      System.err.print(">>>>ERROR<<<< No next key found for index insert "+(seg_Id(ent_Seg(ent)))+":"+(blk_Id(blk))+"\n");
	  }
	if (!(deferInsertUpdates_P)
	    && a2b(ent)
	    && (!(screwCase_P)
		   || a2b(xent))
	    && chainPut(ent, nkeyStr, nkLen, blkidstr, 4, pkt, xent, wcbSar))
	  return (true);
	else {
	  System.err.print("WARNING: "+("parentInsertUpdate")+": couldn't update parent n-id="+(nId)+" nk-len="+(nkLen)+"\n");
	  deferredInserts = 1+(deferredInserts);
	  if (a2b(ent))
	    releaseEnt(ent, accwrite);
	  return false;
	}
      }
    else return false;
  }
}

/*  only valid if called with POS=position of some KEY */

public static boolean atSplitKeyPos_P(byte []blk,int pos)
{
  return (blk_End(blk))==(nextField(blk, 1+(pos)));
}


public static Ent nextNonemptyEnt(Seg seg,int blknum)
{
  if ((blknum)<=0)
    return null;
  else {
    Ent xent = getEnt(seg, blknum, accread);
Lloop: while (true) {
    if (a2b(xent))
      ents_EntUpdateAccess_P(xent, accread, accwrite);
    if (!(a2b(xent)))
      return null;
    else if (!(blkEmpty_P(ent_Blk(xent))))
      return xent;
    else if (0==(blk_NxtId(ent_Blk(xent))))
      {
	releaseEnt(xent, accwrite);
	return null;
      }
    else {
      xent = switchEnt(xent, accwrite, blk_NxtId(ent_Blk(xent)), accwrite);
      continue Lloop;
    }
    }
  }
}

/*  Note: CFP must NOT return the split key position IFF at a LEAF */
/*  RECON-THIS-KEY returns the data in KEY and its length as its return value. */
/*  END-OF-CHAIN (-1) is returned if the key reconstructed is the end-of-file mark */
/*  k-len is now used correctly to signal a potential overflow */


public static int reconThisKey(byte []blk,int pos,byte []keyStr,int kPos,int kLen)
{
  {
    int bPos = blkDataStart;
    int kSize = 0;
    while (!((bPos)>(pos))) {
      if ((kSize)>(fieldLen(blk, bPos))
	  && ((blk[(bPos)+2] & 0xFF))<=((keyStr[(kPos)+(fieldLen(blk, bPos))] & 0xFF)))
	System.err.print(">>>>ERROR<<<< bad key sequence "+(blk_Id(blk))+" @ "+(bPos)+"\n");
      kSize = (fieldLen(blk, bPos))+(fieldLen(blk, 1+(bPos)));
      if ((kSize)>=(kLen))
	System.err.print(">>>>ERROR<<<< not-enough-room "+(kLen)+"\n");
      subbytesMove(blk, (bPos)+2, (bPos)+2+(fieldLen(blk, 1+(bPos))), keyStr, (kPos)+(fieldLen(blk, bPos)));
      bPos = nextField(blk, 1+(bPos));
      if ((bPos)<(blk_End(blk)))
	bPos = nextField(blk, bPos);
      {
      }
    }
    return kSize;
  }
}


public static boolean blk_InsertAndAdjust(byte []blk,int bPos,int kPos,byte []keyStr,int kLen,byte []valStr,int vLen,int bsiz)
{
  {
    int oldkPos = fieldLen(blk, bPos);
    int oldilen = fieldLen(blk, 1+(bPos));
    int ilen = (kLen)-(oldkPos);
    if (blk_ChangeSize(blk, bPos, 2+((kLen)-(kPos))+1+(vLen), bsiz))
      {
	bPos = 1+(bPos);
	bPos = setField(blk, bPos, keyStr, oldkPos, ilen);
	bPos = setField(blk, bPos, valStr, 0, vLen);
	setFieldLen(blk, bPos, kPos);
	setFieldLen(blk, (bPos)+1, (oldilen)-((kPos)-(oldkPos)));
	return (true);
      }
    else return false;
  }
}


public static boolean blk_SimpleInsert(byte []blk,int bPos,int kPos,byte []keyStr,int kLen,byte []valStr,int vLen,int bsiz)
{
  int ilen = (kLen)-(kPos);
  if (blk_ChangeSize(blk, bPos, 3+(vLen)+(ilen), bsiz))
    {
      setFieldLen(blk, bPos, kPos);
      bPos = 1+(bPos);
      bPos = setField(blk, bPos, keyStr, kPos, ilen);
      setField(blk, bPos, valStr, 0, vLen);
      return (true);
    }
  else return false;
}


public static boolean blk_ChangeExistingValue(byte []blk,int bPos,byte []keyStr,int kLen,byte []valStr,int vLen,int bsiz)
{
  int ovLen = 0;
  int vPos = nextField(blk, 1+(bPos));
  ovLen = fieldLen(blk, vPos);
  if (blk_ChangeSize(blk, (vPos)+(ovLen)+1, (vLen)-(ovLen), bsiz))
    {
      setField(blk, vPos, valStr, 0, vLen);
      return (true);
    }
  else return false;
}

/*  leaf-splits are called with ACCWRITE on blk and return without it. */

public static int valLeafSplit(byte []blk,byte []nblk,int bPos,byte []keyStr,int kPos,int kLen,byte []valStr,int vLen)
{
  {
    int vPos = nextField(blk, 1+(bPos));
    int sPos = nextField(blk, vPos);
    int bEnd = blk_End(blk);
    setFieldLen(nblk, blkDataStart, 0);
    if (((bEnd)-(sPos))>((vPos)-(blkDataStart)))
      {
	int mLen = fieldLen(blk, sPos);
	int fChr = (blk[(sPos)+2] & 0xFF);
	setFieldLen(nblk, (blkDataStart)+1, (mLen)+(fieldLen(blk, 1+(sPos))));
	subbytesMove(keyStr, 0, mLen, nblk, (blkDataStart)+2);
	subbytesMove(blk, (sPos)+2, bEnd, nblk, (blkDataStart)+(mLen)+2);
	blk_SetEnd(nblk, ((bEnd)-(sPos))+(mLen)+(blkDataStart));
	bPos = setField(blk, vPos, valStr, 0, vLen);
	blk[(bPos)+2] = (byte) (255&(fChr));
	setFieldLen(blk, bPos, mLen);
      }
    else {
      int nbPos = (blkDataStart)+1;
      nbPos = setField(nblk, nbPos, keyStr, 0, kLen);
      nbPos = setField(nblk, nbPos, valStr, 0, vLen);
      subbytesMove(blk, sPos, bEnd, nblk, nbPos);
      blk_SetEnd(nblk, (nbPos)+((bEnd)-(sPos)));
    }
    setFieldLen(blk, (bPos)+1, 1);
    blk_SetEnd(blk, (bPos)+3);
    return bPos;
  }
}


public static int qpastpLeafSplit(byte []blk,byte []nblk,int bPos,byte []keyStr,int kPos,int kLen,byte []valStr,int vLen)
{
  {
    int bEnd = blk_End(blk);
    setFieldLen(nblk, blkDataStart, 0);
    if (((bEnd)-(bPos))>((bPos)-(blkDataStart)))
      {
	int mLen = fieldLen(blk, bPos);
	int fChr = (blk[(bPos)+2] & 0xFF);
	setFieldLen(nblk, (blkDataStart)+1, (mLen)+(fieldLen(blk, 1+(bPos))));
	subbytesMove(keyStr, 0, mLen, nblk, (blkDataStart)+2);
	subbytesMove(blk, (bPos)+2, bEnd, nblk, (blkDataStart)+(mLen)+2);
	blk_SetEnd(nblk, ((bEnd)-(bPos))+(mLen)+(blkDataStart));
	setFieldLen(blk, bPos, kPos);
	bPos = setField(blk, (bPos)+1, keyStr, kPos, (kLen)-(kPos));
	bPos = setField(blk, bPos, valStr, 0, vLen);
	blk[(bPos)+2] = (byte) (255&(fChr));
	setFieldLen(blk, bPos, mLen);
      }
    else {
      int nbPos = (blkDataStart)+1;
      nbPos = setField(nblk, nbPos, keyStr, 0, kLen);
      nbPos = setField(nblk, nbPos, valStr, 0, vLen);
      subbytesMove(blk, bPos, bEnd, nblk, nbPos);
      blk_SetEnd(nblk, (nbPos)+((bEnd)-(bPos)));
      setFieldLen(blk, bPos, kPos);
      blk[(bPos)+2] = (byte) (255&((keyStr[kPos] & 0xFF)));
    }
    setFieldLen(blk, (bPos)+1, 1);
    blk_SetEnd(blk, (bPos)+3);
    return bPos;
  }
}


public static int pastpLeafSplit(byte []blk,byte []nblk,int bPos,byte []keyStr,int kPos,int kLen,byte []valStr,int vLen)
{
  {
    int mLen = fieldLen(blk, bPos);
    int bEnd = blk_End(blk);
    setFieldLen(nblk, blkDataStart, 0);
    if (((bEnd)-(bPos))>((bPos)-(blkDataStart)))
      {
	int fChr = (blk[(bPos)+2+((kPos)-(fieldLen(blk, bPos)))] & 0xFF);
	setFieldLen(nblk, (blkDataStart)+1, (mLen)+(fieldLen(blk, 1+(bPos))));
	subbytesMove(keyStr, 0, mLen, nblk, (blkDataStart)+2);
	subbytesMove(blk, (bPos)+2, bEnd, nblk, (blkDataStart)+(mLen)+2);
	blk_SetEnd(nblk, ((bEnd)-(bPos))+(mLen)+(blkDataStart));
	setFieldLen(blk, bPos, mLen);
	bPos = setField(blk, (bPos)+1, keyStr, mLen, (kLen)-(mLen));
	bPos = setField(blk, bPos, valStr, 0, vLen);
	blk[(bPos)+2] = (byte) (255&(fChr));
	setFieldLen(blk, bPos, kPos);
      }
    else {
      int nbPos = (blkDataStart)+1;
      int cPos = (bPos)+2+((kPos)-(mLen));
      nbPos = setField(nblk, nbPos, keyStr, 0, kLen);
      nbPos = setField(nblk, nbPos, valStr, 0, vLen);
      setFieldLen(nblk, nbPos, kPos);
      setFieldLen(nblk, (nbPos)+1, ((fieldLen(blk, 1+(bPos)))+(mLen))-(kPos));
      subbytesMove(blk, cPos, bEnd, nblk, (nbPos)+2);
      blk_SetEnd(nblk, (nbPos)+2+((bEnd)-(cPos)));
    }
    setFieldLen(blk, (bPos)+1, 1);
    blk_SetEnd(blk, (bPos)+3);
    return bPos;
  }
}


public static int dummyLeafSplit(byte []blk,byte []nblk,int bPos,byte []keyStr,int kPos,int kLen,byte []valStr,int vLen)
{
  System.err.print(">>>>ERROR<<<< "+("dummyLeafSplit")+": bad-MATCH-TYPE blk "+(blk_Id(blk))+"\n");
  return 0;
}


public static boolean chainPut(Ent ent,byte []keyStr,int kLen,byte []valStr,int vLen,int []pkt,Ent xent,int wcb)
{
  {
    byte []blk = ent_Blk(ent);
    int blklev = blk_Level(blk);
    boolean index_P = (blklev)>(leaf);
    int rootId = blk_TopId(blk);
    Ent nent = null;
    Ent nrent = null;
    Seg seg = ent_Seg(ent);
    int bsiz = seg_Bsiz(seg);
    boolean result_P = false;
    boolean split_P = false;
    Ent nkeyEnt = ent;
    int nkeyPos = pkt_MatchPos(pkt);
    Ent okeyEnt = ent;
    int okeyPos = blkDataStart;
    int nId = 0;
    int sPos = 0;
    byte []splitStr = new byte[256];
    int sLen = 0;
    pkt_SetBlkToCache(pkt, ent_Id(ent));
    if ((pkt_MatchType(pkt))==(pastp)
        && blk_InsertAndAdjust(blk, pkt_MatchPos(pkt), pkt_KeyPos(pkt), keyStr, kLen, valStr, vLen, bsiz))
      {
	result_P = (true);
      }
    else if ((pkt_MatchType(pkt))==(qpastp)
        && blk_SimpleInsert(blk, pkt_MatchPos(pkt), pkt_KeyPos(pkt), keyStr, kLen, valStr, vLen, bsiz))
      {
	result_P = (true);
      }
    else if ((pkt_MatchType(pkt))==(match)
        && blk_ChangeExistingValue(blk, pkt_MatchPos(pkt), keyStr, kLen, valStr, vLen, bsiz))
      {
	result_P = (true);
      }
    else 
    nent = createNewBlkEnt(seg);
    if (!(a2b(nent)))
      ;
    else {
      split_P = (true);
      {
	byte []nblk = ent_Blk(nent);
	nId = ent_Id(nent);
	initNextBlk(blk, nblk);
	blockSplits = (blockSplits)+1;
	switch (pkt_MatchType(pkt)) {
	case pastp:
	  sPos = pastpLeafSplit(blk, nblk, pkt_MatchPos(pkt), keyStr, pkt_KeyPos(pkt), kLen, valStr, vLen);
	  break;
	case qpastp:
	  sPos = qpastpLeafSplit(blk, nblk, pkt_MatchPos(pkt), keyStr, pkt_KeyPos(pkt), kLen, valStr, vLen);
	  break;
	case match:
	  sPos = valLeafSplit(blk, nblk, pkt_MatchPos(pkt), keyStr, pkt_KeyPos(pkt), kLen, valStr, vLen);
	  break;
	default:
	  sPos = dummyLeafSplit(blk, nblk, pkt_MatchPos(pkt), keyStr, pkt_KeyPos(pkt), kLen, valStr, vLen);
	  break;
	}
	sLen = 1+(fieldLen(blk, sPos));
	subbytesMove(nblk, (blkDataStart)+2, 1+(fieldLen(blk, sPos))+((blkDataStart)+2), splitStr, 0);
	if (index_P)
	  {
	    okeyEnt = nent;
	    if ((pkt_MatchPos(pkt))!=(sPos))
	      {
		splitIndexInserts = 1+(splitIndexInserts);
	      }
	    else {
	      okeyPos = nextCnvpair(nblk, blkDataStart);
	      nkeyEnt = nent;
	      nkeyPos = blkDataStart;
	    }
	  }
	if ((pkt_MatchPos(pkt))==(sPos))
	  pkt_SetBlkToCache(pkt, ent_Id(nent));
	if (root_P(blk))
	  {
	    nrent = createNewBlkEnt(seg);
	    if (a2b(nrent))
	      {
		reroot(blk, ent_Blk(nrent), ent_Id(nrent), seg_Bsiz(seg));
		if ((nkeyEnt)==(ent))
		  {
		    nkeyEnt = nrent;
		    pkt_SetBlkToCache(pkt, ent_Id(nrent));
		  }
	      }
	  }
	result_P = (true);
      }
    }
    if (result_P
        && index_P)
      {
	if (a2b(xent))
	  {
	    indexScrewCase = 1+(indexScrewCase);
	    okeyEnt = xent;
	    okeyPos = blkDataStart;
	  }
	else if (!(split_P))
	  okeyPos = nextCnvpair(blk, pkt_MatchPos(pkt));
	{
	  byte []tmpstr = new byte[4];
	  int oldvPos = (nextField(ent_Blk(okeyEnt), (okeyPos)+1))+1;
	  int newvPos = (nextField(ent_Blk(nkeyEnt), (nkeyPos)+1))+1;
	  subbytesMoveLeft(ent_Blk(okeyEnt), oldvPos, (oldvPos)+4, tmpstr, 0);
	  subbytesMoveLeft(ent_Blk(nkeyEnt), newvPos, (newvPos)+4, ent_Blk(okeyEnt), oldvPos);
	  subbytesMoveLeft(tmpstr, 0, 4, ent_Blk(nkeyEnt), newvPos);
	}
      }
    if (a2b(nrent))
      {
	ents_EntWrite(nrent);
	releaseEnt(nrent, accwrite);
      }
    if (a2b(nent))
      {
	ents_EntWrite(nent);
	ents_EntUpdateAccess_P(nent, accwrite, accnone);
      }
    if (result_P)
      {
	ent_SetDty(ent, true);
	if ((split_P
	    || a2b(xent)
	    || 0 != ((wcbSap)&(wcb))))
	  ents_EntWrite(ent);
	releaseEnt(ent, accwrite);
      }
    if (a2b(xent))
      {
	ent_SetDty(xent, true);
	ents_EntWrite(xent);
	releaseEnt(xent, accwrite);
      }
    if (split_P)
      parentInsertUpdate(seg, rootId, blklev, splitStr, sLen, nId);
    if (a2b(nent))
      releaseEnt(nent, accnone);
    return result_P;
  }
}

}
