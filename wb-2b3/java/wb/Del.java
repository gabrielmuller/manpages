/* This file was generated by scm2java from source file "del.scm" */
/*  "del.scm" WB-tree File Based Associative String Data Base System. */
/*  Copyright (C) 1991, 1992, 1993, 2000 Free Software Foundation, Inc. */
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



package wb;

import static wb.Ents.*;
import static wb.Prev.*;
import static wb.Stats.*;
import static wb.Blink.*;
import static wb.Wbdefs.*;
import static wb.Wbsys.*;
import static wb.Blk.*;
import static wb.SchlepRT.*;
import static wb.Pkt.*;
import static wb.Seg.*;
import static wb.Ent.*;
public class Del {


public static boolean del_DeferBlockDeletes_P = false;

/*  fixes: */
/*  1. 1/22 blk-delete should not be called if END-OF-CHAIN */
/*  2.      IND-REM-V&K needed to return B-POS */
/*  3.      CHAIN-KEY-REM also neede to check for being already at root level */
/*  4. 1/23 fixed BLK-DELETE? to set accmode to ACCNONE while calling PREV-BLK-ENT! */
/*  5.      fixed CHAIN-KEY-REM to give error message if key not found in index */

/*  BLK-DELETE assumes caller has ACCWRITE to blk and will */
/*  release if after blk-delete returns */

/*  sorry, waiting on parent-update is losing since */
/*  deletes that  lock the entire path to the root will almost certainly */
/*  NEVER succeed! */


public static boolean del_DeleteBck(Ent ent)
{
  byte []blk = ent_Blk(ent);
  boolean win_P = !(del_DeferBlockDeletes_P);
  if (win_P)
    {
      ents_EntUpdateAccess_P(ent, accwrite, accnone);
      {
	Ent prent = prevBlkEnt(ent, blk_Level(blk));
	win_P = ents_EntUpdateAccess_P(ent, accnone, accwrite);
	if (win_P
	    && a2b(prent))
	  win_P = ents_EntUpdateAccess_P(prent, accread, accwrite);
	win_P = win_P
        && 1==(ent_Ref(ent));
	if (win_P)
	  {
	    if (!(atRootLevel_P(ent_Seg(ent), blk)))
	      {
		int skeyPos = splitKeyPos(blk);
		if (0 < (skeyPos))
		  {
		    int topNum = blk_TopId(blk);
		    Seg seg = ent_Seg(ent);
		    int level = blk_Level(blk);
		    byte []keyStr = new byte[256];
		    int kLen = reconThisKey(blk, skeyPos, keyStr, 0, 256);
		    win_P = parentDeleteUpdate_P(seg, topNum, level, ent_Id(ent), keyStr, kLen);
		  }
	      }
	    win_P = win_P
	    && 1==(ent_Ref(ent));
	    if (win_P)
	      {
		if (a2b(prent))
		  {
		    blk_SetNxtId(ent_Blk(prent), blk_NxtId(blk));
		    ent_SetDty(prent, true);
		    ents_EntWrite(prent);
		  }
		win_P = blkFree(ent);
		if (!(win_P))
		  System.err.print(">>>>ERROR<<<< "+("blkDelete")+": could not free "+(seg_Id(ent_Seg(ent)))+":"+(ent_Id(ent))+"\n");
	      }
	  }
	if (a2b(prent))
	  releaseEnt(prent, ent_Acc(prent));
      }
    }
  if (win_P)
    {
      blockDeletes = (blockDeletes)+1;
    }
  else {
    deferredDeletes = 1+(deferredDeletes);
    System.err.print("Can't delete block "+(ent_Id(ent))+"\n");
  }
  return win_P;
}

/*  return #t if operation was succsessful; #f if not */
/*  Note the deletion of blk OLD-ID by removing its KEY+ID from parent. */
/*  Note this routine does not check if the key has already been */
/*  (perhaps by another process) deleted from the parent. */


public static boolean parentDeleteUpdate_P(Seg seg,int topId,int level,int oldId,byte []keyStr,int kLen)
{
  int []pkt = new int[pktSize];
  int ans =  -1;
  byte []ansStr = new byte[4];
  {
    Ent ent = findEnt(getEnt(seg, topId, accnone), 1+(level),  -1, keyStr, kLen);
    if (!(a2b(ent)))
      ;
    else if (ents_EntUpdateAccess_P(ent, accread, accwrite))
      {
	ent = chainFind(ent, accwrite, keyStr, kLen, pkt);
      }
    else {
      releaseEnt(ent, accread);
      ent = null;
    }
    if (a2b(ent))
      {
	ans = del_ChainRem(ent, keyStr, kLen, ansStr, pkt, wcbSar);
	if ((ans)>=0)
	  if ((oldId)!=(str2long(ansStr, 0)))
	    System.err.print(">>>>ERROR<<<< "+("parentDeleteUpdate_P")+": bad value "+(str2long(ansStr, 0))+" in deleted down pointer "+(oldId)+" told\n");
	releaseEnt(ent, accwrite);
      }
    if ((a2b(ent)
        || (ans)>=0))
      return (true);
    else {
      System.err.print("WARNING: "+("parentDeleteUpdate_P")+" blk="+(seg_Id(seg))+":"+(oldId)+", level="+(level)
   +", key="+(kLen)+"\n");
      return false;
    }
  }
}

/*  called with ACCREAD on ENT, releases ent before returning */
/*  DEL:CHAIN-REM can call BLK-DELETE */
/*    BLK-DELETE calls BLK-FREE */
/*      BLK-FREE calls AMNESIA-ENT! which sets the segment number to -1 */
/*  DEL:CHAIN-REM calls RELEASE-ENT! */
/*  Chad Gadya! */


public static int del_ChainRem(Ent ent,byte []keyStr,int kLen,byte []ansStr,int []pkt,int wcb)
{
  if ((pkt_MatchType(pkt))==(match))
    {
      int alen = success;
      if (a2b(ansStr))
	alen = getThisVal(ent_Blk(ent), pkt_MatchPos(pkt), ansStr);
      blk_RemoveKeyAndVal(ent_Blk(ent), pkt_MatchPos(pkt), seg_Bsiz(ent_Seg(ent)));
      ent_SetDty(ent, true);
      if (blkEmpty_P(ent_Blk(ent))
	  && !(endOfChain_P(ent_Blk(ent))))
	{
	  del_DeleteBck(ent);
	}
      else if ((0 != ((wcbSar)&(wcb))
	  || (blk_Level(ent_Blk(ent)))>(leaf)))
	ents_EntWrite(ent);
      return alen;
    }
  else return notpres;
}

}
