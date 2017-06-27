/* This file was generated by scm2java from source file "handle.scm" */
/*  "handle.scm" WB-tree File Based Associative String Data Base System. */
/*  Copyright (C) 1991, 1992, 1993, 2000, 2007, 2010 Free Software Foundation, Inc. */
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

/*  routines in this file return success codes */



package wb;

import static wb.Ents.*;
import static wb.Prev.*;
import static wb.Stats.*;
import static wb.Blink.*;
import static wb.Del.*;
import static wb.Scan.*;
import static wb.Wbdefs.*;
import static wb.Wbsys.*;
import static wb.SchlepRT.*;
import static wb.Pkt.*;
import static wb.Ent.*;
import static wb.Han.*;
public class Handle {


public static boolean cleverCacheEnable_P = (true);


public static boolean hanError_P(Han han,String caller)
{
  if (!(a2b(han)))
    {
      System.err.print(">>>>ERROR<<<< "+(caller)+": called with NULL handle.\n");
      return (true);
    }
  else if (!(a2b(han_Seg(han))))
    {
      System.err.print(">>>>ERROR<<<< "+(caller)+": called with handle having NULL segment.\n");
      return (true);
    }
  else return false;
}


public static boolean keyError_P(byte []keyStr,int kLen,int xcpt,String caller)
{
  if ((xcpt)==(kLen))
    return false;
  else if (!(0<=(kLen)
	  && (kLen)<=255))
    {
      System.err.print(">>>>ERROR<<<< "+(caller)+": key-length, "+(kLen)+", was out of range 0 <= 255.\n");
      return (true);
    }
  else if (0 < (kLen)
      && 255==((keyStr[0] & 0xFF)))
    {
      System.err.print(">>>>ERROR<<<< "+(caller)+": first byte of key must not be 255.\n");
      return (true);
    }
  else return false;
}


public static int btGet(Han han,byte []keyStr,int kLen,byte []ansStr)
{
  int []pkt = new int[pktSize];
  Ent ent = null;
  if (keyError_P(keyStr, kLen, 0, "btGet"))
    return keyerr;
  else if (hanError_P(han, "btGet"))
    return argerr;
  else {
    ent = chainFindEnt(han, accread, keyStr, kLen, pkt);
    if (!(a2b(ent)))
      {
	getFct = 1+(getFct);
	return unkerr;
      }
    else if ((pkt_MatchType(pkt))!=(match))
      {
	getCt = 1+(getCt);
	releaseEnt(ent, accread);
	return notpres;
      }
    else {
      int alen = getThisVal(ent_Blk(ent), pkt_MatchPos(pkt), ansStr);
      getCt = 1+(getCt);
      releaseEnt(ent, accread);
      return alen;
    }
  }
}


public static int btNext(Han han,byte []keyStr,int kLen,byte []ansStr)
{
  int []pkt = new int[pktSize];
  Ent ent = null;
  if (keyError_P(keyStr, kLen,  -2, "btNext"))
    return keyerr;
  else if (hanError_P(han, "btNext"))
    return argerr;
  else {
    ent = chainFindEnt(han, accread, keyStr, kLen, pkt);
    if (!(a2b(ent)))
      {
	nextFct = 1+(nextFct);
	return unkerr;
      }
    else {
      nextCt = 1+(nextCt);
      {
	int res = chainNext(ent, keyStr, kLen, ansStr, pkt);
	if (cleverCacheEnable_P)
	  han_SetLast(han, pkt_BlkToCache(pkt));
	return res;
      }
    }
  }
}


public static int btPrev(Han han,byte []keyStr,int kLen,byte []ansStr)
{
  int []pkt = new int[pktSize];
  Ent ent = null;
  if (keyError_P(keyStr, kLen,  -1, "btPrev"))
    return keyerr;
  else if (hanError_P(han, "btPrev"))
    return argerr;
  else {
    ent = chainFindPrevEnt(han, accread, keyStr, kLen, pkt);
    if (a2b(ent))
      ent = prev_PrevKEnt(ent, keyStr, kLen, leaf, pkt);
    if (!(a2b(ent)))
      {
	prevFct = 1+(prevFct);
	return unkerr;
      }
    else {
      prevCt = 1+(prevCt);
      if (0==(pkt_MatchPos(pkt)))
	{
	  releaseEnt(ent, accread);
	  return notpres;
	}
      else {
	int kLen2 = reconThisKey(ent_Blk(ent), pkt_MatchPos(pkt), ansStr, 0, 256);
	han_SetLast(han, ent_Id(ent));
	releaseEnt(ent, accread);
	return kLen2;
      }
    }
  }
}


public static int btRem(Han han,byte []keyStr,int kLen,byte []ansStr)
{
  int []pkt = new int[pktSize];
  Ent ent = null;
  if (keyError_P(keyStr, kLen, 0, "btRem"))
    return keyerr;
  else if (hanError_P(han, "btRem"))
    return argerr;
  else {
    ent = chainFindEnt(han, accwrite, keyStr, kLen, pkt);
    if (!(a2b(ent)))
      {
	remFct = 1+(remFct);
	return unkerr;
      }
    else {
      remCt = 1+(remCt);
      {
	int ans = del_ChainRem(ent, keyStr, kLen, ansStr, pkt, han_Wcb(han));
	releaseEnt(ent, accwrite);
	return ans;
      }
    }
  }
}

/*  To make possible bounded-time operation, @0 will purge at most */
/*  BLK-LIMIT blocks at a time; passing BLK-LIMIT of -1 imposes no */
/*  limit. */


public static int btRemRange(Han han,byte []keyStr,int kLen,byte []key2Str,int k2Len)
{
  int []respkt = new int[pktSize];
  pkt_SetSkeyCount(respkt, 0);
  return btScan(han, remScan, keyStr, kLen, key2Str, k2Len, null, null, respkt,  -1);
}


public static int btPut(Han han,byte []keyStr,int kLen,byte []valStr,int vLen)
{
  Ent ent = null;
  int []pkt = new int[pktSize];
  if (keyError_P(keyStr, kLen, 0, "btPut"))
    return keyerr;
  else if (hanError_P(han, "btPut"))
    return argerr;
  else if (!(0<=(vLen)
	  && (vLen)<=255))
    return argerr;
  else {
    ent = chainFindEnt(han, accwrite, keyStr, kLen, pkt);
    if (!(a2b(ent)))
      return unkerr;
    else {
      boolean res_P = chainPut(ent, keyStr, kLen, valStr, vLen, pkt, null, han_Wcb(han));
      if (res_P)
	{
	  if (cleverCacheEnable_P)
	    han_SetLast(han, pkt_BlkToCache(pkt));
	  putCt = 1+(putCt);
	  return success;
	}
      else {
	putFct = 1+(putFct);
	return unkerr;
      }
    }
  }
}


public static int btWrite(Han han,byte []keyStr,int kLen,byte []valStr,int vLen)
{
  Ent ent = null;
  int []pkt = new int[pktSize];
  if (keyError_P(keyStr, kLen, 0, "btWrite"))
    return keyerr;
  else if (hanError_P(han, "btWrite"))
    return argerr;
  else if (!(0<=(vLen)
	  && (vLen)<=255))
    return argerr;
  else {
    ent = chainFindEnt(han, accwrite, keyStr, kLen, pkt);
    if (!(a2b(ent)))
      return unkerr;
    else if ((pkt_MatchType(pkt))==(match))
      {
	releaseEnt(ent, accwrite);
	return notpres;
      }
    else {
      boolean res_P = chainPut(ent, keyStr, kLen, valStr, vLen, pkt, null, han_Wcb(han));
      if (res_P)
	{
	  if (cleverCacheEnable_P)
	    han_SetLast(han, pkt_BlkToCache(pkt));
	  putCt = 1+(putCt);
	  return success;
	}
      else {
	putFct = 1+(putFct);
	return unkerr;
      }
    }
  }
}

}