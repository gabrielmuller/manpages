/* "Pkt.cs" WB-tree File Based Associative String Data Base System. */

/*  Routines for finding the appropriate BLK for an operation. */
/*  PACKETs used to return multiple values from chain-find. */
/*  and various other operations */

public static readonly int pktSize = 6;

public static int[] makePkt()
{
    return new int[pktSize];
}

public static int pkt_MatchType(int[] p)
{
    return p[0];
}

public static int pkt_MatchPos(int[] p)
{
    return p[1];
}

public static int pkt_KeyPos(int[] p)
{
    return p[2];
}

public static int pkt_PrevMatchPos(int[] p)
{
    return p[3];
}

public static int pkt_BlkToCache(int[] p)
{
    return p[4];
}

public static int pkt_SuccessCode(int[] p)
{
    return p[5];
}

public static void pkt_SetMatchType(int[] p,int v)
{
    p[0] = v;
}

public static void pkt_SetMatchPos(int[] p,int v)
{
    p[1] = v;
}

public static void pkt_SetKeyPos(int[] p,int v)
{
    p[2] = v;
}

public static void pkt_SetPrevMatchPos(int[] p,int v)
{
    p[3] = v;
}

public static void pkt_SetBlkToCache(int[] p,int v)
{
    p[4] = v;
}

public static void pkt_SetSuccessCode(int[] p,int v)
{
    p[5] = v;
}

//Aliases
public static int pkt_SkeyCount(int[] p)
{
    return pkt_MatchPos(p);
}

public static void pkt_SetSkeyCount(int[] p, int v)
{
    pkt_SetMatchPos(p, v);
}

public static int pkt_SkeyLen (int[] p)
{
    return pkt_KeyPos(p);
}

public static void pkt_SetSkeyLen (int[]p, int v)
{
    pkt_SetKeyPos(p, v);
}

public static void pkt_Pack(int[] p,int type,int bPos,int kPos,int pPos)
{
    pkt_SetMatchType(p, type);
    pkt_SetMatchPos(p, bPos);
    pkt_SetKeyPos(p, kPos);
    pkt_SetPrevMatchPos(p, pPos);
}
