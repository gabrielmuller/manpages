/* "wbcheck.c" WB-tree File Based Associative String Data Base System.
 * Copyright (C) 1991, 1992, 1993, 2000, 2009, 2010 Free Software Foundation, Inc.
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

typedef unsigned char * STR;
#define MAX_LEVELS   20
#define MAP_SIZE     64000
typedef unsigned long int BLOCK;

unsigned char free_map[65535];

char system_function = 'I';
unsigned long int mon_disk_read = 0;
unsigned long int mon_disk_write = 0;
unsigned long int mon_os_file_read = 0;
unsigned long int mon_interpreter = 0;
unsigned long int mon_blink = 0;
unsigned long int mon_symbol_table = 0;
unsigned long int mon_term_io = 0;

int error_count;
int fatal_error;
int blkgc = 1;
int follow_dirs = 1;
unsigned long int blocks_marked; /* number of blocks in use by system */
unsigned long int total_blocks_marked; /* number of blocks in use by system */

int set_block_used(blk_num)
BLOCK blk_num;
{
 if ( (free_map[blk_num/8] >> (blk_num % 8)) & 1) return(0);
 free_map[blk_num/8] |= (1 << (blk_num % 8));
 return(1);
}

int get_block_used(blk_num)
     BLOCK blk_num;
{
  if ( (free_map[blk_num/8] >> (blk_num % 8)) & 1) return(1);
  return(0);
}

struct disk_blk {
		 ENTRY *entry;
		 BLOCK blk_num;
		 BLOCK cont_ptr;
		 BLOCK tree_head;
		 BLOCK act_blk_num;
		 BLOCK act_tree_head;
		 long size;
		 int level;
		 int type;
		 int directory;
		 unsigned char *block;
		 unsigned char *current_pos;
		 unsigned char *block_end;
		 unsigned char current_name[256];
		 int current_name_len;
		 unsigned char last_block_name[256];
		 int last_block_name_len;
		};

typedef struct disk_blk DISK_BLK;

DISK_BLK disk_blocks[MAX_LEVELS];
DISK_BLK temp_block;

void dspl_char(ch)
     int ch;
{
  if ((ch >= ' ') && (ch <= '~')) putchar(ch);
  else printf("'%d'",ch);
}

void dspl_string(string,size)
     STR string;
     int size;
{
  while (size-- > 0) dspl_char(*string++);
}

void path(level)
     int level;
{
  int i = 0;
  while (i <= level)
    {
      printf("\n  LEVEL %d  BLOCK: %lu --> %lu",
	     disk_blocks[i].level,
	     disk_blocks[i].act_blk_num,
	     disk_blocks[i].cont_ptr);
      printf("\n   NAME: ");
      dspl_string(disk_blocks[i].current_name,
		  disk_blocks[i].current_name_len);
      i++;
    }
  if (++error_count >= 50) fatal_error = 1;
}

int string_compare(name1,len1,name2,len2)
     /* returns 0 if equal 1 if first larger, 2 if second larger */
     STR name1,name2;
     int len1,len2;
{
  int max_len = min(len1,len2);
  int len = 0;
  while ( (len < max_len) && (name1[len] == name2[len]))  len++;
  if (len < len2)
    {
      if (len == len1) return(2);
      else if (name1[len] < name2[len]) return(2);
      else return(1);
    }
  else if (len < len1) return(1);
  else return(0);
}

int cnv_string_to_long(s1b,s1e,i)
     STR s1b, s1e;
     unsigned long int *i;
{
  unsigned long int rslt = 0,rslt1 = 0;
  while ( (s1b != s1e) && (*s1b >= '0') && (*s1b <= '9') )
    {
      if ((rslt = (rslt*10) + (*s1b++ - 48) ) < rslt1) return(0);
      rslt1 = rslt;
    }
  *i = rslt;
  return(1);
}

int read_block(seg, blk_num,dblock,head_block)
     SEGD *seg;
     BLOCK blk_num;
     DISK_BLK *dblock;
     BLOCK head_block;
{
  unsigned char *ptr1,*ptr2;
  if (blk_num == 19551)
    {
      int i;
      i = 0;
    }
  dblock->block = ent_blk(dblock->entry);
  dblock->act_tree_head = head_block;
  dblock->act_blk_num = blk_num;
  if (!(get_ent_copy(dblock->entry,seg,blk_num))) return(1);
  dblock->blk_num = str2long(dblock->block,blk_id_pos);
  dblock->cont_ptr = str2long(dblock->block,blk_nxt_id_pos);
  dblock->tree_head = str2long(dblock->block,blk_top_id_pos);
  dblock->size = str2short(dblock->block,blk_end_pos)
    - blk_data_start;
  dblock->level = dblock->block[blk_level_pos] - '0';
  dblock->type = blk_typ(dblock->block);
  if (dblock->type == 'D') dblock->directory = 'Y';
  else dblock->directory = 'N';
  ptr1 = &dblock->block[blk_data_start];
  ptr2 = ptr1 + dblock->size;
  return(0);
}

int check_saf(seg,nam,len)
     SEGD *seg;
     STR nam;
     int len;
{
  BLOCK head_blk_num = disk_blocks[0].act_blk_num;
  BLOCK cont_ptr;
  int err = 0;
  STR ptr1,ptr2;
  int blk_error = 0;
  do {
    blocks_marked++;
    total_blocks_marked++;
    if (disk_blocks[0].blk_num != disk_blocks[0].act_blk_num)
      {
	printf("\nBLOCK %lu HAS WRONG BLOCK ID, HEAD BLOCK %lu",
	       disk_blocks[0].act_blk_num,head_blk_num);
	err++;
      }
    if (disk_blocks[0].tree_head != head_blk_num)
      {
	printf("\nBLOCK %lu HAS WRONG HEAD BLOCK, HEAD BLOCK %lu",
	       disk_blocks[0].act_blk_num,head_blk_num);
	err++;
      }
    if (!set_block_used(disk_blocks[0].act_blk_num))
      {
	printf("\nSAF BLOCK %lu USED MULTIPLE TIMES, HEAD BLOCK %lu",
	       disk_blocks[0].act_blk_num,head_blk_num);
	err++;
      }
    if (disk_blocks[0].type != seq_typ)
      {
	printf("\nBLOCK %lu NOT A SAF BLOCK, HEAD BLOCK %lu",
	       disk_blocks[0].act_blk_num,head_blk_num);
	err++;
      }
    if (disk_blocks[0].level != '0')
      {
	printf("\nBLOCK %lu SAF not LEVEL 0, HEAD BLOCK %lu",
	       disk_blocks[0].act_blk_num,head_blk_num);
	err++;
      }
    ptr1 = disk_blocks[0].block + blk_data_start;
    ptr2 = ptr1 + disk_blocks[0].size;
    while (ptr1 < ptr2) ptr1 += *ptr1 + 1;
    if (ptr1 != ptr2)
      {
	printf("\nBLOCK %lu SAF SIZE ERROR, HEAD BLOCK %lu",
	       disk_blocks[0].act_blk_num,head_blk_num);
	err++;
      }
    cont_ptr = disk_blocks[0].cont_ptr;
  } while (!err && cont_ptr
	   && !(blk_error = read_block(seg,
				       cont_ptr,
				       &disk_blocks[0],
				       head_blk_num)));
  if (blk_error)
    {
      printf("\nERROR READING SAF BLOCK %lu HEAD BLOCK %lu",cont_ptr,
	     head_blk_num);
      err++;
    }
  return(err);
}

int check_struct_level(seg,dsk_blk_ind,next_block,start_name,start_name_len,
		       end_name,end_name_len,level)
     SEGD *seg;
     int dsk_blk_ind;
     BLOCK next_block;
     STR start_name,end_name;
     int start_name_len,end_name_len,level;
{
  STR ptr1,ptr2,ptr3,ptr4;
  int errs = 0;
  int i,j;
  BLOCK block_num,next_block_num;
  unsigned char *namebuf,*namebuf1;
  int namelen,namelen1,err = 0;
  blocks_marked++;
  total_blocks_marked++;
  namebuf = &disk_blocks[dsk_blk_ind].last_block_name[0];
  namelen = disk_blocks[dsk_blk_ind].last_block_name_len;
  namebuf1 = &disk_blocks[dsk_blk_ind].current_name[0];
  if (disk_blocks[dsk_blk_ind].blk_num
      != disk_blocks[dsk_blk_ind].act_blk_num)
    {
      printf("\nBLOCK %lu HAS WRONG BLOCK ID, HEAD BLOCK %lu",
	     disk_blocks[dsk_blk_ind].act_blk_num,
	     disk_blocks[dsk_blk_ind].tree_head);
      path(dsk_blk_ind);
      err++;
    }
  if (disk_blocks[dsk_blk_ind].tree_head != disk_blocks[0].act_blk_num)
    {
      printf("\nBLOCK %lu HAS WRONG HEAD BLOCK, HEAD BLOCK %lu",
	     disk_blocks[dsk_blk_ind].act_blk_num,
	     disk_blocks[0].act_blk_num);
      path(dsk_blk_ind);
      err++;
    }
  if (disk_blocks[dsk_blk_ind].level != level)
    {
      printf("\n LEVEL FOR BLOCK IS %d BUT EXPECTED %d",
	     disk_blocks[dsk_blk_ind].level, level);
      path(dsk_blk_ind);
      return(++errs);
    }
  if (!set_block_used(disk_blocks[dsk_blk_ind].act_blk_num))
    {
      printf("\nBLOCK %lu USED MULTIPLE TIMES",
	     disk_blocks[dsk_blk_ind].act_blk_num);
      path(dsk_blk_ind);
      return(++errs);
    }
  if (disk_blocks[dsk_blk_ind].cont_ptr != next_block) {
    printf("\n NEXT POINTER FOR BLOCK IS %lu BUT EXPECTED %lu",
	   disk_blocks[dsk_blk_ind].cont_ptr, next_block);
    path(dsk_blk_ind);
    return(++errs);
  }
  ptr1 = disk_blocks[dsk_blk_ind].block + blk_data_start + 1;
  i = *ptr1++;
  namelen1 = 0; while (i-- > 0) namebuf1[namelen1++] = *ptr1++;
  disk_blocks[dsk_blk_ind].current_name_len = namelen1;
  if (namelen && string_compare(namebuf,namelen,namebuf1,namelen1) != 2)
    {
      printf("\nFIRST ENTRY SMALLER THAN LAST ENTRY IN PREV BLOCK");
      printf("\n PREV NAME: ");
      dspl_string(namebuf,namelen);
      printf("\n CURR NAME: ");
      dspl_string(namebuf1,namelen1);
      path(dsk_blk_ind);
    }
  if (string_compare(start_name,start_name_len,namebuf1,namelen1) == 1)
    {
      printf("\nFIRST ENTRY SMALLER THAN INDEX SPLITKEY");
      printf("\n SPLIT KEY: ");
      dspl_string(start_name,start_name_len);
      printf("\n CURR NAME: ");
      dspl_string(namebuf1,namelen1);
      path(dsk_blk_ind);
    }
  ptr1 = disk_blocks[dsk_blk_ind].block + blk_data_start;
  ptr2 = ptr1 + disk_blocks[dsk_blk_ind].size;
  while (ptr1 < ptr2) /* check block size */
    {
      ptr1++;
      ptr1 += *ptr1 + 1;
      if (ptr1 < ptr2)  /* not at splitkey */
	{
	  ptr1 += *ptr1 + 1;
	}
    }
  if (ptr1 != ptr2)
    {
      printf("\nBLOCK %lu SIZE ERROR, HEAD BLOCK %lu",
	     disk_blocks[dsk_blk_ind].act_blk_num,
	     disk_blocks[dsk_blk_ind].tree_head);
      err++;
    }
  ptr1 = disk_blocks[dsk_blk_ind].block + blk_data_start;
  while ((ptr1 < ptr2) && !fatal_error) {
    namelen1 = *ptr1++;
    i = *ptr1++;
    ptr3 = ptr1 + i;
    ptr3 += *ptr3 + 1;
    j = i;
    ptr4 = ptr1;
    while (j-- > 0) namebuf1[namelen1++] = *ptr4++;
    disk_blocks[dsk_blk_ind].current_name_len = namelen1;
    if (string_compare(namebuf,namelen,namebuf1,namelen1) != 2
	&& namelen) {
      printf("\nCOLLATION ERROR");
      path(dsk_blk_ind);
      return(++errs);
    }
    if (ptr3 < ptr2)		/* not on last (splitkey) entry */
      {
	block_num = str2long(ptr1, i + 1);
	ptr1 = ptr3 + *(ptr3+1) + 3;
	if (disk_blocks[dsk_blk_ind].level != 0) {
	  next_block_num = 0;
	  if (ptr1 < ptr2) next_block_num = str2long(ptr1,0);
	  else if (disk_blocks[dsk_blk_ind].cont_ptr)
	    if (get_ent_copy(temp_block.entry,
			     seg,
			     disk_blocks[dsk_blk_ind].cont_ptr)) {
	      ptr1 = ent_blk(temp_block.entry) + blk_data_start + 1;
	      ptr4 = ent_blk(temp_block.entry)
		+ str2short(ent_blk(temp_block.entry),blk_end_pos);
	      ptr1 += *ptr1 + 1;
	      i = *ptr1++;
	      if (ptr1 + i + 1 < ptr4) next_block_num = str2long(ptr1,0);
	    }
	  if (read_block(seg,
			 block_num,&disk_blocks[dsk_blk_ind+1],
			 disk_blocks[dsk_blk_ind].tree_head)) {
	    printf("\nERROR READING BLOCK %lu",block_num);
	    path(dsk_blk_ind);
	  }
	  else errs += check_struct_level(seg,dsk_blk_ind + 1,next_block_num,
					  namebuf,namelen,namebuf1,namelen1,
					  disk_blocks[dsk_blk_ind].level-1);
	}
	namelen = 0;
	while (namelen < namelen1) {
	  namebuf[namelen] = namebuf1[namelen];
	  namelen++;
	}
	disk_blocks[dsk_blk_ind].last_block_name_len = namelen;
      }
    else if (string_compare(end_name,end_name_len,namebuf1,namelen1) != 0)
      /* on last (splitkey) entry */
      {
	printf("\nSPLIT KEY MISMATCH");
	path(dsk_blk_ind);
	return(++errs);
      }
    ptr1 = ptr3;
  }
  return(errs);
}

void check_struct(seg, blk_num, name, len, dirdepth)
     SEGD *seg;
     BLOCK blk_num;
     STR name;
     int len;
     int dirdepth;
{
  unsigned char start[1],end[3];
  unsigned long int blocks_in_directory = 0;
  int directory = 0;
  /*  reset_disk_blocks(); */
  int i = 0;
  while (i < MAX_LEVELS) {
    disk_blocks[i].last_block_name_len = 0;
    disk_blocks[i].current_name_len = 0;
    i++;
  }
  if (read_block(seg, blk_num,&disk_blocks[0],blk_num)) {
    printf("\nERROR READING BLOCK");
    ++error_count;
    return;
  }
  if (!root_P(disk_blocks[0].block)) {
    printf("\nNOT A ROOT BLOCK\n");
    ++error_count;
    return;
  }
  if (blk_typ_P(disk_blocks[0].block, dir_typ)) {
    printf("\n%*s",dirdepth," ");
    dspl_string(name, len);
    printf(" -> block number: %lu",blk_num);
    directory = 1;
  }
  blocks_marked = 0;
  end[0] = 255;
  end[1] = disk_blocks[0].level + '0';
  if (disk_blocks[0].type == seq_typ) /* SEQUENTIAL FILE */
    {
      error_count += check_saf(seg,name,len);
      blocks_in_directory += blocks_marked;
    }
  else if (!(i=check_struct_level(seg,0,0L,start,0,end,2,disk_blocks[0].level)) &&
	   follow_dirs &&
	   (blk_typ_P(disk_blocks[0].block, dir_typ))) {
    unsigned char key[256], val[256];
    int klen=0, vlen;
    HAND han;
    bt_open(seg,blk_num,&han,0);
    blocks_in_directory += blocks_marked;
    while ((klen = bt_next(&han, key, klen, key)) > 0)
      {
	if (((vlen = bt_get(&han, key, klen, val)) >= 5) && (4 == val[0]))
	  {
	    blocks_marked = 0;
	    check_struct(seg, str2long(val,1), key, klen, 1+dirdepth);
	    blocks_in_directory += blocks_marked;
	  }
      }
  }
  else  { error_count += i; blocks_in_directory += blocks_marked; }
  blocks_marked = blocks_in_directory;
  if (directory)
    { printf("\n%*s%lu BLOCKS",dirdepth," ",blocks_in_directory); }
}

BLOCK nextnum(hand, key, klen)
     HAND *hand;
     STR key;
     int klen;
{
  klen = bt_next(hand,key,klen,key);
  if (klen <= 0) return 0;
  else if (klen != 4) {
    printf("BAD SIZE KEY IN FREELIST\n");
    exit(2);
  }
  return str2long(key,0);
}

int wbcheck_usage()
{
  printf("\n\
Usage: wbcheck FILE\n\
Usage: wbcheck FILE BLOCKNUMBER\n\
\n\
  Checks the structure of the database named by FILE and\n\
  reclaims temporary trees to the freelist.\n\
");
  return 2;
}

int main( argc, argv )
     int argc;
     char *argv[];
{
  unsigned int i = 0;
  BLOCK blk_num = 0;
  SEGD *seg;
  if (argc < 2) {
    printf("NO FILE\n");
    return wbcheck_usage();
  }
  if (argc > 3) {
    printf("TOO MANY ARGUMENTS\n");
    return wbcheck_usage();
  }
  if (argc > 2) {
    char *ptr1, *ptr = &argv[2][0];
    ptr1 = ptr;
    while ( (*ptr1 >= '0') && (*ptr1 <= '9')) ptr1++;
    if (*ptr1) {
      printf("NOT A NUMBER\n");
      return wbcheck_usage();
    }
    if (!cnv_string_to_long(ptr, ptr1, &blk_num)) {
      printf("NUMBER TOO LARGE\n");
      return wbcheck_usage();
    }
    blkgc = 0;
    follow_dirs = 0;
  }
  else if (0==strcmp("--help", argv[1])) {
    wbcheck_usage();
    return 0;
  }
  init_wb(75, 150, 16384);
  seg = open_seg(argv[1], blkgc);
  if (seg==0) {
    seg = open_segd(argv[1], blkgc, !0);
    if (seg==0) {
      printf("COULD NOT OPEN FILE \"%s\"\n", argv[1]);
      return 2;
    }
    printf("OPENING CORRUPTLY CLOSED FILE\n");
  }
  while ( (i < MAX_LEVELS) && (disk_blocks[i].entry = allocate_ent()) ) i++;
  if ( i < MAX_LEVELS || !(temp_block.entry = allocate_ent()) ) {
    printf("UNABLE TO ALLOCATE ENOUGH ENTRIES\n");
    return 0;
  }
  i = 0; while (i < MAP_SIZE) free_map[i++] = 0;
  error_count = 0;
  fatal_error = 0;
  total_blocks_marked = 0;
  check_struct(seg, blk_num, "", 0, 1);
  printf("\n%lu BLOCKS MARKED IN USE",total_blocks_marked);
  if (error_count) {
    printf("\n %d ERRORS\n",error_count);
    return 1;
  }
  printf("\nNO STRUCTURE ERRORS FOUND\n");
  if (!blkgc) return 0;
  {
    HAND han;
    unsigned char key[5*4], tmp[4];
    BLOCK used=0, num_freed=0, num_collected=0, fnum;
    int flc_image_len, i, j=0;
    bt_open(seg,0L,&han,0);
    if (4 != bt_get(&han,"USED",4,key)) exit(2);
    used = str2long(key,0);
    flc_image_len = bt_get(&han, "FLC",3,key);

    bt_open(seg,2L,&han,0);
    if (0 > (flc_image_len))
      flc_image_len = 0;
    i = -4+(flc_image_len);
    while (0 <= i) {
      fnum = str2long(key, i);
      /*      printf("\n %lu: used %d FREELIST %d\n",
	      fnum,get_block_used(fnum),bt_get(&han, &key[i], 4, tmp)); */
      if (!(get_block_used(fnum) || (0 < bt_get(&han, &key[i], 4, tmp)))) {
	seg_flc(seg)[j++] = str2long(key, i);
	set_block_used(fnum);
      }
      i = -4+(i);
    }
    seg_set_flc_len(seg, j);

    /* scan the first time for blocks which shouldn't be there. */
    /* This is so that we won't use bad blocks to extend FREELIST */
    blk_num = nextnum(&han, key, 0);
    while (blk_num) {
      if (get_block_used(blk_num)) {
	printf("\nA block in the FREELIST is acutally being used: %lu", blk_num);
	num_freed++;
	flush_flc(seg,flc_len-5);	/* make sure there is room in flc for any deleted blocks */
	bt_rem(&han,key,4,0L);
      }
      else set_block_used(blk_num); /* so that stuff on the FREELIST is marked. */
      blk_num = nextnum(&han, key, 4);
    }
    blk_num=0;
    /* add all of bitmap is that is unused. */
    printf("\n    reclaiming unused blocks:");
    while (used > ++blk_num)
      if (!get_block_used(blk_num)) {
	printf(" %lu", blk_num);
	num_collected++;
	flc_fill(seg); /* make sure there are enought blks in flc for splits */
	flush_flc(seg,flc_len-2);	/* make sure there is room in flc for a collected block */
	lck(seg_lck(seg));
	seg_flc(seg)[seg_flc_len(seg)]=blk_num;
	seg_set_flc_len(seg,1+seg_flc_len(seg));
	unlck(seg_lck(seg));
      }
    close_seg(seg,1);		/* need to close it because changes have been made. */
    printf("\nFREELIST: %lu blocks removed; %lu blocks added.\n",num_freed, num_collected);
  }
  return 0;
}

void display_disk_statistics(b, t, r, w)
     BLOCK b;
     char t;
     unsigned long r,w;
{}
