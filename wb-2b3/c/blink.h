/* This file was generated by scm2c from source file "blink.scm" */

SCM_EXPORT void short2str P((unsigned char *str,int pos,int cint));

SCM_EXPORT short str2short P((unsigned char *str,int pos));

SCM_EXPORT void long2str P((unsigned char *str,int pos,long clong));

SCM_EXPORT long str2long P((unsigned char *str,int pos));

SCM_EXPORT int set_field P((unsigned char *blk,int b_pos,unsigned char *val_str,int f_pos,int f_len));

extern unsigned char leaf_split_key_str[];

SCM_EXPORT void init_leaf_blk P((unsigned char *nblk,long bnum,int typ));

SCM_EXPORT void reroot P((unsigned char *rblk,unsigned char *nblk,long bnum,int bsiz));

SCM_EXPORT void init_next_blk P((unsigned char *blk,unsigned char *nblk));

SCM_EXPORT int split_key_pos P((unsigned char *blk));

SCM_EXPORT int blk_find_pos P((unsigned char *blk,unsigned char *key_str,int k_len,int *pkt));

SCM_EXPORT ENTRY *chain_find P((ENTRY *ent,int accmode,unsigned char *key_str,int k_len,int *pkt));

SCM_EXPORT ENTRY *find_ent P((ENTRY *ent,int desired_level,int last_level,unsigned char *key_str,int k_len));

SCM_EXPORT int blk_prev_key P((unsigned char *blk,int pos));

SCM_EXPORT int get_this_val P((unsigned char *blk,int b_pos,unsigned char *ans_str));

SCM_EXPORT int get_this_key P((unsigned char *blk,int b_pos,unsigned char *key_str,unsigned char *ans_str,ENTRY *ent,int k_len,int *pkt));

SCM_EXPORT int chain_next P((ENTRY *ent,unsigned char *key_str,int k_len,unsigned char *ans_str,int *pkt));

SCM_EXPORT int blk_change_size P((unsigned char *blk,int loc,int growth,int bsiz));

SCM_EXPORT int blk_remove_key_and_val P((unsigned char *blk,int b_pos,int bsiz));

extern int defer_insert_updates_P;

SCM_EXPORT int parent_insert_update P((SEGD *seg,long top_id,int level,unsigned char *nkey_str,int nk_len,long n_id));

SCM_EXPORT int at_split_key_pos_P P((unsigned char *blk,int pos));

SCM_EXPORT ENTRY *next_nonempty_ent P((SEGD *seg,long blknum));

SCM_EXPORT int recon_this_key P((unsigned char *blk,int pos,unsigned char *key_str,int k_pos,int k_len));

SCM_EXPORT int blk_insert_and_adjust P((unsigned char *blk,int b_pos,int k_pos,unsigned char *key_str,int k_len,unsigned char *val_str,int v_len,int bsiz));

SCM_EXPORT int blk_simple_insert P((unsigned char *blk,int b_pos,int k_pos,unsigned char *key_str,int k_len,unsigned char *val_str,int v_len,int bsiz));

SCM_EXPORT int blk_change_existing_value P((unsigned char *blk,int b_pos,unsigned char *key_str,int k_len,unsigned char *val_str,int v_len,int bsiz));

SCM_EXPORT int val_leaf_split P((unsigned char *blk,unsigned char *nblk,int b_pos,unsigned char *key_str,int k_pos,int k_len,unsigned char *val_str,int v_len));

SCM_EXPORT int qpastp_leaf_split P((unsigned char *blk,unsigned char *nblk,int b_pos,unsigned char *key_str,int k_pos,int k_len,unsigned char *val_str,int v_len));

SCM_EXPORT int pastp_leaf_split P((unsigned char *blk,unsigned char *nblk,int b_pos,unsigned char *key_str,int k_pos,int k_len,unsigned char *val_str,int v_len));

SCM_EXPORT int dummy_leaf_split P((unsigned char *blk,unsigned char *nblk,int b_pos,unsigned char *key_str,int k_pos,int k_len,unsigned char *val_str,int v_len));

SCM_EXPORT int_function select_split_fun P((int type));

SCM_EXPORT int chain_put P((ENTRY *ent,unsigned char *key_str,int k_len,unsigned char *val_str,int v_len,int *pkt,ENTRY *xent,int wcb));
