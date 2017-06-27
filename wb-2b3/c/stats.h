/* This file was generated by scm2c from source file "stats.scm" */

extern long next_ct;

extern long next_fct;

extern long prev_ct;

extern long prev_fct;

extern long get_ct;

extern long get_fct;

extern long put_ct;

extern long put_fct;

extern long rem_ct;

extern long rem_fct;

extern long ge_ct;

extern long ge_fct;

extern long tge_ct;

extern long tge_fct;

extern long tce_ct;

extern long tce_fct;

extern int chains_to_next;

extern int deferred_inserts;

extern int split_index_inserts;

extern int index_screw_case;

extern int block_splits;

extern int block_deletes;

extern int deferred_deletes;

extern long dir_dty_ct;

extern long read_ct;

extern long write_ct;

extern long read_fl_ct;

extern long write_fl_ct;

extern long flush_ct;

SCM_EXPORT int clear_stats P((void));

SCM_EXPORT int cstats P((void));

SCM_EXPORT int stats P((void));

SCM_EXPORT void show_buffer P((ENTRY *ent));

extern int buf_verbose_P;

SCM_EXPORT int show_buffer_1 P((ENTRY *ent));

SCM_EXPORT int show_buffers P((void));

SCM_EXPORT int sb P((void));