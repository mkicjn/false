#ifndef HASH_H
#define HASH_H
#include <stdio.h>
#include <stdlib.h>
typedef struct bucket_s {
	unsigned long key;
	void *val;
	struct bucket_s *cdr;
} bucket_t;
typedef struct {
	int size; // # of possible entries
	bucket_t **mem;
} table_t;
unsigned long hash_key(char *);
bucket_t *new_bucket(unsigned long key,void *);
void free_bucket(bucket_t *);
table_t *new_table(int);
void free_table(table_t *);
bucket_t *add_bucket(table_t *,bucket_t *);
bucket_t *add_entry(table_t *,char *,void *);
bucket_t *get_bucket(table_t *,char *);
void *get_entry(table_t *,char *);
void set_entry(table_t *,char *,void *);
#endif
