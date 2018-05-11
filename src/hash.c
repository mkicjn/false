#include "hash.h"
unsigned long hash_key(char *str) // lazy
{
	unsigned long key=0;
	for (char *c=str;*c;c++) {
		key+=*c;
		key*=*c;
		key<<=8;
		key/=*c;
	}
	//fprintf(stderr,"-- Key: %s -> %lu --\n",str,key);
	return key;
}
bucket_t *new_bucket(unsigned long key,void *val)
{
	bucket_t *b=malloc(sizeof(bucket_t));
	b->key=key;
	b->val=val;
	b->cdr=NULL;
	return b;
}
void free_bucket(bucket_t *b)
{
	free(b->val);
	if (b->cdr)
		free_bucket(b->cdr);
	free(b);
}
table_t *new_table(int size)
{
	table_t *table=malloc(sizeof(table_t));
	table->mem=calloc(size,sizeof(bucket_t *));
	table->size=size;
	return table;
}
void free_table(table_t *table)
{
	for (int i=0;i<table->size;i++)
		if (table->mem[i])
			free_bucket(table->mem[i]);
	free(table->mem);
	free(table);
}
bucket_t *add_bucket(table_t *table,bucket_t *entry)
{
	unsigned long key=entry->key;
	bucket_t **def=&table->mem[key%table->size];
	if (*def) {
		bucket_t *d=*def;
		for (;d->cdr&&d->key!=key;d=d->cdr);
		if (d->key==key) {
			fprintf(stderr,"-- Key already exists in table --\n");
			if (d->val)
				free(d->val);
			d->val=entry->val;
			free(entry);
		} else
			d->cdr=entry;
		def=&d;
	} else
		*def=entry;
	return *def;
}
bucket_t *add_entry(table_t *table,char *str,void *entry)
{
	unsigned long key=hash_key(str);
	bucket_t *bucket=new_bucket(key,entry);
	return add_bucket(table,bucket);
}
bucket_t *get_bucket(table_t *table,char *str)
{
	unsigned long key=hash_key(str);
	bucket_t *def=table->mem[key%table->size];
	for (;def&&def->key!=key;def=def->cdr);
	if (!def) {
		//fprintf(stderr,"-- Entry could not be found --\n");
		return NULL;
	}
	return def;
}
void *get_entry(table_t *table,char *str)
{
	return get_bucket(table,str)->val;
}
void set_entry(table_t *table,char *str,void *entry) // add_entry() without collision handling
{
	unsigned long key=hash_key(str);
	bucket_t *def=table->mem[key%table->size];
	for (;def&&def->key!=key;def=def->cdr);
	if (def)
		def->val=entry;
}
