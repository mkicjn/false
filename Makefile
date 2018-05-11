CC=clang
CFLAGS=

false: false.c src/hash.c
	$(CC) false.c src/hash.c $(CFLAGS)
