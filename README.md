# cl-false
Implementations of FALSE in Common Lisp and C

The Common Lisp version is super old, and very slow. I probably wrote it in high school, so it isn't very good.
I wrote the new C version recently just for the heck of it. It doesn't have memory management, but it might soon.

The new C version is barebones, but has a few new instructions for convenience, and basic error checking.
If I add memory management similar to csl, it would be easy to expand on the error checking.

The new update includes new commands for manually allocating and freeing memory, as well as saving variables of any name as values in a hash table.
The hash table implementation used was also written from scratch, coming from an older project of mine and cleaned up a bit.

Useful examples:

0 to (+/-)N: `[$10>$$[[1-]]?\~[[1+]]?\%\[$10=~][$@@1O!]#\%]`

Over: `[\$@@]` or `[1O]`

Modulo: `[[1O1O-0\>~][$@@-\]#%]`

Take numeric input: `[0[^$$'0\>~\'9>~&]['0-\10*+]#%]`

Factorial: `[$[$2>][1-$@*\]#%]`

Prime number detector: `[2[1O1O1O1O[1O1O-0\>~][$@@-\]#%0=~@@>&][1+]#=]`

Prime numbers from 2 to N (requires the above in the p variable): `[2[$2O>~][$$p;![@@$]?%1+]#%%]`
