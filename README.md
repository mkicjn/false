# cl-false
Implementing FALSE in Common Lisp using macros (mostly finished)

Example program (deliberately over-complex to demonstrate major functionalities):

`{This leaves all the numbers from i (up/down) to 10 on the stack}`

`1 i:
[10>$$[[1-]]?\~[[1+]]?\%]f:
[$;@!\:]u:
[[$10=~][$f;!i$@\u;!;]#]l:
i;l;!`

Here's a better function which does the same thing but to the top item on the stack, using no variables:

`[$10>$$[[1-]]?\~[[1+]]?\%\[$10=~][$@@1O!]#\%]`

Other code snippets:

`{Over}
[\$@@]o:` or `[1O]o:` using "pick"

`{Modulo (based on Over)}
[[1O1O-0\>~][$@@-\]#%]m:`

`{Repeat (uses i, f)}
[i:f:[i;0>][f;!i;1-i:]#]r:`

Example: `[...]f:f;!f;!f;!f;!` becomes `[...]4r;!`

`{Digits (take an integer as input)}
[0[^$$'0\>~\'9>~&]['0-\10*+]#%]d:`

`{Basic prime number detector (requires Over, Mod, and Repeat}
[2[o;4r;!m;!0=~@@>&][1+]#=]p:`
or `[2[1O1O1O1O[1O1O-0\>~][$@@-\]#%0=~@@>&][1+]#=]p:` requiring nothing

Another example: This inefficient but compact function leaves on the stack all prime numbers from 2 to the top number on the stack using x as a temporary variable:
`[x:2[$x;>~][$p;!\$1+@~[\%]?]#%]s:`

Final example: The above function but better in every single way possible

`[2[$2O>~][$$p;![@@$]?%1+]#%%]s:`
