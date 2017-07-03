# cl-false
Implementing FALSE in Common Lisp using macros (mostly finished)

Example program (deliberately over-complex to demonstrate major functionalities):

`{This leaves all the numbers from i (up/down) to 10 on the stack}`

`1 i:
[10>$$[[1-]]?\~[[1+]]?\%]f:
[$;@!\:]u:
[[$10=~][$f;!i$@\u;!;]#]l:
i;l;!`

Other code snippets:

`{Over}
[\$@@]o:`

`{Modulo (requires over)}
[[o;!o;!-0\>~][$@@-\]#%]m:`

`{Repeat (uses i, f)}
[i:f:[i;0>][f;!i;1-i:]#]r:`

Example: `o;!o;!o;!o;!` can be rewritten as `o;4r;!`

`{Basic prime number detector (requires mod, over, repeat)}
[2[o;4r;!m;!0=~@@>&][1+]#=]p:`

Another example: This inefficient but compact function leaves on the stack all prime numbers from 2 to the top number on the stack using x as a temporary variable:
`[x:2[$x;>~][$p;!\$1+@~[\%]?]#%]s:`

`{Digits (take an integer as input)}
[0[^$$'0\>~\'9>~&]['0-\10*+]#%]d:`
