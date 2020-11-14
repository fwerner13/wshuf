# wshuf

wshuf - generate random weighted permutations

## Synopsis

**wshuf** [*OPTION*]... [*FILE*]

## Description

Write a random permutation of the input lines to standard output, using weights. 

The input lines must follow the format

 `value;weight` 



**-n**, **--lines**=*COUNT*

output at most COUNT lines

**-s, --separator**=*CHAR*

use *CHAR* to separate values and weights. Defaults to semicolon.

**-v**, **--verbose**

Display the lines including the weight values.

[TBD] **--help**

display this help and exit

[TBD] **--version**

output version information and exit



[TBD] With no FILE, or when FILE is -, read standard input.