2017-11-23: Dug down into the differences between the backends.

It turns out that the key is the variable declaration order - swapping
two lines is enough to get a different optimum solution (among the two
optimal solutions).

Note that the Makefile currently has the path to the MiniZinc bundle
hard-coded (which will not work for anyone else than me).
