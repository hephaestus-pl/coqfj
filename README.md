#CoqFJ
Featherweight Java is a minimimum core calculus for Java formalized in [this paper](https://www.cis.upenn.edu/~bcpierce/papers/fj-toplas.pdf)

CoqFJ is a mechanization of the standard theorems for type safety, i.e. progress and preservation, using coq.

To compile the code, simply run ```make``` at the root folder.

You can also generate the makefile from scratch running ```coq_makefile -f _CoqProject -o makefile```

If you use CoqIDE, make sure to enable Project File options at ```Edit -> Preferences -> Project Project file options are "append to arguments"```.

And ```Default name for project file _CoqProject```
