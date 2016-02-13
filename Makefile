all:
	happy -gca FJ/ParFjSyntax.y
	alex -g FJ/LexFjSyntax.x
	ghc --make FJ/TestFjSyntax.hs -o FJ/TestFjSyntax

clean:
	-rm -f FJ/*.log FJ/*.aux FJ/*.hi FJ/*.o FJ/*.dvi

distclean: clean
	-rm -f FJ/DocFjSyntax.* FJ/LexFjSyntax.* FJ/ParFjSyntax.* FJ/LayoutFjSyntax.* FJ/SkelFjSyntax.* FJ/PrintFjSyntax.* FJ/TestFjSyntax.* FJ/AbsFjSyntax.* FJ/TestFjSyntax FJ/ErrM.* FJ/SharedString.* FJ/ComposOp.* FJ/fj_syntax.dtd FJ/XMLFjSyntax.* Makefile*
		-rmdir -p FJ/

