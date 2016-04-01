all:
	@echo "Usage: make ffj or make fj"

ffj:
	happy -gca FFJ/Parffj_syntax.y
	alex -g FFJ/Lexffj_syntax.x
	@#(cd FFJ/; latex Docffj_syntax.tex; dvips Docffj_syntax.dvi -o Docffj_syntax.ps)
	ghc --make FFJ/Testffj_syntax.hs -o FFJ/Testffj_syntax

fj:
	happy -gca FJ/ParFjSyntax.y
	alex -g FJ/LexFjSyntax.x
	ghc --make FJ/TestFjSyntax.hs -o FJ/TestFjSyntax

clean_ffj:
	-rm -f FFJ/*.log FFJ/*.aux FFJ/*.hi FFJ/*.o FFJ/*.dvi
	-rm -f FFJ/Docffj_syntax.ps

clean_fj:
	-rm -f FJ/*.log FJ/*.aux FJ/*.hi FJ/*.o FJ/*.dvi

distclean_ffj: clean
	-rm -f FFJ/Docffj_syntax.* FFJ/Lexffj_syntax.* FFJ/Parffj_syntax.* FFJ/Layoutffj_syntax.* FFJ/Skelffj_syntax.* FFJ/Printffj_syntax.* FFJ/Testffj_syntax.* FFJ/Absffj_syntax.* FFJ/Testffj_syntax FFJ/ErrM.* FFJ/SharedString.* FFJ/ffj_syntax.dtd FFJ/XMLffj_syntax.* Makefile*
	-rmdir -p FFJ/

distclean_fj: clean
	-rm -f FJ/DocFjSyntax.* FJ/LexFjSyntax.* FJ/ParFjSyntax.* FJ/LayoutFjSyntax.* FJ/SkelFjSyntax.* FJ/PrintFjSyntax.* FJ/TestFjSyntax.* FJ/AbsFjSyntax.* FJ/TestFjSyntax FJ/ErrM.* FJ/SharedString.* FJ/ComposOp.* FJ/fj_syntax.dtd FJ/XMLFjSyntax.* Makefile*
	-rmdir -p FJ/
