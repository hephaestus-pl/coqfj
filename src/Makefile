all:
	@echo "Usage: make ffj or make fj"

ffj:
	bnfc -p FFJ.Syntax ffj_syntax.cf
	happy -gca FFJ/Syntax/Parffj_syntax.y
	alex -g FFJ/Syntax/Lexffj_syntax.x
	@#(cd FFJ/; latex Docffj_syntax.tex; dvips Docffj_syntax.dvi -o Docffj_syntax.ps)
	ghc --make FFJ/Syntax/Testffj_syntax.hs -o FFJ/Syntax/Testffj_syntax

fj:
	bnfc -p FJ.Syntax fj_syntax.cf
	happy -gca FJ/Syntax/Parfj_syntax.y
	alex -g FJ/Syntax/Lexfj_syntax.x
	@#(cd FJ/Syntax/; latex Docfj_syntax.tex; dvips Docfj_syntax.dvi -o Docfj_syntax.ps)
	ghc --make FJ/Syntax/Testfj_syntax.hs -o FJ/Syntax/Testfj_syntax

cclean_ffj:
	-rm -f FFJ/*.log FFJ/*.aux FFJ/*.hi FFJ/*.o FFJ/*.dvi
	-rm -f FFJ/Docffj_syntax.ps

clean_fj:
	-rm -f FJ/Syntax/*.log FJ/Syntax/*.aux FJ/Syntax/*.hi FJ/Syntax/*.o FJ/Syntax/*.dvi

distclean_ffj: clean
	-rm -f FFJ/Docffj_syntax.* FFJ/Lexffj_syntax.* FFJ/Parffj_syntax.* FFJ/Layoutffj_syntax.* FFJ/Skelffj_syntax.* FFJ/Printffj_syntax.* FFJ/Testffj_syntax.* FFJ/Absffj_syntax.* FFJ/Testffj_syntax FFJ/ErrM.* FFJ/SharedString.* FFJ/ffj_syntax.dtd FFJ/XMLffj_syntax.* Makefile*
	-rmdir -p FFJ/

distclean_fj: clean
	-rm -f FJ/Syntax/DocFjSyntax.* FJ/Syntax/LexFjSyntax.* FJ/Syntax/ParFjSyntax.* FJ/Syntax/LayoutFjSyntax.* FJ/Syntax/SkelFjSyntax.* FJ/Syntax/PrintFjSyntax.* FJ/Syntax/TestFjSyntax.* FJ/Syntax/AbsFjSyntax.* FJ/Syntax/TestFjSyntax FJ/Syntax/ErrM.* FJ/Syntax/SharedString.* FJ/Syntax/ComposOp.* FJ/Syntax/fj_syntax.dtd FJ/Syntax/XMLFjSyntax.* Makefile*
	-rmdir -p FJ/Syntax/
	-rm dir -p FJ
