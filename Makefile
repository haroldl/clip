
clip: Main.hs Lexer.hs PythonIndention.hs Token.hs Parser.hs AST.hs Optimize.hs CodeGen.hs
	ghc $< -o clip

%.hs : %.y
	happy -i -d -a $<

%.hs : %.x
	alex --ghc $<

test : UnitTests.hs Lexer.hs Token.hs PythonIndention.hs
	runghc UnitTests.hs

clean:
	-rm *.o *.hi Lexer.hs Parser.info Parser.hs clip
	-rm -rf dist
