myinterpreter : myinterpreter.hs Grammar.hs Tokens.hs
	ghc --make myinterpreter

Grammar.hs : Grammar.y
	happy Grammar.y
    
Tokens.hs : Tokens.x
	alex Tokens.x    