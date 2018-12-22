{ 
module Grammar where 
import Tokens 
}

%name parseKUCI 
%tokentype { Token } 
%error { parseError }
%token 
    output { TokenOutput _ } 
    read  { TokenRead _ }
	pos { TokenPos _ }
	val { TokenVal _ }
    var { TokenVar _ $$ }
	format { TokenFormat _ $$ }
	'=='  { TokenEq _ }
	'!='  { TokenNotEq _ }
	'>'  { TokenGT _ }
	'>='  { TokenGET _ }
	'<'  { TokenLT _ }
	'<='  { TokenLET _ }
	'^' { TokenConjunction _ }
    '/' { TokenFilter _ } 
	'$' { TokenExist _ }
    '(' { TokenLParen _ } 
    ')' { TokenRParen _ } 

%left output
%left '$'
%left '/'
%left '^'
%left read
%left '==' '!=' '>' '>=' '<' '<='
%left pos val
%% 
Exp : Exp '^' Exp            { Conjunction $1 $3 }
    | Exp '/' Exp            { Filter $1 $3 } 
	| output Exp Exp         { Output $2 $3 }
	| read Exp Exp           { Read $2 $3 }
	| '$' Exp                { Exist $2 }
    | '(' Exp ')'            { $2 } 
	| Exp '==' Exp           { Equal $1 $3 }
	| Exp '!=' Exp           { NotEqual $1 $3 }
	| Exp '>' Exp            { GRT $1 $3 }
	| Exp '>=' Exp           { GET $1 $3 }
	| Exp '<' Exp            { LST $1 $3 }
	| Exp '<=' Exp           { LET $1 $3 }
	| pos Exp                { Position $2 }
	| val Exp                { Value $2 }
    | format                 { Format $1 }
    | var                    { Var $1 }
    
{ 
formatPos :: AlexPosn -> String
formatPos (AlexPn x y z) = "Syntax error at line: " ++ show y ++ ", column: " ++ show z

parseError :: [Token] -> a
parseError xs = error (formatPos (token_posn (head xs)))

data Exp = Output Exp Exp 
         | Conjunction Exp Exp
         | Filter Exp Exp 
		 | Equal Exp Exp
		 | NotEqual Exp Exp
		 | GRT Exp Exp
		 | GET Exp Exp
		 | LST Exp Exp
		 | LET Exp Exp
         | Read Exp Exp
		 | Exist Exp
		 | Format String
         | Var String 
		 | Position Exp
		 | Value Exp
         deriving Show 
} 