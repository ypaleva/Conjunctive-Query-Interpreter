{ 
module Tokens where 
}

%wrapper "posn" 
$digit = [0-9]     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters
$format = [\x \*]

tokens :-
$white+       ; 
  "\\".*      ; 
  output                           { tok (\p s -> TokenOutput p) } 
  OUTPUT                           { tok (\p s -> TokenOutput p) } 
  read                             { tok (\p s -> TokenRead p) }
  READ                             { tok (\p s -> TokenRead p) }
  pos                              { tok (\p s -> TokenPos p) }
  POS                              { tok (\p s -> TokenPos p) }
  val                              { tok (\p s -> TokenVal p) }
  VAL                              { tok (\p s -> TokenVal p) }
  \==                              { tok (\p s -> TokenEq p) }
  \!=                              { tok (\p s -> TokenNotEq p) }
  \>=                              { tok (\p s -> TokenGET p) }
  \>                               { tok (\p s -> TokenGT p) }
  \<=                              { tok (\p s -> TokenLET p) }
  \<                               { tok (\p s -> TokenLT p) }
  \^                               { tok (\p s -> TokenConjunction p) }
  \/                               { tok (\p s -> TokenFilter p) }
  \$                               { tok (\p s -> TokenExist p) }
  \(                               { tok (\p s -> TokenLParen p) }
  \)                               { tok (\p s -> TokenRParen p) }
  $format [$format $digit \,]*     { tok (\p s -> TokenFormat p s) }
  $alpha [$alpha $digit \_ \- \.]* { tok (\p s -> TokenVar p s) }
  $digit [$digit $alpha \_ \- \.]* { tok (\p s -> TokenVar p s) }

{ 

tok f p s = f p s

-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
  TokenOutput AlexPosn        |
  TokenRead AlexPosn          |
  TokenConjunction AlexPosn   |
  TokenFilter AlexPosn        |
  TokenExist AlexPosn         |
  TokenVar AlexPosn String    | 
  TokenFormat AlexPosn String |
  TokenLParen AlexPosn        |
  TokenRParen AlexPosn        |
  TokenPos AlexPosn           |
  TokenVal AlexPosn           |
  TokenEq AlexPosn            |
  TokenNotEq AlexPosn         |
  TokenGT AlexPosn            |
  TokenGET AlexPosn           |
  TokenLT AlexPosn            |
  TokenLET AlexPosn             
  deriving (Eq,Show) 
  
token_posn (TokenOutput p) = p  
token_posn (TokenRead p) = p  
token_posn (TokenConjunction p) = p  
token_posn (TokenFilter p) = p  
token_posn (TokenExist p) = p  
token_posn (TokenVar p _ ) = p  
token_posn (TokenFormat p _ ) = p  
token_posn (TokenLParen p) = p  
token_posn (TokenRParen p) = p  
token_posn (TokenPos p) = p  
token_posn (TokenVal p) = p  
token_posn (TokenEq p) = p
token_posn (TokenNotEq p) = p
token_posn (TokenGT p) = p
token_posn (TokenGET p) = p
token_posn (TokenLT p) = p
token_posn (TokenLET p) = p
  
}