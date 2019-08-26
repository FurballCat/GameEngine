#pragma once

/*
 
 Example grammar (might not be related to final NC script grammar)
 
 func CallMe(x):
    return x+1
 
 grammar    : NcGrammar;
 statement  : return_statement
            | function_definition;
 
 return_statement       : 'return' expression;
 function_definition    : 'func' IDENTIFIER '(' arguments ')' ':';
 expression             : IDENTIFIER '+' NUMBER;
 arguments              : argument (',' argument)*;
 argument               : IDENTIFIER;
 IDENTIFIER             : ('a'..'z'|'A'..'Z')+;
 NUMBER                 : '0'..'9'+;
 
 */
