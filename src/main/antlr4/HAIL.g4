grammar HAIL;

hail
    :   topLevelDef*
    ;

topLevelDef
    :   fixedAlloc
    |   hybridAlloc
    |   memInit
    ;

fixedAlloc
    :   '.new' nam=HAIL_NAME '<' ty=type '>'
    ;

hybridAlloc
    :   '.newhybrid' nam=HAIL_NAME '<' ty=type '>' len=intExpr
    ;
    
memInit
    :   '.init' lv=lValue '=' rv=rValue
    ;
    
lValue
    :   nam=name (indices+=index)*
    ;
    
rValue
    :   GLOBAL_NAME     # RVGlobal
    |   intLiteral      # RVInt
    |   floatLiteral    # RVFloat
    |   doubleLiteral   # RVDouble
    |   'NULL'          # RVNull
    |   HAIL_NAME       # RVHailRef
    |   '&' lValue      # RVIRefOf
    |   list            # RVList
    ;
    
list
    :   '{' rv+=rValue* '}'
    ;
    
index
    :   '[' intExpr ']'
    ;
    
intExpr
    :   intLiteral  # IntLit
    |   GLOBAL_NAME # IntGlobal
    ;
    
type
    :   GLOBAL_NAME
    ;

name
    :   GLOBAL_NAME
    |   HAIL_NAME
    ;

intLiteral
    :   INT_DEC
    |   INT_OCT
    |   INT_HEX
    ;

floatLiteral
    :   FP_NUM 'f'  # FloatNumber
    |   INF 'f'     # FloatInf
    |   NAN 'f'     # FloatNan
    |   'bitsf' '(' intLiteral ')'   # FloatBits
    ;

doubleLiteral
    :   FP_NUM 'd'  # DoubleNumber
    |   INF 'd'     # DoubleInf
    |   NAN 'd'     # DoubleNan
    |   'bitsd' '(' intLiteral ')'   # DoubleBits
    ;

// LEXER

INT_DEC
    :   ('+'|'-')? DIGIT_NON_ZERO DIGIT*
    ;
    
INT_OCT
    :   ('+'|'-')? '0' OCT_DIGIT*
    ;

INT_HEX
    :   ('+'|'-')? '0x' HEX_DIGIT+
    ;
    
FP_NUM
    :   ('+'|'-')? DIGIT+ '.' DIGIT+ ('e' ('+'|'-')? DIGIT+)?
    ;

INF
    :   ('+'|'-') 'inf'
    ;
    
NAN
    :   'nan'
    ;

GLOBAL_NAME
    :   GLOBAL_NAME_PREFIX IDCHAR+
    ;

HAIL_NAME
    :   HAIL_NAME_PREFIX IDCHAR+
    ;

fragment
DIGIT
    :   [0-9]
    ;

fragment
DIGIT_NON_ZERO
    :   [1-9]
    ;

fragment
OCT_DIGIT
    :   [0-7]
    ;

fragment
HEX_DIGIT
    :   [0-9a-fA-F]
    ;

fragment
GLOBAL_NAME_PREFIX: '@';

fragment
HAIL_NAME_PREFIX: '$';

fragment
IDCHAR
    :   [a-z]
    |   [A-Z]
    |   [0-9]
    |   '-'
    |   '_'
    |   '.'
    ;

WS : [ \t\r\n]+ -> skip ; // skip spaces, tabs, newlines

LINE_COMMENT
    :   '//' ~[\r\n]* -> skip
    ;
