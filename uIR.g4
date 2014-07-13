/*
 * defines uVM IR text form
 */

grammar uIR;

ir
    :   metaData*
    ;

metaData
    :   typeDef
    |   funcSigDef
    |   constDef
    |   globalDef
    |   funcDecl
    |   funcDef
    ;

typeDef
    :   '.typedef' GLOBAL_ID '=' typeConstructor
    ;

funcSigDef
    :   '.funcsig' GLOBAL_ID '=' funcSigConstructor
    ;

constDef
    :   '.const' GLOBAL_ID '<' type '>' '=' constExpr
    ;
    
globalDef
    :   '.global' GLOBAL_ID '<' type '>'
    ;

funcDecl
    :   '.funcdecl' GLOBAL_ID '<' funcSig '>'
    ;
    
funcDef
    :   '.funcdef' GLOBAL_ID '<' funcSig '>' paramList funcBody
    ;

type
    :   GLOBAL_ID           # ReferencedType
    |   typeConstructor     # InLineType
    ;

typeConstructor
    :   'int' '<' intLiteral '>'            # IntType
    |   'float'                             # FloatType
    |   'double'                            # DoubleType
    |   'ref' '<' type '>'                  # RefType
    |   'iref' '<' type '>'                 # IRefType
    |   'weakref' '<' type '>'              # WeakRefType
    |   'struct' '<' type* '>'              # StructType
    |   'array' '<' type intLiteral '>'     # ArrayType
    |   'hybrid' '<' type type '>'          # HybridType
    |   'void'                              # VoidType
    |   'func' '<' funcSig '>'              # FuncType
    |   'thread'                            # ThreadType
    |   'stack'                             # StackType
    |   'tagref64'                          # TagRef64Type
    ;

funcSig
    :   GLOBAL_ID           # ReferencedFuncSig
    |   funcSigConstructor  # InLineFuncSig
    ;

funcSigConstructor
    :   type '(' type* ')'
    ;

constant
    :   GLOBAL_ID           # ReferencedConst
    |   constExpr           # InLineConst
    ;

constExpr
    :   intLiteral          # IntConst
    |   floatLiteral        # FloatConst
    |   doubleLiteral       # DoubleConst
    |   '{' constant* '}'   # StructConst
    |   'NULL'              # NullConst
    ;

paramList
    :   '(' LOCAL_ID* ')'
    ;

funcBody
    :   '{' basicBlocks '}'
    ;

basicBlocks
    :   entryBlock regularBlock*
    ;

entryBlock
    :   label? inst+
    ;

regularBlock
    :   label inst+
    ;

label
    :   LOCAL_ID ':'
    ;

inst
    :   (LOCAL_ID '=')? instBody
    ;

instBody
    // Integer/FP Arithmetic
    :   binops '<' type '>' value value         # InstBinOp

    // Integer/FP Comparison
    |   cmpops '<' type '>' value value         # InstCmp

    // Conversions
    |   convops  '<' type type '>' value            # InstConversion
    
    // Select
    |   'SELECT' '<' type '>' value value value     # InstSelect

    // Intra-function Control Flow
    |   'BRANCH' LOCAL_ID                           # InstBranch
    |   'BRANCH2' value LOCAL_ID LOCAL_ID           # InstBranch2
    |   'SWITCH' '<' type '>' value LOCAL_ID '{'
            (value ':' LOCAL_ID ';')* '}'           # InstSwitch
    |   'PHI' '<' type '>' '{'
            (LOCAL_ID ':' value ';')* '}'           # InstPhi

    // Inter-function Control Flow
    |   'CALL' funcCallBody keepAlive?              # InstCall
    |   'INVOKE' funcCallBody LOCAL_ID LOCAL_ID keepAlive? # InstInvoke
    |   'TAILCALL' funcCallBody                     # InstTailCall

    |   'RET' '<' type '>' value                    # InstRet
    |   'RETVOID'                                   # InstRetVoid
    |   'THROW' value                               # InstThrow
    |   'LANDINGPAD'                                # InstLandingPad

    // Aggregate Operations
    |   'EXTRACTVALUE' '<' type intLiteral '>' value        # InstExtractValue
    |   'INSERTVALUE' '<' type intLiteral '>' value value   # InstInsertValue

    // Memory Operations
    |   'NEW'           '<' type '>'                # InstNew
    |   'NEWHYBRID'     '<' type '>' value          # InstNewHybrid
    |   'ALLOCA'        '<' type '>'                # InstAlloca
    |   'ALLOCAHYBRID'  '<' type '>' value          # InstAllocaHybrid
    
    |   'GETIREF'       '<' type '>' value              # InstGetIRef

    |   'GETFIELDIREF'  '<' type intLiteral '>' value   # InstGetFieldIRef
    |   'GETELEMIREF'   '<' type '>' value value        # InstGetElemIRef
    |   'SHIFTIREF'     '<' type '>' value value        # InstShiftIRef
    |   'GETFIXEDPARTIREF'  '<' type '>' value          # InstGetFixedPartIRef
    |   'GETVARPARTIREF'    '<' type '>' value          # InstGetVarPartIRef
    
    |   'LOAD' atomicord? '<' type '>' value            # InstLoad
    |   'STORE' atomicord? '<' type '>' value value     # InstStore
    |   'CMPXCHG' atomicord atomicord
                    '<' type '>' value value value      # InstCmpXchg
    |   'ATOMICRMW' atomicord atomicrmwop
                '<' type '>' value value                # InstAtomicRMW

    |   'FENCE' atomicord                               # InstFence

    // Trap
    |   'TRAP' '<' type '>'
            LOCAL_ID LOCAL_ID keepAlive                 # InstTrap
    |   'WATCHPOINT' intLiteral '<' type '>'
            LOCAL_ID LOCAL_ID LOCAL_ID keepAlive        # InstWatchPoint

    // Foreign Function Interface
    |   'CCALL' callconv funcCallBody                   # InstCCall

    // Thread and Stack Operations
    |   'NEWSTACK' funcCallBody                         # InstNewStack

    // Intrinsic Functions
    |   'ICALL' GLOBAL_ID args keepAlive?               # InstICall
    |   'IINVOKE' GLOBAL_ID args
            LOCAL_ID LOCAL_ID keepAlive?                # InstIInvoke
    ;

funcCallBody
    :   '<' funcSig '>' value args
    ;

args
    :   '(' value* ')'
    ;

keepAlive
    :   'KEEPALIVE' '(' value* ')'
    ;

callconv : 'DEFAULT' ;

binops : ibinops | fbinops ;

ibinops
    : 'ADD'
    | 'SUB'
    | 'MUL'
    | 'UDIV'
    | 'SDIV'
    | 'UREM'
    | 'SREM'
    | 'SHL'
    | 'LSHR'
    | 'ASHR'
    | 'AND'
    | 'OR'
    | 'XOR'
    ;
    
fbinops
    : 'FADD' | 'FSUB' | 'FMUL' | 'FDIV' | 'FREM'
    ;

cmpops : icmpops | fcmpops ;

icmpops
    : 'EQ'
    | 'NE'
    | 'SGT'
    | 'SLT'
    | 'SGE'
    | 'SLE'
    | 'UGT'
    | 'ULT'
    | 'UGE'
    | 'ULE'
    ;

fcmpops
    : 'FTRUE' | 'FFALSE' 
    | 'FUNO' | 'FUEQ' | 'FUNE' | 'FUGT' | 'FULT' | 'FUGE' | 'FULE'
    | 'FORD' | 'FOEQ' | 'FONE' | 'FOGT' | 'FOLT' | 'FOGE' | 'FOLE'
    ;
    
convops
    : 'TRUNC' | 'ZEXT' | 'SEXT' | 'FPTRUNC' | 'FPEXT'
    | 'FPTOUI' | 'FPTOSI' | 'UITOFP' | 'SITOFP' | 'BITCAST'
    | 'REFCAST' | 'IREFCAST' | 'FUNCCAST'
    ;

atomicord
    : 'NOT_ATOMIC' | 'UNORDERED' | 'MONOTONIC' | 'ACQUIRE' | 'RELEASE'
    | 'ACQ_REL' | 'SEQ_CST'
    ;

atomicrmwop
    : 'XCHG' | 'ADD' | 'SUB' | 'AND' | 'NAND' | 'OR' | 'XOR'
    | 'MAX' | 'MIN' | 'UMAX' | 'UMIN'
    ;
value
    :   identifier      # ReferencedValue
    |   constExpr       # InlineConstValue
    ;

intLiteral
    :   INT_DEC     # DecIntLiteral
    |   INT_OCT     # OctIntLiteral
    |   INT_HEX     # HexIntLiteral
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

identifier
    :   GLOBAL_ID
    |   LOCAL_ID
    ;

// LEXER

INT_DEC
    :   ('+'|'-')? DIGIT_NON_ZERO DIGIT*
    ;
    
INT_OCT
    :   ('+'|'-')? '0' DIGIT*
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

GLOBAL_ID
    :   GLOBAL_ID_PREFIX IDCHAR+
    ;

LOCAL_ID
    :   LOCAL_ID_PREFIX IDCHAR+
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
HEX_DIGIT
    :   [0-9a-fA-F]
    ;

fragment
GLOBAL_ID_PREFIX: '@';

fragment
LOCAL_ID_PREFIX: '%';

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
