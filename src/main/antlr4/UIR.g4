grammar UIR;

ir
    :   topLevelDef*
    ;

topLevelDef
    :   typeDef
    |   funcSigDef
    |   constDef
    |   globalDef
    |   funcDecl
    |   funcDef
    |   exposeDef
    ;

typeDef
    :   '.typedef' nam=GLOBAL_NAME '=' ctor=typeConstructor
    ;

funcSigDef
    :   '.funcsig' nam=GLOBAL_NAME '=' ctor=funcSigConstructor
    ;

constDef
    :   '.const' nam=GLOBAL_NAME '<' ty=type '>' '=' ctor=constConstructor
    ;
    
globalDef
    :   '.global' nam=GLOBAL_NAME '<' ty=type '>'
    ;

funcDecl
    :   '.funcdecl' nam=GLOBAL_NAME '<' sig=funcSig '>'
    ;
    
funcDef
    :   '.funcdef' nam=GLOBAL_NAME 'VERSION' ver=GLOBAL_NAME '<' sig=funcSig '>' params=paramList body=funcBody
    ;
    
exposeDef
    :   '.expose' nam=GLOBAL_NAME '=' funcName=GLOBAL_NAME '<' callconv '>' cookie=GLOBAL_NAME
    ;

typeConstructor
    :   'int' '<' length=intLiteral '>'                         # TypeInt
    |   'float'                                                 # TypeFloat
    |   'double'                                                # TypeDouble
    |   'ref' '<' type '>'                                      # TypeRef
    |   'iref' '<' type '>'                                     # TypeIRef
    |   'weakref' '<' type '>'                                  # TypeWeakRef
    |   'struct' '<' type+ '>'                                  # TypeStruct
    |   'array' '<' type length=intLiteral '>'                  # TypeArray
    |   'hybrid' '<' fixedTy=type varTy=type '>'                # TypeHybrid
    |   'void'                                                  # TypeVoid
    |   'func' '<' funcSig '>'                                  # TypeFunc
    |   'thread'                                                # TypeThread
    |   'stack'                                                 # TypeStack
    |   'tagref64'                                              # TypeTagRef64
    |   'vector' '<' type length=intLiteral '>'                 # TypeVector
    ;

funcSigConstructor
    :   retTy=type '(' (paramTy+=type*) ')'
    ;

constConstructor
    :   intLiteral                  # ConstInt
    |   floatLiteral                # ConstFloat
    |   doubleLiteral               # ConstDouble
    |   '{' GLOBAL_NAME* '}'        # ConstStruct
    |   'NULL'                      # ConstNull
    |   'VEC' '{' constant* '}'     # ConstVector
    ;

type
    :   GLOBAL_NAME
    ;

funcSig
    :   GLOBAL_NAME
    ;

constant
    :   GLOBAL_NAME
    ;

paramList
    :   '(' name* ')'
    ;

funcBody
    :   '{' basicBlock* '}'
    ;

basicBlock
    :   label inst+
    ;

label
    :   name ':'
    ;

inst
    :   (name '=')? instBody
    ;

instBody
    // Integer/FP Arithmetic
    :   binop '<' type '>' op1=value op2=value excClause                # InstBinOp

    // Integer/FP Comparison
    |   cmpop '<' type '>' op1=value op2=value                          # InstCmp

    // Conversions
    |   convop  '<' fromTy=type toTy=type '>' opnd=value                # InstConversion
    
    // Select
    |   'SELECT' '<' condTy=type resTy=type '>' cond=value ifTrue=value ifFalse=value       # InstSelect

    // Intra-function Control Flow
    |   'BRANCH' bbName                                                 # InstBranch
    |   'BRANCH2' cond=value ifTrue=bbName ifFalse=bbName               # InstBranch2
    |   'SWITCH' '<' type '>' opnd=value defDest=bbName '{'
            (caseVal+=value ':' caseDest+=bbName ';')* '}'              # InstSwitch
    |   'PHI' '<' type '>' '{'
            (caseSrc+=bbName ':' caseVal+=value ';')* '}'               # InstPhi

    // Inter-function Control Flow
    |   'CALL' funcCallBody excClause keepAliveClause                   # InstCall
    |   'TAILCALL' funcCallBody                     # InstTailCall

    |   'RET' '<' type '>' retVal=value             # InstRet
    |   'RETVOID'                                   # InstRetVoid
    |   'THROW' exc=value                           # InstThrow
    |   'LANDINGPAD'                                # InstLandingPad

    // Aggregate Operations
    |   'EXTRACTVALUE' '<' type intLiteral '>' opnd=value               # InstExtractValue
    |   'INSERTVALUE' '<' type intLiteral '>' opnd=value newVal=value   # InstInsertValue
    |   'EXTRACTELEMENT' '<' vecTy=type indTy=type '>' opnd=value index=value                           # InstExtractElement
    |   'INSERTELEMENT' '<' vecTy=type indTy=type '>' opnd=value index=value newVal=value               # InstInsertElement
    |   'SHUFFLEVECTOR' '<' vecTy=type maskTy=type '>' vec1=value vec2=value mask=value                 # InstShuffleVector

    // Memory Operations
    |   'NEW'           '<' allocTy=type '>' excClause                              # InstNew
    |   'NEWHYBRID'     '<' allocTy=type lenTy=type '>' length=value excClause      # InstNewHybrid
    |   'ALLOCA'        '<' allocTy=type '>' excClause                              # InstAlloca
    |   'ALLOCAHYBRID'  '<' allocTy=type lenTy=type '>' length=value excClause      # InstAllocaHybrid
    
    |   'GETIREF'       '<' refTy=type '>' opnd=value                               # InstGetIRef

    |   'GETFIELDIREF'      (ptr='PTR'?) '<' refTy=type index=intLiteral '>' opnd=value          # InstGetFieldIRef
    |   'GETELEMIREF'       (ptr='PTR'?) '<' refTy=type indTy=type '>' opnd=value index=value    # InstGetElemIRef
    |   'SHIFTIREF'         (ptr='PTR'?) '<' refTy=type offTy=type '>' opnd=value offset=value   # InstShiftIRef
    |   'GETFIXEDPARTIREF'  (ptr='PTR'?) '<' refTy=type '>' opnd=value                           # InstGetFixedPartIRef
    |   'GETVARPARTIREF'    (ptr='PTR'?) '<' refTy=type '>' opnd=value                           # InstGetVarPartIRef
    
    |   'LOAD'      (ptr='PTR'?) memord? '<' type '>' loc=value excClause                        # InstLoad
    |   'STORE'     (ptr='PTR'?) memord? '<' type '>' loc=value newVal=value excClause           # InstStore
    |   'CMPXCHG'   (ptr='PTR'?) (isWeak='WEAK'?) ordSucc=memord ordFail=memord
                    '<' type '>' loc=value expected=value desired=value excClause                # InstCmpXchg
    |   'ATOMICRMW' (ptr='PTR'?) memord atomicrmwop '<' type '>' loc=value opnd=value excClause  # InstAtomicRMW

    |   'FENCE' memord                                                              # InstFence

    // Trap
    |   'TRAP' '<' type '>' excClause keepAliveClause                               # InstTrap
    |   'WATCHPOINT' wpid=intLiteral '<' type '>'
            dis=bbName ena=bbName ('WPEXC' '(' wpExc=bbName ')')? keepAliveClause   # InstWatchPoint

    // Foreign Function Interface
    |   'CCALL' callconv '<' funcTy=type funcSig '>' callee=value argList           # InstCCall

    // Thread and Stack Operations
    |   'NEWSTACK' funcCallBody excClause                                                   # InstNewStack
    |   'SWAPSTACK' swappee=value curStackClause newStackClause excClause keepAliveClause   # InstSwapStack

    // Common Instructions
    |   'COMMINST' nam=GLOBAL_NAME flagList? typeList? argList? excClause keepAliveClause     # InstCommInst
    ;

bbName
    :   name
    ;

value
    :   name
    ;

funcCallBody
    :   '<' funcSig '>' callee=value argList
    ;

excClause
    :   ('EXC' '(' nor=bbName exc=bbName ')')?
    ;

keepAliveClause
    :   ('KEEPALIVE' '(' value* ')')?
    ;

flagList
    :   '<' flag* '>'
    ;

typeList
    :   '<' type* '>'
    ;

argList
    :   '(' value* ')'
    ;

curStackClause
    :   'RET_WITH' '<' type '>'     # CurStackRetWith
    |   'KILL_OLD'                  # CurStackKillOld
    ;

newStackClause
    :   'PASS_VALUE' '<' type '>' value     # NewStackPassValue
    |   'PASS_VOID'                         # NewStackPassVoid
    |   'THROW_EXC' exc=value               # NewStackThrowExc
    ;

binop
    : 'ADD' | 'SUB' | 'MUL' | 'UDIV' | 'SDIV' | 'UREM' | 'SREM' | 'SHL' | 'LSHR' | 'ASHR' | 'AND' | 'OR' | 'XOR'
    | 'FADD'| 'FSUB' | 'FMUL' | 'FDIV' | 'FREM'
    ;

cmpop
    : 'EQ' | 'NE' | 'SGT' | 'SLT' | 'SGE' | 'SLE' | 'UGT' | 'ULT' | 'UGE' | 'ULE'
    | 'FTRUE' | 'FFALSE'
    | 'FUNO' | 'FUEQ' | 'FUNE' | 'FUGT' | 'FULT' | 'FUGE' | 'FULE'
    | 'FORD' | 'FOEQ' | 'FONE' | 'FOGT' | 'FOLT' | 'FOGE' | 'FOLE'
    ;
    
convop
    : 'TRUNC' | 'ZEXT' | 'SEXT' | 'FPTRUNC' | 'FPEXT'
    | 'FPTOUI' | 'FPTOSI' | 'UITOFP' | 'SITOFP'
    | 'BITCAST' | 'REFCAST' | 'PTRCAST'
    ;

memord
    : 'NOT_ATOMIC' | 'RELAXED' | 'CONSUME' | 'ACQUIRE' | 'RELEASE' | 'ACQ_REL' | 'SEQ_CST'
    ;

atomicrmwop
    : 'XCHG' | 'ADD' | 'SUB' | 'AND' | 'NAND' | 'OR' | 'XOR' | 'MAX' | 'MIN' | 'UMAX' | 'UMIN'
    ;

callconv
    :   'DEFAULT'
    ;

flag
    :   callconv
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

name
    :   GLOBAL_NAME
    |   LOCAL_NAME
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

LOCAL_NAME
    :   LOCAL_NAME_PREFIX IDCHAR+
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
LOCAL_NAME_PREFIX: '%';

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
