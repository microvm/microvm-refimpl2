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
    |   funcExpDef
    ;

typeDef
    :   '.typedef' nam=globalName '=' ctor=typeConstructor
    ;

funcSigDef
    :   '.funcsig' nam=globalName '=' ctor=funcSigConstructor
    ;

constDef
    :   '.const' nam=globalName '<' ty=type '>' '=' ctor=constConstructor
    ;
    
globalDef
    :   '.global' nam=globalName '<' ty=type '>'
    ;

funcDecl
    :   '.funcdecl' nam=globalName '<' sig=funcSig '>'
    ;
    
funcDef
    :   '.funcdef' nam=globalName 'VERSION' ver=name '<' sig=funcSig '>' body=funcBody
    ;
    
funcExpDef
    :   '.expose' nam=globalName '=' func callConv=flag cookie=constant
    ;

typeConstructor
    :   'int'    	'<' length=intLiteral '>'                	# TypeInt
    |   'float'                                                 # TypeFloat
    |   'double'                                                # TypeDouble
    |   'ref'	 	'<' ty=type '>'                             # TypeRef
    |   'iref' 		'<' ty=type '>'                             # TypeIRef
    |   'weakref' 	'<' ty=type '>'                             # TypeWeakRef
    |   'struct' 	'<' fieldTys+=type+ '>'                     # TypeStruct
    |   'array' 	'<' ty=type length=intLiteral '>'           # TypeArray
    |   'hybrid' 	'<' fieldTys+=type* varTy=type '>'          # TypeHybrid
    |   'void'                                                  # TypeVoid
    |   'funcref' 	'<' funcSig '>'                             # TypeFuncRef
    |   'threadref'                                             # TypeThreadRef
    |   'stackref'                                              # TypeStackRef
    |   'tagref64'                                              # TypeTagRef64
    |   'vector' 	'<' ty=type length=intLiteral '>'           # TypeVector
    |   'uptr' 		'<' ty=type '>'                             # TypeUPtr
    |   'ufuncptr' 	'<' funcSig '>'                             # TypeUFuncPtr
    ;

funcSigConstructor
    :   '(' paramTys+=type* ')' '->' '(' retTys+=type* ')'
    ;

constConstructor
    :   intLiteral                  # CtorInt
    |   floatLiteral                # CtorFloat
    |   doubleLiteral               # CtorDouble
    |   '{' globalVar* '}'          # CtorList
    |   'NULL'                      # CtorNull
    ;

type
    :   globalName
    ;

funcSig
    :   globalName
    ;

constant
    :   globalName
    ;

globalCell
    :   globalName
    ;

func
    :   globalName
    ;

funcVer
    :   globalName
    ;

expFunc
    :   globalName
    ;
    
globalVar
    :   globalName
    ;

funcBody
    :   '{' basicBlock+ '}'
    ;

basicBlock
    :   label inst+
    ;

label
    :   name '(' bbParam* ')' excParam? ':'
    ;
	
bbParam
	: 	'<' ty=type '>' name
	;
	
excParam
	: 	'[' name ']'
	;
	
instResults
    :   results+=name
    |   '(' results+=name* ')'
    ;

inst
    :   (instResults '=')? ('[' name ']')? instBody
    ;

instBody
    // Integer/FP Arithmetic
    :   binop '<' ty=type '>' op1=value op2=value excClause                # InstBinOp

    // Integer/FP Comparison
    |   cmpop '<' ty=type '>' op1=value op2=value                          # InstCmp

    // Conversions
    |   convop  '<' fromTy=type toTy=type '>' opnd=value                # InstConversion
    
    // Select
    |   'SELECT' '<' condTy=type resTy=type '>' cond=value ifTrue=value ifFalse=value       # InstSelect

    // Intra-function Control Flow
    |   'BRANCH' dest=destClause                                        # InstBranch
    |   'BRANCH2' cond=value ifTrue=destClause ifFalse=destClause       # InstBranch2
    |   'SWITCH' '<' ty=type '>' opnd=value defDest=destClause '{'
            (caseVal+=value caseDest+=destClause )* '}'                 # InstSwitch

    // Inter-function Control Flow
    |   'CALL' funcCallBody excClause keepAliveClause                   # InstCall
    |   'TAILCALL' funcCallBody                     # InstTailCall

    |   'RET' retVals                               # InstRet
    |   'THROW' exc=value                           # InstThrow

    // Aggregate Operations
    |   'EXTRACTVALUE' 		'<' ty=type intLiteral '>' 		opnd=value               				# InstExtractValue
    |   'INSERTVALUE' 		'<' ty=type intLiteral '>' 		opnd=value newVal=value   				# InstInsertValue
    |   'EXTRACTELEMENT' 	'<' seqTy=type indTy=type '>' 	opnd=value index=value                  # InstExtractElement
    |   'INSERTELEMENT' 	'<' seqTy=type indTy=type '>' 	opnd=value index=value newVal=value     # InstInsertElement
    |   'SHUFFLEVECTOR' 	'<' vecTy=type maskTy=type '>' 	vec1=value vec2=value  mask=value       # InstShuffleVector

    // Memory Operations
    |   'NEW'           '<' allocTy=type '>' excClause                              # InstNew
    |   'NEWHYBRID'     '<' allocTy=type lenTy=type '>' length=value excClause      # InstNewHybrid
    |   'ALLOCA'        '<' allocTy=type '>' excClause                              # InstAlloca
    |   'ALLOCAHYBRID'  '<' allocTy=type lenTy=type '>' length=value excClause      # InstAllocaHybrid
    
    |   'GETIREF'       '<' refTy=type '>' opnd=value                               # InstGetIRef

    |   'GETFIELDIREF'      (ptr='PTR'?) '<' refTy=type index=intLiteral '>' opnd=value          # InstGetFieldIRef
    |   'GETELEMIREF'       (ptr='PTR'?) '<' refTy=type indTy=type '>' opnd=value index=value    # InstGetElemIRef
    |   'SHIFTIREF'         (ptr='PTR'?) '<' refTy=type offTy=type '>' opnd=value offset=value   # InstShiftIRef
    |   'GETVARPARTIREF'    (ptr='PTR'?) '<' refTy=type '>' opnd=value                           # InstGetVarPartIRef
    
    |   'LOAD'      (ptr='PTR'?) memord? '<' type '>' loc=value excClause                        # InstLoad
    |   'STORE'     (ptr='PTR'?) memord? '<' type '>' loc=value newVal=value excClause           # InstStore
    |   'CMPXCHG'   (ptr='PTR'?) (isWeak='WEAK'?) ordSucc=memord ordFail=memord
                    '<' type '>' loc=value expected=value desired=value excClause                # InstCmpXchg
    |   'ATOMICRMW' (ptr='PTR'?) memord atomicrmwop '<' type '>' loc=value opnd=value excClause  # InstAtomicRMW

    |   'FENCE' memord                                                              # InstFence

    // Trap
    |   'TRAP' typeList excClause keepAliveClause                               # InstTrap
    |   'WATCHPOINT' wpid=intLiteral typeList
            dis=destClause ena=destClause ('WPEXC' '(' wpExc=destClause ')')? keepAliveClause    # InstWatchPoint
    |   'WPBRANCH' wpid=intLiteral dis=destClause ena=destClause                # InstWPBranch

    // Foreign Function Interface
    |   'CCALL' callConv=flag '<' funcTy=type funcSig '>' callee=value argList excClause keepAliveClause   # InstCCall

    // Thread and Stack Operations
    |   'NEWTHREAD' stack=value newStackClause excClause                                     # InstNewThread
    |   'SWAPSTACK' swappee=value curStackClause newStackClause excClause keepAliveClause   # InstSwapStack

    // Common Instructions
    |   'COMMINST' nam=globalName flagList? typeList? funcSigList? argList? excClause keepAliveClause     # InstCommInst
    ;
    
retVals
    :   '(' vals+=value* ')'
    |   vals+=value
    ;

destClause
    :   bb argList
    ;
    
bb
	:	name
	;

value
    :   name
    ;

funcCallBody
    :   '<' funcSig '>' callee=value argList
    ;

excClause
    :   ('EXC' '(' nor=destClause exc=destClause ')')?
    ;

keepAliveClause
    :   ('KEEPALIVE' '(' value* ')')?
    ;

flagList
    :   '[' flag* ']'
    ;

typeList
    :   '<' type* '>'
    ;

funcSigList
    :   '<[' funcSig* ']>'
    ;

argList
    :   '(' value* ')'
    ;

curStackClause
    :   'RET_WITH' typeList             # CurStackRetWith
    |   'KILL_OLD'                      # CurStackKillOld
    ;

newStackClause
    :   'PASS_VALUES' typeList argList      # NewStackPassValue
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

flag
    :   FLAG
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
    :   globalName
    |   localName
    ;

globalName : GLOBAL_NAME;
localName : LOCAL_NAME;

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
    
FLAG
    :   FLAG_PREFIX [A-Z_]+
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
FLAG_PREFIX: '#';

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
