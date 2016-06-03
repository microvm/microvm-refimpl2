"""
USAGE: python3 migrate_scripts/irbuildertomuctx.py < src/main/scala/uvm/ir/irbuilder/IRBuilder.scala | xclip -selection c

And then paste the result into src/main/scala/uvm/refimpl/MuCtxIRBuilderPart.scala
"""

import re
import sys

begin = "SCRIPT: BEGIN HERE"
end = "SCRIPT: END HERE"

replaces = [(re.compile(x), y) for (x,y) in [
    (r'BN', 'MuBundleNode'),
    (r'(CN|ChildNode)\[(_\s*<:\s*)?Identified\]', 'MuChildNode'),
    (r'(CN|ChildNode)\[(_\s*<:\s*)?IdentifiedSettable\]', 'MuChildNode'),
    (r'(CN|ChildNode)\[Type\w*\]', 'MuTypeNode'),
    (r'(CN|ChildNode)\[Abstract\w+Type\]', 'MuTypeNode'),
    (r'(CN|ChildNode)\[FuncSig\]', 'MuFuncSigNode'),
    (r'(CN|ChildNode)\[Const\w+\]', 'MuConstNode'),
    (r'(CN|ChildNode)\[GlobalCell\]', 'MuGlobalNode'),
    (r'(CN|ChildNode)\[Function\]', 'MuFuncNode'),
    (r'(CN|ChildNode)\[ExposedFunc\]', 'MuExpFuncNode'),
    (r'(CN|ChildNode)\[FuncVer\]', 'MuFuncVerNode'),
    (r'(CN|ChildNode)\[BasicBlock\]', 'MuBBNode'),
    (r'(CN|ChildNode)\[BB\]', 'MuBBNode'),
    (r'(CN|ChildNode)\[SSAVariable\]', 'MuVarNode'),
    (r'(CN|ChildNode)\[Var\]', 'MuVarNode'),
    (r'(CN|ChildNode)\[LocalVariable\]', 'MuLocalVarNode'),
    (r'(CN|ChildNode)\[NorParam\]', 'MuNorParamNode'),
    (r'(CN|ChildNode)\[ExcParam\]', 'MuExcParamNode'),
    (r'(CN|ChildNode)\[InstResult\]', 'MuInstResNode'),
    (r'(CN|ChildNode)\[Inst\w+\]', 'MuInstNode'),
    (r'(CN|ChildNode)\[HasKeepAliveClause\]', 'MuInstNode'),
    ]]

sig = re.compile(r'^(  def\s+(\w+)\s*\(([^)]*)\):\s+\w+\s+=)', re.MULTILINE)
arg = re.compile(r'(\w*):\s+([a-zA-Z0-9\[\]]+)')
node_like = re.compile(r'Mu\w+Node')
node_seq_like = re.compile(r'Seq\[Mu\w+Node\]')

lines = sys.stdin.read().splitlines()
l1 = [n for (n,l) in enumerate(lines) if begin in l][0]
l2 = [n for (n,l) in enumerate(lines) if end in l][0]

text = "\n".join(lines[l1+1:l2])

for p, t in replaces:
    text = p.sub(t, text)

#print(text)

#sys.exit(0)

for whole, name, arglist in sig.findall(text):
    print(whole, "{")
    argnames = []
    for an,at in arg.findall(arglist):
        argnames.append(an)
        #print(an, at)
        if node_seq_like.match(at) is not None:
            print('    for((n,i) <- {}.zipWithIndex) require(!n.isNull, "{}[%d] must not be NULL".format(i))'.format(an, an))
        elif node_like.match(at) is not None:
            print('    require(!{}.isNull, "{} must not be NULL")'.format(an, an))
    if name.startswith("new"):
        print('    addHandle(irBuilder.{}({}))'.format(name, ", ".join(argnames)))
    else:
        print('    irBuilder.{}({})'.format(name, ", ".join(argnames)))
    print("  }")
    print()
    #print(whole, name, args)
    #for n,a in sig.findall(line):
        #args = arg_name.findall(a)
        #print("    addHandle(irBuilder.{}({}))".format(n, ", ".join(args)))
        

