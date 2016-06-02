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
    (r'CN\[_\s*<:\s*Identified\]', 'MuChildNode'),
    (r'CN\[IdentifiedSettable\]', 'MuChildNode'),
    (r'CN\[Type\w*\]', 'MuTypeNode'),
    (r'CN\[Abstract\w+Type\]', 'MuTypeNode'),
    (r'CN\[FuncSig\]', 'MuFuncSigNode'),
    (r'CN\[Const\w+\]', 'MuConstNode'),
    (r'CN\[GlobalCell\]', 'MuGlobalNode'),
    (r'CN\[Function\]', 'MuFuncNode'),
    (r'CN\[ExposedFunc\]', 'MuExpFuncNode'),
    (r'CN\[FuncVer\]', 'MuFuncVerNode'),
    (r'CN\[BasicBlock\]', 'MuBBNode'),
    (r'CN\[BB\]', 'MuBBNode'),
    (r'CN\[SSAVariable\]', 'MuVarNode'),
    (r'CN\[Var\]', 'MuVarNode'),
    (r'CN\[LocalVariable\]', 'MuLocalVarNode'),
    (r'CN\[NorParam\]', 'MuNorParamNode'),
    (r'CN\[ExcParam\]', 'MuExcParamNode'),
    (r'CN\[InstResult\]', 'MuInstResNode'),
    (r'CN\[Inst\w+\]', 'MuInstNode'),
    (r'CN\[HasKeepAliveClause\]', 'MuInstNode'),
    ]]

sig = re.compile(r'^(  def (\w+)\(([^)]*)\):\s+\w+\s+=)', re.MULTILINE)
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
        

