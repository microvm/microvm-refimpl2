"""
Converts MuCtx methods in muapi.h to nativeClientSupport

USAGE: python3 muapitoncs.py < cbinding/muapi.h | xclip -selection c

then paste in function signatures, then paste the result in
src/main/scala/uvm/refimpl/nat/nativeClientSupport.scala

Use pbcopy on Mac.
"""

import sys
import re

r_comment = re.compile(r'//.*$', re.MULTILINE)
r_decl = re.compile(r'(?P<ret>\w+)\s*\(\s*\*\s*(?P<name>\w+)\s*\)\s*\((?P<params>[^)]*)\)\s*;')
r_param = re.compile(r'\s*(?P<type>\w+)\s*(?P<ptr>\*?)\s*(?P<name>\w+)')
r_value_ty = re.compile(r'Mu\w*(Value|Node)')

begin = "/// IR Builder API"
end = "// Common instruction opcodes"

target_begin = '/// SCRIPT: INSERT BEGIN'
target_begin = '/// SCRIPT: INSERT END'

lines = sys.stdin.read().splitlines()
l1 = [n for (n,l) in enumerate(lines) if begin in l][0]
l2 = [n for (n,l) in enumerate(lines) if end in l][0]

text = "\n".join(lines[l1+1:l2])

text = r_comment.sub("", text)

_simple_map = {
        "void": "Unit",
        "int": "Int",
        "long": "Long",
        "uint64_t": "Long",
        "uint64_t*": "LongPtr",
        "float": "Float",
        "double": "Double",
        }

def conv_ret_ty(ty):
    if ty in _simple_map:
        ty = _simple_map[ty]
    m = r_value_ty.match(ty)
    if m is not None:
        return (True, "MuValueFak")
    else:
        return (False, ty)

_special_case = {
        "id": "ID",
        "uptr": "UPtr",
        "ufuncptr": "UFuncPtr",
        "iref": "IRef",
        "weakref": "WeakRef",
        "funcref": "FuncRef",
        "tagref64": "TagRef64",
        "threadref": "ThreadRef",
        "stackref": "StackRef",
        "framecursorref": "FrameCursorRef",
        "irnoderef": "IRNodeRef",
        "funcsig": "FuncSig",
        "bb": "BB",
        "keepalives": "KeepAlives",
        "binop": "BinOp",
        "tailcall": "TailCall",
        "extractvalue": "ExtractValue",
        "insertvalue"   : "InsertValue",
        "extractelement": "ExtractElement",
        "insertelement" : "InsertElement",
        "shufflevector" : "ShuffleVector",
        "newhybrid"     : "NewHybrid",
        "allocahybrid"  : "AllocaHybrid",
        "getiref"       : "GetIRef",
        "getfieldiref"  : "GetFieldIRef",
        "getelemiref"   : "GetElemIRef",
        "shiftiref"     : "ShiftIRef",
        "getvarpartiref": "GetVarPartIRef",
        "cmpxchg"       : "CmpXchg",
        "atomicrmw"     : "AtomicRMW",
        "watchpoint"    : "WatchPoint",
        "wpbranch"    : "WPBranch",
        "ccall"         : "CCall",
        "newthread"     : "NewThread",
        "newstack"     : "NewStack",
        "swapstack"     : "SwapStack",
        "comminst"      : "CommInst",
        }

def toCamelCase(name):
    ins = name.split("_")
    outs = [ins[0]]
    for inn in ins[1:]:
        if inn in _special_case:
            outs.append(_special_case[inn])
        else:
            outs.append(inn[0].upper()+inn[1:])

    return "".join(outs)

_special_param = {
        }

def conv_param_ty(name, ty):
    if ty == "MuCtx*":
        return "MuCtx"
    elif r_value_ty.match(ty) is not None and ty.endswith("*"):
        return "MuValueFakArrayPtr"
    elif ty == "MuFlag*":
        return "MuFlagArrayPtr"
    elif name == "threadlocal" and ty == "MuVarNode":
        return "Option[MuVarNode]"
    elif ty in _special_param:
        return _special_param[ty]
    elif ty in _simple_map:
        return _simple_map[ty]
    else:
        return ty

def conv_param_val(func, name, ty):
    if ty == "MuValueFakArrayPtr":
        if func == "set_newstack_pass_values":
            lenvar = "nvars"
        else:
            lenvar = "n" + name
        return "readFromValueFakArray({}, {})".format(name, lenvar)
    elif ty == "MuFlagArrayPtr":
        lenvar = "n" + name
        return "readFromFlagArray({}, {})".format(name, lenvar)
    elif name in ["is_ptr", "is_weak"]:
        return name + " != 0"
    else:
        return name

_num_params = "nfieldtys nfixedtys nparamtys nrettys nelems nargs nrvs nvars nflags ntys nsigs nret_tys".split()

def forward_call(func, params):
    params = [p for p in params if p[0] not in _num_params]
    return "ctx.{}({})".format(toCamelCase(func), ", ".join(
        conv_param_val(func, n, t) for n,t in params))

for m in r_decl.finditer(text):
    name, params, ret = [m.groupdict()[k] for k in "name params ret".split()]

    params_out = []
    params = params.split(",")
    for param in params:
        mp = r_param.search(param)
        pt, pp, pn = [mp.groupdict()[k] for k in "type ptr name".split()]
        pt = conv_param_ty(pn, pt+pp)
        params_out.append((pn, pt))

    params_out_str = ", ".join("{}: {}".format(pn, pt) for pn, pt in params_out)

    is_value, ret = conv_ret_ty(ret)

    impl = forward_call(name, params_out[1:])
    if is_value:
        impl = "exposeMuValue(ctx, {})".format(impl)

    print("  def {}({}): {} = {}".format(name, params_out_str, ret, impl))

        




