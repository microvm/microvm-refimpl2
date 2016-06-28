"""
Read muapi.h and write common instruction definitions in reStructuredText.

Usage: python3 muapitoirbuildercomminstsimpl.py
"""

import muapiparser
import sys, os
from collections import namedtuple

from refimpl2injectablefiles import injectable_files, muapi_h_path

start_id_comminst = 0x300   # this is in the spec
#start_id_constant = 0x400

CommInstDesc = namedtuple("CommInstDesc", """
        funcname
        paramnames
        cparamtys
        cretty
        muid
        muname
        muparamtys
        muretty
        pragmas
        arrays
        sizes
        optionals
        """.split())

_type_map = {
        "void"            : "",
        "MuID"            : "int<32>",
        "MuName"          : "iref<int<8>>",
        "MuBool"          : "int<32>",
        "MuWPID"          : "int<32>",
        "MuArraySize"     : "int<64>",
        "MuBinOptr"       : "int<32>",
        "MuCmpOptr"       : "int<32>",
        "MuConvOptr"      : "int<32>",
        "MuDestKind"      : "int<32>",
        "MuMemOrd"        : "int<32>",
        "MuAtomicRMWOptr" : "int<32>",
        "MuCallConv"      : "int<32>",
        "MuCommInst"      : "int<32>",
        "MuFlag"          : "int<32>",
        "int"             : "int<32>",
        "long"            : "int<64>",
        "int8_t"          : "int<8>",
        "uint8_t"         : "int<8>",
        "int16_t"         : "int<16>",
        "uint16_t"        : "int<16>",
        "int32_t"         : "int<32>",
        "uint32_t"        : "int<32>",
        "int64_t"         : "int<64>",
        "uint64_t"        : "int<64>",
        "intptr_t"        : "int<64>",
        "uintptr_t"       : "int<64>",
        "float"           : "float",
        "double"          : "double",
        }

def to_mu_ty(cty):
    if cty.endswith("*"):
        return "iref<{}>".format(to_mu_ty(cty[:-1]))
    elif cty.startswith("Mu") and cty.endswith("Node"):
        return "irnoderef"
    elif cty in _type_map:
        return _type_map[cty]
    else:
        raise Exception("I don't know how to translate: {}".format(cty))

def get_comminsts(ast):
    muctx_methods = [s["methods"] for s in ast["structs"]
            if s["name"] == "MuCtx"][0]

    index_new_bundle = 0
    while muctx_methods[index_new_bundle]["name"] != "new_bundle":
        index_new_bundle += 1

    next_id = start_id_comminst

    comminsts = []

    for i in range(index_new_bundle, len(muctx_methods)):
        meth = muctx_methods[i]
        name = meth["name"]
        cparams = meth["params"]
        cretty = meth["ret_ty"]
        pragmas = meth["pragmas"]
        arrays = {}
        sizes = set()
        optionals = set()

        paramnames = []
        cparamtys = []
        muparamtys = []

        for param in cparams[1:]: # skip MuCtx*
            pn = param["name"]
            pt = param["type"]
            mpt = to_mu_ty(pt)

            paramnames.append(pn)
            cparamtys.append(pt)
            muparamtys.append(mpt)

            mypragmas = [pw for pw in (p.split(":") for p in pragmas) if pw[0] == pn]
            for pragma in mypragmas:
                if pragma[1] == "array":
                    sz = pragma[2]
                    arrays[pn] = sz
                    sizes.add(sz)
                elif pragma[1] == "optional":
                    optionals.add(pn)

        muretty = to_mu_ty(cretty)

        comminst = CommInstDesc(
            funcname   = name,
            paramnames = paramnames,
            cparamtys  = cparamtys,
            cretty     = cretty,
            pragmas    = pragmas,
            muid       = next_id,
            muname     = "@uvm.irbuilder." + name,
            muparamtys = muparamtys,
            muretty    = muretty,
            arrays     = arrays,
            sizes      = sizes,
            optionals  = optionals,
            )
        comminsts.append(comminst)

        next_id += 1

    return comminsts

def gen_comminsts_defs(comminsts):
    lines = []
    for comminst in comminsts:
        lines.append('  commInst(0x{:x}, "{}")'.format(
            comminst.muid, comminst.muname))

    return "\n".join(lines)

_mu_ty_to_internal = {
        "": "", # no return values
        #"int<1>": "I1",
        #"int<8>": "I8",
        #"int<16>": "I16",
        "int<32>": "I32",
        #"int<64>": "I64",
        #"float": "FLOAT",
        #"double": "DOUBLE",
        "irnoderef": "IRNODEREF",
        }

def mu_ty_to_internal(mty):
    if mty in _mu_ty_to_internal:
        return _mu_ty_to_internal[mty]

    raise Exception("I don't know how to convert to internal: {}".format(mty))

def gen_comminsts_retvals(comminsts):
    lines = []

    for comminst in comminsts:
        lines.append('      case "{}" => Seq({})'.format(
            comminst.muname, mu_ty_to_internal(comminst.muretty)))

    return "\n".join(lines)

_get_arg_meths = {
        "int<64>": "asInt64.toLong",
        "int<32>": "asInt32.toInt",
        "float": "asFloat",
        "double": "asDouble",
        }

def get_arg_meth(mty, cty, cname):
    if mty in _get_arg_meths:
        return _get_arg_meths[mty]
    elif mty.startswith("iref<"):
        return "asIRef"

    raise Exception("I don't know how to get arg: {}, {}, {}".format(mty, cty, cname))

def get_arg(ind, mty, cty, cname, is_optional):
    if cty == "MuBundleNode":
        meth = 'asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %{} must not be null")).asInstanceOf[BundleNode]'.format(cname)
    elif mty == "irnoderef":
        if is_optional:
            meth = 'asIRNode'
        else:
            meth = 'asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %{} must not be null"))'.format(cname)
    else:
        meth = get_arg_meth(mty, cty, cname)
    return "        val {} = argList({}).{}".format(cname, ind, meth)

_set_arg_meths = {
        "int<32>": "asInt32",
        }

def set_arg_meth(mty, cty):
    if mty in _set_arg_meths:
        return _set_arg_meths[mty]
    elif mty.startswith("iref<"):
        return "asIRef"

    raise Exception("I don't know how to set return value: {}, {}".format(mty, cty))

def set_ret(ind, mty, cty, value):
    if mty == "irnoderef":
        meth = 'asIRNode'
        return "        results({}).{} = Some({})".format(ind, meth, value)
    else:
        meth = set_arg_meth(mty, cty)
        return "        results({}).{} = {}".format(ind, meth, value)

_special_cases = {
        "id":             "ID",
        "sint8":          "SInt8",
        "uint8":          "UInt8",
        "sint16":         "SInt16",
        "uint16":         "UInt16",
        "sint32":         "SInt32",
        "uint32":         "UInt32",
        "sint64":         "SInt64",
        "uint64":         "UInt64",
        "uint64s":        "UInt64s",
        "fp":             "FP",
        "uptr":           "UPtr",
        "ufuncptr":       "UFuncPtr",
        "iref":           "IRef",
        "weakref":        "WeakRef",
        "funcref":        "FuncRef",
        "tagref64":       "TagRef64",
        "threadref":      "ThreadRef",
        "stackref":       "StackRef",
        "framecursorref": "FrameCursorRef",
        "irnoderef":      "IRNodeRef",
        "funcsig":        "FuncSig",
        "bb":             "BB",
        "binop":          "BinOp",
        "tailcall":       "TailCall",
        "extractvalue":   "ExtractValue",
        "insertvalue":    "InsertValue",
        "extractelement": "ExtractElement",
        "insertelement":  "InsertElement",
        "shufflevector":  "ShuffleVector",
        "newhybrid":      "NewHybrid",
        "allocahybrid":   "AllocaHybrid",
        "getiref":        "GetIRef",
        "getfieldiref":   "GetFieldIRef",
        "getelemiref":    "GetElemIRef",
        "shiftiref":      "ShiftIRef",
        "getvarpartiref": "GetVarPartIRef",
        "cmpxchg":        "CmpXchg",
        "atomicrmw":      "AtomicRMW",
        "watchpoint":     "WatchPoint",
        "wpbranch":       "WPBranch",
        "ccall":          "CCall",
        "newthread":      "NewThread",
        "newstack":       "NewStack",
        "swapstack":      "SwapStack",
        "comminst":       "CommInst",
        }

def toCamelCase(name):
    ins = name.split("_")
    outs = [ins[0]]
    for inn in ins[1:]:
        if inn in _special_cases:
            outs.append(_special_cases[inn])
        else:
            outs.append(inn[0].upper()+inn[1:])

    return "".join(outs)

def gen_comminst_impl(comminst):
    lines = []

    lines.append('      case "{}" => {{'.format(comminst.muname))

    for ind, (cty, mty, cname) in enumerate(zip(
            comminst.cparamtys, comminst.muparamtys, comminst.paramnames)):
        lines.append(get_arg(ind, mty, cty, cname,
            is_optional=(cname in comminst.optionals)))

    ir_builder_args = []

    for cty, mty, cname in zip(
            comminst.cparamtys, comminst.muparamtys, comminst.paramnames):
        if cname in comminst.arrays:
            sz = comminst.arrays[cname]
            loaded_array_name = "_ary_" + cname
            if mty == "iref<irnoderef>":
                loader_func = "loadIRNodeArray"
            elif mty == "iref<int<64>>":
                loader_func = "loadInt64Array"
            elif cty == "MuFlag*":
                loader_func = "loadFlagArray"
            else:
                raise Exception("I don't know how to load array: {}, {}, {}".format(
                    cty, mty, cname))
            lines.append('        val {} = {}({}, {})'.format(
                loaded_array_name, loader_func, cname, sz))
            ir_builder_args.append(loaded_array_name)
        elif cname in comminst.sizes:
            pass    # skip array sizes
        elif cty == "MuBool":
            bool_name = "_bool_" + cname
            lines.append('        val {} = {} != 0'.format(bool_name, cname))
            ir_builder_args.append(bool_name)
        else:
            ir_builder_args.append(cname)

    ir_builder_meth_name = toCamelCase(comminst.funcname)
    lines.append('        val _rv = irBuilder.{}({})'.format(
        ir_builder_meth_name, ", ".join(ir_builder_args)))

    if comminst.cretty != "void":
        lines.append(set_ret(0, comminst.muretty, comminst.cretty, "_rv"))
    
    lines.append("        continueNormally()")
    lines.append("      }")

    return "\n".join(lines)

# These functions are too speical. Implemented manually in Scala.
_blacklist = [
        "load_bundle_from_node",
        "abort_bundle_node",
        "set_name",
        "new_const_int_ex",
        ]

def gen_comminsts_impls(comminsts):
    lines = []

    for comminst in comminsts:
        if comminst.funcname not in _blacklist:
            lines.append(gen_comminst_impl(comminst))

    return "\n".join(lines)

def main():
    with open(muapi_h_path) as f:
        txt = f.read()

    ast = muapiparser.parse_muapi(txt)

    comminsts = get_comminsts(ast)

    comminsts_defs    = gen_comminsts_defs(comminsts)
    comminsts_retvals = gen_comminsts_retvals(comminsts)
    comminsts_impls   = gen_comminsts_impls(comminsts)

    #print(comminsts_defs)

    injectable_files.inject_many({
        "comminsts.scala": {
            "IRBUILDER_COMMINSTS": comminsts_defs,
            },
        "internals.scala": {
            "IRBUILDER_RETVALS": comminsts_retvals,
            },
        "ir-ci-exec": {
            "IRBUILDER_IMPL": comminsts_impls,
            },
        })

    #print()
    #print_constants(ast)  # Do not print. This merely pollutes the pre-loaded
                           # space with more constants, and not profitable.
                           # We keep muapi.h as the canonical definition.
                           # Clients should define their own constants.

if __name__=='__main__':
    main()
