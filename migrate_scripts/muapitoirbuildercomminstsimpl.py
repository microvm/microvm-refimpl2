"""
Read muapi.h and write common instruction definitions in reStructuredText.

Usage: python3 muapitoirbuildercomminstsimpl.py
"""

import muapiparser
import sys, os
from collections import namedtuple

from refimpl2injectablefiles import injectable_files

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
        bools
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
        bools = set()

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
                elif pragma[1] == "bool":
                    bools.add(pn)

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
            bools      = bools,
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

def gen_comminsts_impls(comminsts):
    lines = []

    for comminst in comminsts:
        lines.append('      case "{}" => ???'.format(comminst.muname))

    return "\n".join(lines)

my_dir = os.path.dirname(__file__)
muapi_h_path = os.path.join(my_dir, "../cbinding/muapi.h")
#dst_path = os.path.join(my_dir, "../pythonbinding/libmu.py")

def main():
    with open(muapi_h_path) as f:
        txt = f.read()

    ast = muapiparser.parse_muapi(txt)

    comminsts = get_comminsts(ast)

    comminsts_defs    = gen_comminsts_defs(comminsts)
    comminsts_retvals = gen_comminsts_retvals(comminsts)
    comminsts_impls   = gen_comminsts_impls(comminsts)

    #print(comminsts_defs)

    injectable_files["comminsts.scala"].inject_many({
        "IRBUILDER_COMMINSTS": comminsts_defs,
        })
    injectable_files["internals.scala"].inject_many({
        "IRBUILDER_RETVALS": comminsts_retvals,
        })
    injectable_files["ir-ci-exec"].inject_many({
        "IRBUILDER_IMPL": comminsts_impls,
        })

    #print()
    #print_constants(ast)  # Do not print. This merely pollutes the pre-loaded
                           # space with more constants, and not profitable.
                           # We keep muapi.h as the canonical definition.
                           # Clients should define their own constants.

if __name__=='__main__':
    main()
