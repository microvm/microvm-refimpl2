"""
Inject things in muapi.h to ../pythonbinding/libmu.py

USAGE: python3 muapitolibmupy.py
"""

import sys
import os, os.path
import re
import tempfile
from typing import Tuple

import muapiparser
from refimpl2injectablefiles import injectable_files, muapi_h_path

# C types to ctypes types

_primitive_types = {
        "void"      : "None",
        "int"       : "ctypes.c_int",
        "long"      : "ctypes.c_long",
        "int8_t"    : "ctypes.c_int8",
        "uint8_t"   : "ctypes.c_uint8",
        "int16_t"   : "ctypes.c_int16",
        "uint16_t"  : "ctypes.c_uint16",
        "int32_t"   : "ctypes.c_int32",
        "uint32_t"  : "ctypes.c_uint32",
        "int64_t"   : "ctypes.c_int64",
        "uint64_t"  : "ctypes.c_uint64",
        "intptr_t"  : "ctypes.c_int64",
        "uintptr_t" : "ctypes.c_uint64",
        "float"     : "ctypes.c_float",
        "double"    : "ctypes.c_double",
        "char*"     : "ctypes.c_char_p",
        "void*"     : "ctypes.c_void_p",
        }

def type_is_explicit_ptr(ty):
    return ty.endswith("*")

r_handle_ty = re.compile(r'^Mu\w*(Value|Node)$')

def type_is_handle(ty):
    return r_handle_ty.match(ty) is not None

def type_is_ptr(ty):
    return type_is_explicit_ptr(ty) or type_is_handle(ty)

def type_is_handle_array(ty):
    return type_is_ptr(ty) and type_is_handle(ty[:-1])

def to_basic_type(typedefs, name):
    while name in typedefs:
        name = typedefs[name]
    return name

_no_conversion = {
        "MuID",          # It's just Int.
        "MuTrapHandler", # It is a function pointer. Handle in Scala.
        "MuCPtr",        # Intended to be raw pointer. Passed directly.
        "MuCFP",         # ditto
        "MuWPID",        # Just Int
        "MuCommInst",    # Only used in new_comminst, and the builder uses opcode directly.
        }

_array_converters = {
        "char*"     : "readCharArray",
        "uint64_t*" : "readLongArray",
        "MuFlag*"   : "readFlagArray",
        }

_special_converters = {
        "MuBool"          : "intToBoolean",
        "MuName"          : "readCString",
        "MuMemOrd"        : "toMemoryOrder",
        "MuAtomicRMWOptr" : "toAtomicRMWOptr",
        "MuBinOptr"       : "toBinOptr",
        "MuCmpOptr"       : "toCmpOptr",
        "MuConvOptr"      : "toConvOptr",
        "MuCallConv"      : "toFlag",
        "MuDestKind"      : "toDestKind",
        }

_special_return_converters = {
        "MuBool" : "booleanToInt",
        "MuName" : "exposeString",
        "MuCtx*" : "exposeMuCtx",
        "MuVM*"  : "exposeMicroVM",
        }

_manually_defined_types = [
        "MuCFP",
        "MuValuesFreer",
        "MuTrapHandler",
        ]

def generate_ctypes(ast):
    typedefs = ast["typedefs_order"]

    lines = []

    for f, t in typedefs:
        if f in _manually_defined_types:
            continue
        elif t in _primitive_types:
            nex = _primitive_types[t]
        else:
            nex = "C" + t
        fro = "C" + f
        lines.append("{} = {}".format(fro, nex))

    return "\n".join(lines)

_enums = [
        ("MuTrapHandlerResult", 'MU_'),
        ("MuDestKind",          'MU_DEST_'),
        ("MuBinOptr",           'MU_BINOP_'),
        ("MuCmpOptr",           'MU_CMP_'),
        ("MuConvOptr",          'MU_CONV_'),
        ("MuMemOrd",            'MU_ORD_'),
        ("MuAtomicRMWOptr",     'MU_ARMW_'),
        ("MuCallConv",          'MU_CC_'),
        #("MuCommInst",          'MU_CI_'), # Generate a dictionary instead.
        ]

def generate_cenums(ast):
    enums_map = {e["name"]:e["defs"] for e in ast["enums"]}

    lines = []

    for ty, prefix in _enums:
        defs = enums_map[ty]

        lines.append("class {}:".format(ty))
        for d in defs:
            dn, dv = d["name"], d["value"]
            assert dn.startswith(prefix)
            pn = dn[len(prefix):]
            lines.append("    {} = {}".format(pn, dv))

        lines.append("")

    comminst_defs = enums_map["MuCommInst"]
    lines.append("common_instruction_opcodes = {")
    for d in comminst_defs:
        dp, dv = d["pragmas"], d["value"]
        muname = [pv for pk, pv in (p.split(":") for p in dp) if pk == "muname"][0]
        lines.append("    '{}': {},".format(muname, dv))
    lines.append("}")
    
    return "\n".join(lines)

def c_is_subtype_of(typedefs, lhs, rhs):
    cur = lhs

    while cur != rhs:
        if cur not in typedefs:
            return False
        cur = typedefs[cur]

    return True


def generate_muvalues(ast):
    typedefs = ast["typedefs"]
    typedefs_order = ast["typedefs_order"]

    mu_value_derived = [k for k,v in typedefs_order
            if k != "MuValue" and c_is_subtype_of(typedefs, k, "MuValue")]
    abstract_mu_value = {t for t in mu_value_derived if not any(
        u for u in mu_value_derived if u != t and c_is_subtype_of(typedefs, u, t))}

    namesz = max(len(t) for t in mu_value_derived)

    lines = []

    for t in mu_value_derived:
        sup = typedefs[t]
        body = "pass" if t not in abstract_mu_value else "_ctypes_ = C{}".format(t)
        lines.append('class {} ({}): {}'.format( t.ljust(namesz), sup.ljust(namesz), body))

    return "\n".join(lines)

_special_convertions = {
        "MuBool": "bool",
        }

_defined_in_python = {k for k,v in _enums} | {
        "MuCFP",
        } 

_mu_structs = {
        "MuVM*": "MuVM",
        "MuCtx*": "MuCtx",
        }

def to_python_ty(cty):
    if cty is None:
        return None
    elif cty in _primitive_types:
        return _primitive_types[cty]
    elif cty in _special_convertions:
        return _special_convertions[cty]
    elif cty in _mu_structs:
        return _mu_structs[cty]
    elif type_is_handle(cty):
        return cty
    elif type_is_ptr(cty):
        return "ctypes.c_void_p"
    else:
        pty = "C" + cty
        # print("{} probably converts to {}".format(cty, pty))
        return pty

# These methods are not very pythonic. Append _ after their names.
_method_blacklist = {
        "MuVM": {
            "set_trap_handler",
            },
        "MuCtx": {
            "load_bundle",
            "load_hail",
            "handle_from_sint8",
            "handle_from_uint8",
            "handle_from_sint16",
            "handle_from_uint16",
            "handle_from_sint32",
            "handle_from_uint32",
            "handle_from_sint64",
            "handle_from_uint64",
            "handle_from_uint64s",
            "handle_to_sint8",
            "handle_to_uint8",
            "handle_to_sint16",
            "handle_to_uint16",
            "handle_to_sint32",
            "handle_to_uint32",
            "handle_to_sint64",
            "handle_to_uint64",
            "insert_element",
            "load",
            "store",
            "cmpxchg",
            "atomicrmw",
            "fence",
            "new_thread_nor",
            "new_thread_exc",
            "dump_keepalives",
            },
        }

def generate_method(typedefs, strname, meth) -> Tuple[str, str]:
    name    = meth['name']
    params  = meth['params'][1:]
    ret_ty  = meth['ret_ty']
    pragmas = meth['pragmas']

    param_tys = [p["type"] for p in params]

    py_param_tys = [to_python_ty(t) for t in param_tys]
    py_ret_ty    = to_python_ty(ret_ty)

    if name in _method_blacklist[strname]:
        name = name + "_"

    return """('{}', {}, [{}]),""".format(
            name, py_ret_ty, ", ".join(py_param_tys))

def generate_stubs_for_struct(ast, name) -> str:
    st      = [s for s in ast["structs"] if s["name"] == name][0]
    methods = st["methods"]

    typedefs = ast["typedefs"]

    results = []

    for meth in methods:
        code = generate_method(typedefs, name, meth)
        results.append(code)

    return "\n".join(results)

def main():
    with open(muapi_h_path) as f:
        src_text = f.read()

    ast = muapiparser.parse_muapi(src_text)

    c_types = generate_ctypes(ast)
    c_enums = generate_cenums(ast)
    muvalues = generate_muvalues(ast)
    muvm_stubs = generate_stubs_for_struct(ast, "MuVM")
    muctx_stubs = generate_stubs_for_struct(ast, "MuCtx")

    injectable_files["libmu.py"].inject_many({
        "CTYPES":  c_types,
        "CENUMS":  c_enums,
        "MUVALUE": muvalues,
        "MuVM":    muvm_stubs,
        "MuCtx":   muctx_stubs,
        })

main()
