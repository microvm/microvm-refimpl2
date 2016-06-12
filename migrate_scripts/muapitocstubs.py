"""
Converts MuCtx methods in muapi.h to nativeClientSupport

USAGE: python3 muapitoncs.py < cbinding/muapi.h | xclip -selection c

then paste in function signatures, then paste the result in
src/main/scala/uvm/refimpl/nat/nativeClientSupport.scala

Use pbcopy on Mac.
"""

import sys
import os, os.path
import re
import tempfile
from typing import Tuple

import muapiparser
import injecttools

target_begin = '/// SCRIPT: GENERATED CODE BEGIN'
target_end   = '/// SCRIPT: GENERATED CODE END'

def inject_generated_code(parent, generated):
    return injecttools.inject_lines(parent, target_begin, target_end, generated)

# C types to Scala types, JFFI types and JFFI Buffer getters and setters

_primitive_types = {
        "void":     ["Unit",   "VOID",   None,        None   ],
        "int":      ["Int",    "SINT",   "getInt",    "setIntReturn"],
        "long":     ["Long",   "SLONG",  "getLong",   "setLongReturn"],
        "int8_t":   ["Byte",   "SINT8",  "getByte",   "setByteReturn"],
        "uint8_t":  ["Byte",   "UINT8",  "getByte",   "setByteReturn"],
        "int16_t":  ["Short",  "SINT16", "getShort",  "setShortReturn"],
        "uint16_t": ["Short",  "UINT16", "getShort",  "setShortReturn"],
        "int32_t":  ["Int",    "SINT32", "getInt",    "setIntReturn"],
        "uint32_t": ["Int",    "UINT32", "getInt",    "setIntReturn"],
        "int64_t":  ["Long",   "SINT64", "getLong",   "setLongReturn"],
        "uint64_t": ["Long",   "UINT64", "getLong",   "setLongReturn"],
        "float":    ["Float",  "FLOAT",  "getFloat",  "setFloatReturn"],
        "double":   ["Double", "DOUBLE", "getDouble", "setDoubleReturn"],
        }

_other_ptr_types = {"MuName", "MuCFP", "MuTrapHandler", "MuValueFreer"}

_self_getters = {
        "MuVM*": "getMicroVM",
        "MuCtx*": "getMuCtx",
        }

def type_is_explicit_ptr(ty):
    return ty.endswith("*")

r_handle_ty = re.compile(r'^Mu\w*(Value|Node)$')

def type_is_handle(ty):
    return r_handle_ty.match(ty) is not None

def type_is_ptr(ty):
    return type_is_explicit_ptr(ty) or type_is_handle(ty) or ty in _other_ptr_types

def type_is_handle_array(ty):
    return type_is_ptr(ty) and type_is_handle(ty[:-1])

def to_jffi_ty(raw_type):
    if raw_type in _primitive_types:
        jty = _primitive_types[raw_type][1]
    elif type_is_ptr(raw_type):
        jty = "POINTER"
    else:
        raise ValueError("No JFFI JType: " + raw_type)

    return "JType." + jty

def to_jffi_getter(raw_type):
    if raw_type in _primitive_types:
        getter = _primitive_types[raw_type][2]
    elif type_is_ptr(raw_type):
        getter = "getAddress"
    else:
        raise ValueError("No JFFI Buffer getter: " + raw_type)

    return getter

def to_jffi_setter(raw_type):
    if raw_type in _primitive_types:
        getter = _primitive_types[raw_type][3]
    elif type_is_ptr(raw_type):
        getter = "setAddressReturn"
    else:
        raise ValueError("No JFFI Buffer getter: " + raw_type)

    return getter

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
        "MuCommInst",    # Onlu used in new_comminst, and the builder uses opcode directly.
        }

_array_converters = {
        "char*"     : "readCharArray",
        "uint64_t*" : "readLongArray",
        "MuFlag*"   : "readFlagArray",
        }

_special_converters = {
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
        "MuName" : "exposeString",
        "MuCtx*" : "exposeMuCtx",
        "MuVM*"  : "exposeMicroVM",
        }

def param_converter(pn, pt, rn, rt, is_optional, array_sz, is_bool, is_out):
    if pt == "void":
        raise ValueError("Parameter cannot be void. Param name: {}".format(pn))

    if pt == "int" and is_bool:
        return "{} != 0".format(rn)

    if pt in _primitive_types or pt in _no_conversion or is_out:
        return rn   # does not need conversion

    if array_sz is not None:
        if type_is_handle_array(pt):
            ac = "readMuValueArray"
        elif pt in _array_converters:
            ac = _array_converters[pt]
        else:
            raise ValueError("I don't know how to convert array {}. "
                    "Param name: {}, array size: {}".format(pt, pn, array_sz))
        return "{}({}, {})".format(ac, rn, array_sz)

    if type_is_handle(pt):
        if is_optional:
            return "getMuValueNullable({}).asInstanceOf[Option[{}]]".format(rn, pt)
        else:
            return "getMuValueNotNull({}).asInstanceOf[{}]".format(rn, pt)

    if pt in _special_converters:
        return "{}({})".format(_special_converters[pt], rn)

    raise ValueError("I don't know how to convert {}. Param name: {}".format(
        pt, pn))

def generate_method(typedefs, strname, meth) -> Tuple[str, str]:
    name    = meth['name']
    params  = meth['params']
    ret_ty  = meth['ret_ty']
    pragmas = meth['pragmas']

    valname = strname.upper() + "__" + name.upper()

    jffi_retty = to_jffi_ty(to_basic_type(typedefs, ret_ty))
    jffi_paramtys = [to_jffi_ty(to_basic_type(typedefs, p["type"])) for p in params]

    pretty_name = "{}.{}".format(strname, name)

    header = 'val {} = exposedMethod("{}", {}, Array({})) {{ _jffiBuffer =>'.format(
            valname, pretty_name, jffi_retty, ", ".join(jffi_paramtys))

    stmts = []

    # pragmas
    pragmas = [tuple(p.split(":")) for p in pragmas]

    array_szs = [p[2] for p in pragmas if p[1] == 'array']

    # get raw parameters
    for i in range(len(params)):
        param = params[i]
        pn = param['name']
        pt = param['type']
        rt = to_basic_type(typedefs, pt) # raw type
        jffi_getter = to_jffi_getter(rt)

        rn = "_raw_" + pn # raw name

        stmts.append("val {} = _jffiBuffer.{}({})".format(rn,
            jffi_getter, i))

    self_param_name = params[0]["name"]
    self_param_type = params[0]["type"]

    # get the self object (MuVM or MuCtx)

    stmts.append("val {} = {}({})".format(
        self_param_name,
        _self_getters[self_param_type],
        "_raw_"+self_param_name))

    # convert parameters
    args_to_pass = []

    for i in range(1, len(params)):
        param = params[i]
        pn = param['name']

        if pn in array_szs:
            continue    # Array sizes don't need to be passed explicitly.

        args_to_pass.append(pn)

        pt = param['type']
        rn = "_raw_" + pn
        rt = to_basic_type(typedefs, pt)

        pps = [p for p in pragmas if p[0] == pn]
        is_optional = False
        array_sz = None
        is_bool = False
        is_out = False
        for pp in pps:
            if pp[1] == 'array':
                array_sz = "_raw_" + pp[2]
            elif pp[1] == 'optional':
                is_optional = True
            elif pp[1] == 'bool':
                is_bool = True
            elif pp[1] == 'out':
                is_out = True

        pc = param_converter(pn, pt, rn, rt, is_optional, array_sz, is_bool, is_out)

        stmts.append("val {} = {}".format(pn, pc))

    # make the call

    camelName = toCamelCase(name)
    stmts.append("val _RV = {}.{}({})".format(
        self_param_name, camelName, ", ".join(args_to_pass)))

    # return value

    if ret_ty != "void":
        raw_ret_ty = to_basic_type(typedefs, ret_ty)
        jffi_setter = to_jffi_setter(raw_ret_ty)

        if type_is_handle(ret_ty):
            assert(strname == "MuCtx")
            assert(jffi_setter == "setAddressReturn")
            stmts.append("val _RV_FAK = exposeMuValue({}, _RV)".format(
                self_param_name))
            stmts.append("_jffiBuffer.{}(_RV_FAK)".format(jffi_setter))
        elif ret_ty in _special_return_converters:
            assert(jffi_setter == "setAddressReturn")
            stmts.append("val _RV_FAK = {}(_RV)".format(
                _special_return_converters[ret_ty]))
            stmts.append("_jffiBuffer.{}(_RV_FAK)".format(jffi_setter))
        elif ("RV", "bool") in pragmas:
            stmts.append("_jffiBuffer.{}(if(_RV) 1 else 0)".format(jffi_setter))
        else:
            stmts.append("_jffiBuffer.{}(_RV)".format(jffi_setter))


    footer = "}"

    return (valname, "\n".join([header] + stmts + [footer]))

def generate_stubs_for_struct(typedefs, st) -> str:
    name    = st["name"]
    methods = st["methods"]

    results = []
    ptrs    = []

    for meth in methods:
        ptrname, code = generate_method(typedefs, name, meth)
        ptrs.append(ptrname)
        results.append(code)

    results.append("val stubsOf{} = new Array[Word]({})".format(name, len(ptrs)))
    for i,ptr in enumerate(ptrs):
        results.append("stubsOf{}({}) = {}.address".format(name, i, ptr))

    return "\n".join(results)

def generate_stubs(ast):
    struct_codes = []

    for st in ast["structs"]:
        code = generate_stubs_for_struct(ast["typedefs"], st)
        struct_codes.append(code)

    return "\n".join(struct_codes)

def generate_enums(ast):
    vals = []

    for enum in ast['enums']:
        for d in enum['defs']:
            vals.append("val {} = {}".format(d['name'], d['value']))

    return "\n".join(vals)

_enum_types_to_generate_converters = [
        ("MuDestKind",      "DestKind",      'MU_DEST_'),
        ("MuBinOptr",       "BinOptr",       'MU_BINOP_'),
        ("MuCmpOptr",       "CmpOptr",       'MU_CMP_'),
        ("MuConvOptr",      "ConvOptr",      'MU_CONV_'),
        ("MuMemOrd",        "MemoryOrder",   'MU_ORD_'),
        ("MuAtomicRMWOptr", "AtomicRMWOptr", 'MU_ARMW_'),
        ]

def generate_enum_converters(ast):
    enums = ast['enums']
    edict = {}

    for e in enums:
        edict[e['name']] = e['defs']

    lines = []

    for cty, sty, prefix in _enum_types_to_generate_converters:
        func_name = "to"+sty
        lines.append("def {}(cval: {}): {}.Value = cval match {{".format(
            func_name, cty, sty))

        defs = edict[cty]
        for d in defs:
            dn = d['name']
            dv = d['value']
            assert(dn.startswith(prefix))
            sn = dn[len(prefix):]
            lines.append("  case {} => {}.{}".format(dv, sty, sn))

        lines.append("}")

    return "\n".join(lines)

def generate_things(ast):
    stubs = generate_stubs(ast)

    enums = generate_enums(ast)

    enum_convs = generate_enum_converters(ast)

    return "\n".join([stubs, enums, enum_convs])

src_path = "cbinding/muapi.h"
dst_path = "src/main/scala/uvm/refimpl/nat/cStubs.scala"

def main():
    with open(src_path) as f:
        src_text = f.read()

    ast = muapiparser.parse_muapi(src_text)

    generated = generate_things(ast)

    with open(dst_path) as f:
        dst_text = f.read()

    result_text = inject_generated_code(dst_text, generated)

    with tempfile.NamedTemporaryFile("w") as f:
        print("Backup to temporary file:", f.name)
        f.write(dst_text)

    with open(dst_path, "w") as f:
        f.write(result_text)

main()
