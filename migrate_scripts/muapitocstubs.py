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

_self_getters = {
        "MuVM*": "getMuVM",
        "MuCtx*": "getMuCtx",
        }

def type_is_ptr(ty):
    return ty.endswith("*")

_special_cases = {
        "id":             "ID",
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
        "keepalives":     "KeepAlives",
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
        if inn in _special_case:
            outs.append(_special_case[inn])
        else:
            outs.append(inn[0].upper()+inn[1:])

    return "".join(outs)

r_handle_ty = re.compile(r'Mu\w*(Value|Node)')

def is_handle(ty):
    return r_handle_ty.match(ty) is not None

def is_handle_array(ty):
    return is_ptr(ty) and r_handle_ty.match(ty[:-1]) is not None

def generate_stubs(ast):
    stubs = []
    for st in ast["structs"]:
        if st["name"] == "MuCtx":
            for meth in st["methods"]:
                stubs.append(meth["name"])

    return "\n".join("// "+ fn for fn in stubs)


def generate_things(ast):
    stubs = generate_stubs(ast)

    enums = "" # TODO: generate_enums(ast)

    return "\n".join([stubs])

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
