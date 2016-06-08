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

target_begin = '/// SCRIPT: GENERATED CODE BEGIN'
target_end   = '/// SCRIPT: GENERATED CODE END'

def find_line(lines, substr, start=0):
    for i in range(start, len(lines)):
        if substr in lines[i]:
            return i

    raise KeyError("Not found: " + substr)


def general_inject_generated_code(parent: str, begin: str, end: str, generated: str):
    lines = parent.splitlines()

    begin_line = find_line(lines, begin)
    end_line = find_line(lines, end, begin_line+1)

    new_lines = lines[:begin_line+1] + generated.splitlines() + lines[end_line:]

    return "\n".join(new_lines)

def inject_generated_code(parent, generated):
    return general_inject_generated_code(parent, target_begin, target_end, generated)

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

src_path = os.path.join(*"src/main/scala/uvm/refimpl/nat/cStubs.scala".split("/"))
with open(src_path) as f:
    src_text = f.read()

generated = "// goodbye world"

result_text = inject_generated_code(src_text, generated)

with tempfile.NamedTemporaryFile("w") as f:
    print("Backup to temporary file:", f.name)
    f.write(src_text)

with open(src_path, "w") as f:
    f.write(result_text)
