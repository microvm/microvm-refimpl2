"""
Parse the muapi.h so that you can generate different bindings.

The result will be a simple JSON object (dict of dicts).
"""

import re

r_commpragma = re.compile(r'///\s*MUAPIPARSER:(.*)$')
r_comment = re.compile(r'//.*$', re.MULTILINE)
r_decl = re.compile(r'(?P<ret>\w+\s*\*?)\s*\(\s*\*\s*(?P<name>\w+)\s*\)\s*\((?P<params>[^)]*)\)\s*;\s*(?:///\s*MUAPIPARSER\s+(?P<pragma>.*)$)?', re.MULTILINE)
r_param = re.compile(r'\s*(?P<type>\w+\s*\*?)\s*(?P<name>\w+)')
r_value_ty = re.compile(r'Mu\w*(Value|Node)')

r_define = re.compile(r'#define\s*(?P<name>\w+)\s*(?P<value>\w+)')

r_struct_start = re.compile(r'^struct\s+(\w+)\s*\{')
r_struct_end = re.compile(r'^\};')

def filter_ret_ty(text):
    return text.replace(" ","")

def extract_params(text):
    params = []
    for text1 in text.split(','):
        ty, name = r_param.search(text1).groups()
        ty = ty.replace(" ",'')
        params.append({"type": ty, "name": name})

    return params


def extract_pragmas(text):
    text = text.strip()
    if len(text) == 0:
        return []
    else:
        return text.split(";")

def extract_methods(body):
    methods = []
    for ret, name, params, pragma in r_decl.findall(body):
        methods.append({
            "name": name,
            "params": extract_params(params),
            "ret_ty": filter_ret_ty(ret),
            "pragmas": extract_pragmas(pragma),
            })
        
    return methods

def extract_struct(lines, name):
    for i in range(len(lines)):
        m = r_struct_start.search(lines[i])
        if m is not None and m.group(1) == name:
            for j in range(i+1, len(lines)):
                m2 = r_struct_end.search(lines[j])
                if m2 is not None:
                    body = lines[i+1:j]
                    return "\n".join(body)
            else:
                raise Exception("Cannot find the end of struct {}".format(name))
    else:
        raise Exception("Cannot find the start of struct {}".format(name))

def extract_enums(lines, typename, pattern):
    defs = []
    for line in lines:
        m = r_define.search(line)
        if m is not None:
            name, value = m.groups()
            if pattern.search(name) is not None:
                defs.append({"name": name, "value": value})
    return {
            "name": typename,
            "defs": defs,
            }

_top_level_structs = ["MuVM", "MuCtx"]
_enums = [(typename, re.compile(regex)) for typename, regex in [
    ("MuTrapHandlerResult", r'^MU_(THREAD|REBIND)'),
    ("MuDestKind", r'^MU_DEST_'),
    ("MuBinOptr", r'^MU_BINOP_'),
    ("MuCmpOptr", r'^MU_CMP_'),
    ("MuConvOptr", r'^MU_CONV_'),
    ("MuMemOrd", r'^MU_ORD_'),
    ("MuAtomicRMWOp", r'^MU_ARMW_'),
    ("MuFlag", r'^MU_CC_'),
    ]]

def parse_muapi(text):
    structs = []

    lines = text.splitlines()

    for sn in _top_level_structs:
        b = extract_struct(lines, sn)
        methods = extract_methods(b)
        structs.append({"name": sn, "methods": methods})

    enums = []

    for tn,pat in _enums:
        enums.append(extract_enums(lines, tn, pat))

    return {
            "structs": structs,
            "enums": enums,
            }

if __name__=='__main__':
    import sys, pprint, shutil

    width = 80

    try:
        width, height = shutil.get_terminal_size((80, 25))
    except:
        pass

    text = sys.stdin.read()
    pprint.pprint(parse_muapi(text), width=width)


