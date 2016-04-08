from _libmuprivcommon import _assert_instance

def _is_str_like(v):
    return isinstance(v, str) or isinstance(v, bytes)

def _encode_ascii(v):
    _assert_instance(v, bytes, str)
    if isinstance(v, str):
        return v.encode("ascii")
    else:
        return v

def _decode_ascii(v):
    _assert_instance(v, bytes, str)
    if isinstance(v, bytes):
        return v.decode("ascii")
    else:
        return v

def _assert_int_like(v):
    _assert_instance(v, int)
