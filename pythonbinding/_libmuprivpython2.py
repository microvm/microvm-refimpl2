from __future__ import division, absolute_import, print_function, unicode_literals

from _libmuprivcommon import _assert_instance

def _is_str_like(v):
    return isinstance(v, str) or isinstance(v, unicode)

def _encode_ascii(v):
    _assert_instance(v, str, unicode)
    if isinstance(v, unicode):
        return v.encode("ascii")
    else:
        return v

def _decode_ascii(v):
    _assert_instance(v, str, unicode)
    if isinstance(v, str):
        return v.decode("ascii")
    else:
        return v

def _assert_int_like(v):
    _assert_instance(v, int, long)
