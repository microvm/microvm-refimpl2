from __future__ import division, absolute_import, print_function, unicode_literals

from _libmuprivcommon import _assert_instance

def _is_str_like(v):
    return isinstance(v, str) or isinstance(v, unicode)

def _encode(v, encoding):
    _assert_instance(v, str, unicode)
    if isinstance(v, unicode):
        return v.encode(encoding)
    else:
        return v

def _decode(v, encoding):
    _assert_instance(v, str, unicode)
    if isinstance(v, str):
        return v.decode(encoding)
    else:
        return v

def _assert_int_like(v):
    _assert_instance(v, int, long)
