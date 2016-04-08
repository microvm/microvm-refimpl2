from __future__ import division, absolute_import, print_function, unicode_literals

def _assert_instance(obj, *tys):
    if not any(isinstance(obj, ty) for ty in tys):
        raise AssertionError("{} is not an instance of {}".format(obj,
            " or ".join(map(str,tys))))

