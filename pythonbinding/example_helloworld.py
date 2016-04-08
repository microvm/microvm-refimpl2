from __future__ import division, absolute_import, print_function, unicode_literals

import sys

import ctypes, ctypes.util
from libmupython2 import *

def slurp(filename):
    with open(filename) as t:
        return t.read()

prelude_uir = slurp("examples/prelude.uir")
helloworld_uir = slurp("examples/helloworld.uir")
helloworld_hail = slurp("examples/helloworld.hail")

libc = ctypes.CDLL(ctypes.util.find_library("c"))
libc.write.restype = ctypes.c_ssize_t
libc.write.argtypes = [ctypes.c_int, ctypes.c_void_p, ctypes.c_size_t]

dll = MuRefImpl2StartDLL("../cbinding/libmurefimpl2start.so")
mu = dll.mu_refimpl2_new()

with mu.new_context() as ctx:
    # frequently used function
    id_of = ctx.id_of

    # Load bundles and hail scripts.
    ctx.load_bundle(prelude_uir)
    ctx.load_bundle(helloworld_uir)
    ctx.load_hail(helloworld_hail)

    # Perpare the C function pointers:
    write_addr = ctypes.cast(libc.write, ctypes.c_void_p).value
    print("write_addr = ", write_addr, hex(write_addr))
    hwrite_addr = ctx.handle_from_fp(id_of("@write.fp"), write_addr)
    hwrite_g = ctx.handle_from_global(id_of("@write.g"))
    ctx.store(hwrite_g, hwrite_addr)
    
    # Prepare args.
    ## new string array
    nargs = len(sys.argv)
    hnargs = ctx.handle_from_int(nargs, 64)
    args = ctx.new_hybrid(id_of("@array_ref_string"), hnargs)

    ## length
    args_ir = ctx.get_iref(args)
    args0_ir = ctx.get_field_iref(args_ir, 0)
    ctx.store(args0_ir, hnargs)

    ## elements
    argsv_ir = ctx.get_var_part_iref(args_ir)

    for i, arg in enumerate(sys.argv):
        with DelayedDisposer() as x:   ### auto dispose
            ### new string
            arglen = len(arg)
            harglen = x << ctx.handle_from_int(arglen, 64)
            harg = x << ctx.new_hybrid(id_of("@string"), harglen)
            harg_ir = x << ctx.get_iref(harg)

            ### I ignored the hash

            ### length
            harg1_ir = x << ctx.get_field_iref(harg_ir, 1)
            ctx.store(harg1_ir, harglen)
            
            ### characters
            hargv_ir = x << ctx.get_var_part_iref(harg_ir)
            for j, ch in enumerate(arg):
                with DelayedDisposer() as y:   ### auto dispose
                    hj = y << ctx.handle_from_int(j, 64)
                    hargvj_ir = y << ctx.shift_iref(hargv_ir, hj)
                    hch = y << ctx.handle_from_int(ord(ch), 8)
                    ctx.store(hargvj_ir, hch)

            ### add to the array
            hi = x << ctx.handle_from_int(i, 64)
            argsvi_ir = x << ctx.shift_iref(argsv_ir, hi)
            ctx.store(argsvi_ir, harg)

    # Create a box to receive the return value from @main.
    # Mu stacks cannot return values to the client, so only the memory can be
    # used to exchange data.

    rvbox = ctx.new_fixed(id_of("@i32"))

    _start = ctx.handle_from_func(id_of("@_start"))
    st = ctx.new_stack(_start)
    th = ctx.new_thread(st, PassValues(args, rvbox))

    mu.execute()

    rvbox_ir = ctx.get_iref(rvbox)
    hrv = ctx.load(rvbox_ir).cast(MuIntValue)
    rv = ctx.handle_to_sint(hrv)

    print("Returned from Mu. Return value = {}".format(rv))

