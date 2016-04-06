#!/usr/bin/env python2

from __future__ import division, absolute_import, print_function, unicode_literals

from libmupython2 import *

dll = MuRefImpl2StartDLL(u"libmurefimpl2start.so")
mu = dll.mu_refimpl2_new()
ctx = mu.new_context()

h = ctx.handle_from_sint64_(100, 64)
v = ctx.handle_to_sint64_(h)
print("v=",v)
assert(v==100)

h2 = ctx.handle_from_int(2147483648, 32)
v21 = ctx.handle_to_sint(h2)
print("v21=", v21)
assert(v21==-2147483648)
v22 = ctx.handle_to_uint(h2)
print("v22=", v22)
assert(v22==2147483648)

