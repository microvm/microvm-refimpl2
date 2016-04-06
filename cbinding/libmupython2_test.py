#!/usr/bin/env python2

from __future__ import division, absolute_import, print_function, unicode_literals

from libmupython2 import *

dll = MuRefImpl2StartDLL(u"libmurefimpl2start.so")
mu = dll.mu_refimpl2_new()
ctx = mu.new_context()
h = ctx.handle_from_sint64(100, 64)

v = ctx.handle_to_sint64(h)
print("v=",v)
