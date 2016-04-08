#!/usr/bin/env python2

from __future__ import division, absolute_import, print_function, unicode_literals

import unittest

from libmupython2 import *

dll = MuRefImpl2StartDLL(u"libmurefimpl2start.so")
mu = dll.mu_refimpl2_new()

with mu.new_context() as ctx:
    ctx.load_bundle("""
    .typedef @i8 = int<8>
    .typedef @i16 = int<16>
    .typedef @i32 = int<32>
    .typedef @i64 = int<64>
    .typedef @float = float
    .typedef @double = double

    .const @FOO <@i64> = 10000
    .const @BAR <@double> = 3.25d

    .global @BAZ <@i64>

    .funcsig @fib.sig = (@i64) -> (@i64)
    .funcdecl @fib <@fib.sig>

    .typedef @s1 = struct<@i32 @i64>
    .typedef @a1 = array<@i32 5>
    .typedef @4xi32 = vector<@i32 4>
    """ + 
    "\n".join(".const @I32_{} <@i32> = {}".format(i,i) for i in range(20))

    + """

    .const @S1 <@s1> = {@I32_1 @I32_2}
    .const @A1 <@a1> = {@I32_3 @I32_4 @I32_5 @I32_6 @I32_7}
    .const @V1 <@4xi32> = {@I32_11 @I32_12 @I32_13 @I32_14}

    .typedef @h1 = hybrid<@i32 @i64 @i8>

    .global @main_rv <@i32>

    .funcsig @main.sig = (@i32) -> ()
    .funcdef @main VERSION %v1 <@main.sig> {
        %entry(<@i32> %n):
            %n2 = ADD <@i32> %n @I32_1
            STORE <@i32> @main_rv %n2
            COMMINST @uvm.thread_exit
    }

    .funcsig @trap_exit_test.sig = (@i32) -> ()
    .funcdef @trap_exit_test VERSION %v1 <@trap_exit_test.sig> {
        %entry(<@i32> %n):
            %n2 = ADD <@i32> %n @I32_1
            [%trap1] TRAP <> KEEPALIVE (%n %n2)
            COMMINST @uvm.thread_exit
    }

    .funcsig @trap_rebind_test.sig = () -> ()
    .funcdef @trap_rebind_test VERSION %v1 <@trap_rebind_test.sig> {
        %entry():
            (%r1 %r2 %r3) = [%trap1] TRAP <@i32 @i32 @i32> EXC(
                                %nor(%r1 %r2 %r3)
                                %exc()
                                )

        %nor(<@i32> %r1 <@i32> %r2 <@i32> %r3):
            [%trap2] TRAP <> KEEPALIVE (%r1 %r2 %r3)
            COMMINST @uvm.thread_exit
        %exc() [%e]:
            [%trap3] TRAP <> KEEPALIVE (%e)
            COMMINST @uvm.thread_exit
    }

    .typedef @void = void
    .typedef @refvoid = ref<@void>
    .const @NULLREF <@refvoid> = NULL
    """)

class TestRefImpl2CBinding(unittest.TestCase):

    def setUp(self):
        self.ctx = mu.new_context()

    def tearDown(self):
        self.ctx.close_context()

    def test_basics(self):
        h = self.ctx.handle_from_sint64_(100, 64)
        v = self.ctx.handle_to_sint64_(h)
        print("v=",v)
        self.assertEqual(v, 100)

        h2 = self.ctx.handle_from_int(2147483648, 32)
        v21 = self.ctx.handle_to_sint(h2)
        print("v21=", v21)
        self.assertEqual(v21, -2147483648)
        v22 = self.ctx.handle_to_uint(h2)
        print("v22=", v22)
        self.assertEqual(v22, 2147483648)

    def test_globalvars(self):
        ctx = self.ctx
        id_of = ctx.id_of

        h1 = ctx.handle_from_const(id_of("@FOO")).cast(MuIntValue)
        h2 = ctx.handle_from_const(id_of("@BAR")).cast(MuDoubleValue)
        h3 = ctx.handle_from_global(id_of("@BAZ"))
        h4 = ctx.handle_from_func(id_of("@fib"))

        v1 = ctx.handle_to_sint(h1)
        self.assertEqual(v1, 10000)
        v2 = ctx.handle_to_double(h2)
        self.assertEqual(v2, 3.25)

        self.assertIsInstance(h3, MuIRefValue)
        self.assertIsInstance(h4, MuFuncRefValue)

        ctx.delete_value(h1)
        ctx.delete_value(h2)
        ctx.delete_values(h3,h4)

    def test_aggregate(self):
        ctx = self.ctx
        id_of = ctx.id_of

        s1 = ctx.handle_from_const(id_of("@S1")).cast(MuStructValue)
        a1 = ctx.handle_from_const(id_of("@A1")).cast(MuArrayValue)
        v1 = ctx.handle_from_const(id_of("@V1")).cast(MuVectorValue)

        s10 = ctx.extract_value(s1, 0).cast(MuIntValue)
        s11 = ctx.extract_value(s1, 1).cast(MuIntValue)
        vs10 = ctx.handle_to_sint(s10)
        vs11 = ctx.handle_to_sint(s11)
        self.assertEqual(vs10, 1)
        self.assertEqual(vs11, 2)

        for i in range(5):
            hi = ctx.handle_from_int(i, 64)
            a1i = ctx.extract_element(a1, hi).cast(MuIntValue)
            va1i = ctx.handle_to_sint(a1i)
            self.assertEqual(va1i, 3+i, "va1{} != {}".format(i, 3+i))

        for i in range(4):
            hi = ctx.handle_from_int(i, 64)
            v1i = ctx.extract_element(v1, hi).cast(MuIntValue)
            vv1i = ctx.handle_to_sint(v1i)
            self.assertEqual(vv1i, 11+i, "vv1{} != {}".format(i, 3+i))

        zero = ctx.handle_from_int(0, 32)
        one = ctx.handle_from_int(1, 32)
        s2 = ctx.insert_value(s1, 0, one)
        a2 = ctx.insert_element(a1, zero, one)
        v2 = ctx.insert_element(v1, zero, one)

        self.assertIsInstance(s2, MuStructValue)
        self.assertIsInstance(a2, MuArrayValue)
        self.assertIsInstance(v2, MuVectorValue)

    def test_mem(self):
        ctx = self.ctx
        id_of = ctx.id_of

        r1 = ctx.new_fixed(id_of("@s1"))
        ir1 = ctx.get_iref(r1)
        ir10 = ctx.get_field_iref(ir1, 0)
        ir11 = ctx.get_field_iref(ir1, 1)

        hnum1 = ctx.handle_from_int(100, 32)
        hnum2 = ctx.handle_from_int(200, 64)

        ctx.store(ir10, hnum1, ord=MU_SEQ_CST)
        hr10 = ctx.load(ir10, ord=MU_SEQ_CST).cast(MuIntValue)
        vr10 = ctx.handle_to_sint(hr10)
        self.assertEqual(vr10, 100)

        ctx.store(ir11, hnum2)
        hr11 = ctx.load(ir11).cast(MuIntValue)
        vr11 = ctx.handle_to_sint(hr11)
        self.assertEqual(vr11, 200)

        r2 = ctx.new_fixed(id_of("@a1"))
        ir2 = ctx.get_iref(r2)
        zero = ctx.handle_from_int(0, 64)
        three = ctx.handle_from_int(3, 64)
        ir20 = ctx.get_elem_iref(ir2, zero)
        ir23 = ctx.get_elem_iref(ir2, three)
        ir23s = ctx.shift_iref(ir20, three)

        self.assertEqual(ctx.ref_eq(ir23, ir23s), 1)
        self.assertEqual(ctx.ref_eq(ir20, ir23), 0)
        self.assertEqual(ctx.ref_ult(ir20, ir23), 1)
        self.assertEqual(ctx.ref_ult(ir23, ir23s), 0)

        ctx.store(ir23, hnum1)
        hr23 = ctx.load(ir23s).cast(MuIntValue)
        vr23 = ctx.handle_to_sint(hr23)
        self.assertEqual(vr23, 100)

        r3 = ctx.new_hybrid(id_of("@h1"), three)
        ir3 = ctx.get_iref(r3)
        ir3f0 = ctx.get_field_iref(ir3, 0)
        ir3f1 = ctx.get_field_iref(ir3, 1)
        ir3v = ctx.get_var_part_iref(ir3)
        ir3v3 = ctx.shift_iref(ir3v, three)

        self.assertTrue(ctx.ref_eq(ir3, ir3f0))
        self.assertTrue(ctx.ref_ult(ir3f0, ir3v))

    def test_atomic(self):
        ctx = self.ctx
        id_of = ctx.id_of

        ir = ctx.handle_from_global(id_of("@BAZ"))
        h100 = ctx.handle_from_int(100, 64)
        h200 = ctx.handle_from_int(200, 64)
        h300 = ctx.handle_from_int(300, 64)
        ctx.store(ir, h100, ord=MU_SEQ_CST)

        h, succ = ctx.cmpxchg(ir, h100, h200)
        h = h.cast(MuIntValue)
        v = ctx.handle_to_sint(h)
        self.assertEqual(v, 100)
        self.assertTrue(succ)
        
        h2, succ2 = ctx.cmpxchg(ir, h100, h300)
        h2 = h2.cast(MuIntValue)
        v2 = ctx.handle_to_sint(h2)
        self.assertEqual(v2, 200)
        self.assertFalse(succ2)

        h3 = ctx.atomicrmw(MU_ADD, ir, h300).cast(MuIntValue)
        h4 = ctx.load(ir, ord=MU_SEQ_CST).cast(MuIntValue)
        v3 = ctx.handle_to_sint(h3)
        v4 = ctx.handle_to_sint(h4)
        self.assertEqual(v3, 200)
        self.assertEqual(v4, 500)

    def test_stack_thread(self):
        ctx = self.ctx
        id_of = ctx.id_of

        forty_two = ctx.handle_from_int(42, 32)

        main = ctx.handle_from_func(id_of("@main"))
        st = ctx.new_stack(main)
        th = ctx.new_thread(st, PassValues(forty_two))

        mu.execute()

        loc = ctx.handle_from_global(id_of("@main_rv"))
        hv = ctx.load(loc).cast(MuIntValue)
        v = ctx.handle_to_sint(hv)
        self.assertEqual(v, 43)
        

    def test_trap_exit(self):
        ctx = self.ctx
        id_of = ctx.id_of

        class MyHandler(MuTrapHandler):
            def handle_trap(self, ctx, thread, stack, wpid):
                print("Hey! This is Python!")

                return ThreadExit()

        mh = MyHandler()

        mu.set_trap_handler(mh)

        forty_two = ctx.handle_from_int(42, 32)

        func = ctx.handle_from_func(id_of("@trap_exit_test"))
        st = ctx.new_stack(func)
        th = ctx.new_thread(st, PassValues(forty_two))

        mu.execute()

    def test_trap_exit(self):
        ctx = self.ctx
        id_of = ctx.id_of

        result = []

        class MyHandler(MuTrapHandler):
            def handle_trap(self, ctx, thread, stack, wpid):
                print("Hey! This is Python!")
                cursor = ctx.new_cursor(stack)
                n, n2 = ctx.dump_keepalives(cursor, 2)
                n = n.cast(MuIntValue)
                n2 = n2.cast(MuIntValue)
                ctx.close_cursor(cursor)

                vn = ctx.handle_to_sint(n)
                vn2 = ctx.handle_to_sint(n2)

                print("vn, vn2 = ", vn, vn2)
                result.append(vn)
                result.append(vn2)

                return ThreadExit()

        mh = MyHandler()

        mu.set_trap_handler(mh)

        forty_two = ctx.handle_from_int(42, 32)

        func = ctx.handle_from_func(id_of("@trap_exit_test"))
        st = ctx.new_stack(func)
        th = ctx.new_thread(st, PassValues(forty_two))

        mu.execute()

        self.assertEqual(result, [42, 43])

    def test_trap_rebind_pass_value(self):
        ctx = self.ctx
        id_of = ctx.id_of
        name_of = ctx.name_of

        class MyHandler(MuTrapHandler):
            def __init__(self, expected, box):
                self.expected = expected
                self.box = box

            def handle_trap(self, ctx, thread, stack, wpid):
                print("Hey! This is Python!")
                cursor = ctx.new_cursor(stack)
                fid = ctx.cur_func(cursor)
                fvid = ctx.cur_func_ver(cursor)
                iid = ctx.cur_inst(cursor)
                ctx.close_cursor(cursor)
                print("The current frame:",fid, fvid, iid)
                print("The current frame:",name_of(fid), name_of(fvid), name_of(iid))

                if name_of(iid) == "@trap_rebind_test.v1.entry.trap1":
                    one = ctx.handle_from_int(1, 32)
                    two = ctx.handle_from_int(2, 32)
                    three = ctx.handle_from_int(3, 32)

                    if self.expected == True:
                        return Rebind(stack, PassValues(one, two, three))
                    else:
                        nul = ctx.handle_from_const(id_of("@NULLREF"))
                        return Rebind(stack, ThrowExc(nul))
                elif name_of(iid) == "@trap_rebind_test.v1.nor.trap2":
                    return_normally = True
                else:
                    return_normally = False

                if return_normally == self.expected:
                    print("Yes. You are there!")
                    self.box.append(True)
                else:
                    print("Oops! Wrong branch!")
                    self.box.append(False)

                return ThreadExit()

        test_result1 = []
        mh1 = MyHandler(True, test_result1)

        mu.set_trap_handler(mh1)

        func = ctx.handle_from_func(id_of("@trap_rebind_test"))
        st = ctx.new_stack(func)
        th = ctx.new_thread(st, PassValues())

        mu.execute()

        self.assertEqual(test_result1, [True])

        test_result2 = []
        mh2 = MyHandler(False, test_result2)

        mu.set_trap_handler(mh2)

        func = ctx.handle_from_func(id_of("@trap_rebind_test"))
        st = ctx.new_stack(func)
        th = ctx.new_thread(st, PassValues())

        mu.execute()

        self.assertEqual(test_result2, [True])
