"""
libmu: Python binding for the Mu-client API.

This module works for both Python 2 and Python 3.

For PyPy users: This module is not RPython!

The Mu reference implementation 2 contains a ``cbinding`` directory which
produces the ``libmurefimpl2start.so`` library. Users of this Python binding
should construct a MuRefImpl2StartDLL object with the pathname of that shared
object as the argument to the constructor.

This binding is a "medium-level" binding. It is more Python-friendly than the
low-level raw C API, and it does a lot of run-time type checking when API
functions are invoked, but it does not provide higher-level functionalities than
the C API, such as the LLVM-style CFG builder.


==========
How to use
==========

Creating the Micro VM and Contexts
----------------------------------

This Python binding depends on the C binding. After building the C binding, a
shared object ``../cbinding/libmurefimpl2start.so`` will be generated. The
client needs to create a MuRefImpl2StartDLL object with the path of that SO as
its argument::

    dll = MuRefImpl2StartDLL("../cbinding/libmurefimpl2start.so")

Then a ``MuVM`` instance can be created from that object::

    mu = dll.mu_refimpl2_new()

or::

    mu = dll.mu_refimpl2_new_ex(
        sosSize = 2*1024*1024,
        losSize = 2*1024*1024,
        globalSize = 4*1024*1024,
        stackSize = 63*1024,
        staticCheck = False,
        sourceInfo = False,
        gcLog = "WARN",
        vmLog = "INFO",
    )

A ``MuCtx`` instance can be created from the ``MuVM`` object::

    ctx = mu.new_context()
    ...
    ctx.close_context()

or use the ``with`` statement:

    with mu.new_context() as ctx:
        ...


Exeucte Mu Programs
-------------------

To start executing Mu programs, the client should create a stack and a thread.
As a limitation of this reference implementation, the client also needs to
invoke the ``MuVM.execute()`` method to actually execute the program. For
example::

    dll = MuRefImpl2StartDLL("../cbinding/libmurefimpl2start.so")
    mu = dll.mu_refimpl2_new()

    with mu.new_context() as ctx:
        func   = ctx.handle_from_func(ctx.id_of("@main"))
        arg1   = ctx.handle_from_int(10, 64)
        arg2   = ctx.handle_from_double(3.14)
        stack  = ctx.new_stack(func)
        thread = ctx.new_thread(stack, None, PassValues(arg1, arg2))

    mu.execute()    # The thread will actually run now.


Invoking API Methods
--------------------

The ``MuVM`` and the ``MuCtx`` Python object mimics the ``MuVM`` and ``MuCtx``
structs in the C API.

Function pointers in the C struct are mapped to methods of the same name in the
corresponding Python object. The first argument of the function pointers in the
C API is always a pointer to the struct itself. This first argument is omitted.
Other arguments are auto converted to the corresponding C types thanks to the
``ctypes`` module. For example::

    # MuCtx *ctx = mvm->new_context(mvm);   // C
    ctx = mu.new_context()                  # Python

    # MuDoubleValue h = ctx->handle_from_double(ctx, 3.14);     // C
    h = ctx.handle_from_double(3.14)                            # Python

IDs are Python ``int`` and names are Python ``str``.

Handles are sub-classes of the ``MuValue`` class. They wrap the underlying
C-level handles (void*) and know which ``MuCtx`` they are created from. They can
be pasesed into methods like in C. For example::

    h = ctx.handle_from_double(3.14)    # a MuDoubleValue
    num = ctx.handle_to_double(h)       # num is the Python value 3.14

    id_of = ctx.id_of   # This method is so frequently used that it is
                        # preferrable to make a shorter alias.
    func   = ctx.handle_from_func(id_of("@factorial"))
    arg    = ctx.handle_from_int(10, 64)
    stack  = ctx.new_stack(func)
    thread = ctx.new_thread(stack, None, PassValues(arg))

Type checks are performed on each and every method call. Handles (instances of
MuValue) must match the expected argument types. For example, if a method
expects a ``MuIntValue``, passing a ``MuValue`` will raise an error::

    const_int = ctx.handle_from_const(id_of("@CONST_INT_42"))   # MuValue
    num = ctx.handle_to_sint(const_int)     # ERROR: Expect MuIntValue, got MuValue

``MuValue`` has a ``cast`` method to cast an instance to a desired subtype.::

    const_int = ctx.handle_from_const(id_of("@CONST_INT_42")).cast(MuIntValue) # MuIntValue
    num = ctx.handle_to_sint(const_int)     # OK

The following functions usually need casting: ``handle_from_const``,
``handle_from_expose``, ``extract_value``, ``extract_element``, ``load``,
``cmpxchg``, ``atomicrmw``, ``dump_keepalives``, ``expose``.

The signatures of some methods are completely re-designed in Python to make them
more "Pythonic". For example, the ``load_bundle`` method of ``MuCtx`` takes a
single Python ``str`` and does not need the length. The ``cmpxchg`` method now
returns a pair rather than having one of them as the "output argument". The trap
handler is also completely re-designed. Such methods are defined and documented
in ``MuVM`` and ``MuCtx``. If they are explicitly defined there, the original C
function is accessible with a suffix ``_``, such as ``load_bundle_`` which is
the original C function that takes a length as argument. If they are not
explicitly defined, they follow the conventions mentioned before.


Automatically Delete Handles
----------------------------

The ``DelayedDisposer`` class can delete selected handles created in a scope.
Read the docstring.


Further Reading
---------------

It is recommended to read the docstrings in the following types: ``MuVM``,
``MuCtx``, ``MuValue`` and ``MuTrapHandler``.

"""

from __future__ import division, absolute_import, print_function, unicode_literals

import sys

if sys.version_info[0] == 2:
    import _libmuprivpython2 as _priv
else:
    import _libmuprivpython3 as _priv

import ctypes, ctypes.util
import logging

logger = logging.getLogger(__name__)

_libc = ctypes.CDLL(ctypes.util.find_library("c"))
_libc.malloc.restype = ctypes.c_void_p
_libc.malloc.argtypes = [ctypes.c_size_t]
_libc.free.restype = None
_libc.free.argtypes = [ctypes.c_void_p]

#### Helper functions

def _assert_equal(a, b):
    if not (a == b):
        raise AssertionError("{} does not equal {}".format(a, b))

def _assert_range(v, lb, ub):
    if not (lb <= v <= ub):
        raise AssertionError("{} is not between {} and {}".format(v, lb, ub))

from _libmuprivcommon import _assert_instance

#### Low-level C types counterpart

def _funcptr(restype, *paramtypes, **kwargs):
    return ctypes.CFUNCTYPE(restype, *paramtypes, use_errno=True, **kwargs)

## GEN:BEGIN:CTYPES
CMuValue = ctypes.c_void_p
CMuSeqValue = CMuValue
CMuGenRefValue = CMuValue
CMuIntValue = CMuValue
CMuFloatValue = CMuValue
CMuDoubleValue = CMuValue
CMuUPtrValue = CMuValue
CMuUFPValue = CMuValue
CMuStructValue = CMuSeqValue
CMuArrayValue = CMuSeqValue
CMuVectorValue = CMuSeqValue
CMuRefValue = CMuGenRefValue
CMuIRefValue = CMuGenRefValue
CMuTagRef64Value = CMuGenRefValue
CMuFuncRefValue = CMuGenRefValue
CMuThreadRefValue = CMuGenRefValue
CMuStackRefValue = CMuGenRefValue
CMuFCRefValue = CMuGenRefValue
CMuIRNode = CMuGenRefValue
CMuBundleNode = CMuIRNode
CMuChildNode = CMuIRNode
CMuTypeNode = CMuChildNode
CMuFuncSigNode = CMuChildNode
CMuVarNode = CMuChildNode
CMuGlobalVarNode = CMuVarNode
CMuConstNode = CMuGlobalVarNode
CMuGlobalNode = CMuGlobalVarNode
CMuFuncNode = CMuGlobalVarNode
CMuExpFuncNode = CMuGlobalVarNode
CMuLocalVarNode = CMuVarNode
CMuNorParamNode = CMuLocalVarNode
CMuExcParamNode = CMuLocalVarNode
CMuInstResNode = CMuLocalVarNode
CMuFuncVerNode = CMuChildNode
CMuBBNode = CMuChildNode
CMuInstNode = CMuChildNode
CMuID = ctypes.c_uint32
CMuName = ctypes.c_char_p
CMuCPtr = ctypes.c_void_p
CMuBool = ctypes.c_int
CMuArraySize = ctypes.c_uint64
CMuWPID = ctypes.c_uint32
CMuFlag = ctypes.c_uint32
CMuTrapHandlerResult = CMuFlag
CMuDestKind = CMuFlag
CMuBinOptr = CMuFlag
CMuCmpOptr = CMuFlag
CMuConvOptr = CMuFlag
CMuMemOrd = CMuFlag
CMuAtomicRMWOptr = CMuFlag
CMuCallConv = CMuFlag
CMuCommInst = CMuFlag
## GEN:END:CTYPES

## GEN:BEGIN:CENUMS
class MuTrapHandlerResult:
    THREAD_EXIT = 0x00
    REBIND_PASS_VALUES = 0x01
    REBIND_THROW_EXC = 0x02

class MuDestKind:
    NORMAL = 0x01
    EXCEPT = 0x02
    TRUE = 0x03
    FALSE = 0x04
    DEFAULT = 0x05
    DISABLED = 0x06
    ENABLED = 0x07

class MuBinOptr:
    ADD = 0x01
    SUB = 0x02
    MUL = 0x03
    SDIV = 0x04
    SREM = 0x05
    UDIV = 0x06
    UREM = 0x07
    SHL = 0x08
    LSHR = 0x09
    ASHR = 0x0A
    AND = 0x0B
    OR = 0x0C
    XOR = 0x0D
    FADD = 0xB0
    FSUB = 0xB1
    FMUL = 0xB2
    FDIV = 0xB3
    FREM = 0xB4

class MuCmpOptr:
    EQ = 0x20
    NE = 0x21
    SGE = 0x22
    SGT = 0x23
    SLE = 0x24
    SLT = 0x25
    UGE = 0x26
    UGT = 0x27
    ULE = 0x28
    ULT = 0x29
    FFALSE = 0xC0
    FTRUE = 0xC1
    FUNO = 0xC2
    FUEQ = 0xC3
    FUNE = 0xC4
    FUGT = 0xC5
    FUGE = 0xC6
    FULT = 0xC7
    FULE = 0xC8
    FORD = 0xC9
    FOEQ = 0xCA
    FONE = 0xCB
    FOGT = 0xCC
    FOGE = 0xCD
    FOLT = 0xCE
    FOLE = 0xCF

class MuConvOptr:
    TRUNC = 0x30
    ZEXT = 0x31
    SEXT = 0x32
    FPTRUNC = 0x33
    FPEXT = 0x34
    FPTOUI = 0x35
    FPTOSI = 0x36
    UITOFP = 0x37
    SITOFP = 0x38
    BITCAST = 0x39
    REFCAST = 0x3A
    PTRCAST = 0x3B

class MuMemOrd:
    NOT_ATOMIC = 0x00
    RELAXED = 0x01
    CONSUME = 0x02
    ACQUIRE = 0x03
    RELEASE = 0x04
    ACQ_REL = 0x05
    SEQ_CST = 0x06

class MuAtomicRMWOptr:
    XCHG = 0x00
    ADD = 0x01
    SUB = 0x02
    AND = 0x03
    NAND = 0x04
    OR = 0x05
    XOR = 0x06
    MAX = 0x07
    MIN = 0x08
    UMAX = 0x09
    UMIN = 0x0A

class MuCallConv:
    DEFAULT = 0x00

common_instruction_opcodes = {
    '@uvm.new_stack': 0x201,
    '@uvm.kill_stack': 0x202,
    '@uvm.thread_exit': 0x203,
    '@uvm.current_stack': 0x204,
    '@uvm.set_threadlocal': 0x205,
    '@uvm.get_threadlocal': 0x206,
    '@uvm.tr64.is_fp': 0x211,
    '@uvm.tr64.is_int': 0x212,
    '@uvm.tr64.is_ref': 0x213,
    '@uvm.tr64.from_fp': 0x214,
    '@uvm.tr64.from_int': 0x215,
    '@uvm.tr64.from_ref': 0x216,
    '@uvm.tr64.to_fp': 0x217,
    '@uvm.tr64.to_int': 0x218,
    '@uvm.tr64.to_ref': 0x219,
    '@uvm.tr64.to_tag': 0x21a,
    '@uvm.futex.wait': 0x220,
    '@uvm.futex.wait_timeout': 0x221,
    '@uvm.futex.wake': 0x222,
    '@uvm.futex.cmp_requeue': 0x223,
    '@uvm.kill_dependency': 0x230,
    '@uvm.native.pin': 0x240,
    '@uvm.native.unpin': 0x241,
    '@uvm.native.expose': 0x242,
    '@uvm.native.unexpose': 0x243,
    '@uvm.native.get_cookie': 0x244,
    '@uvm.meta.id_of': 0x250,
    '@uvm.meta.name_of': 0x251,
    '@uvm.meta.load_bundle': 0x252,
    '@uvm.meta.load_hail': 0x253,
    '@uvm.meta.new_cursor': 0x254,
    '@uvm.meta.next_frame': 0x255,
    '@uvm.meta.copy_cursor': 0x256,
    '@uvm.meta.close_cursor': 0x257,
    '@uvm.meta.cur_func': 0x258,
    '@uvm.meta.cur_func_Ver': 0x259,
    '@uvm.meta.cur_inst': 0x25a,
    '@uvm.meta.dump_keepalives': 0x25b,
    '@uvm.meta.pop_frames_to': 0x25c,
    '@uvm.meta.push_frame': 0x25d,
    '@uvm.meta.enable_watchpoint': 0x25e,
    '@uvm.meta.disable_watchpoint': 0x25f,
    '@uvm.meta.set_trap_handler': 0x260,
    '@uvm.irbuilder.new_bundle': 0x300,
    '@uvm.irbuilder.load_bundle_from_node': 0x301,
    '@uvm.irbuilder.abort_bundle_node': 0x302,
    '@uvm.irbuilder.get_node': 0x303,
    '@uvm.irbuilder.get_id': 0x304,
    '@uvm.irbuilder.set_name': 0x305,
    '@uvm.irbuilder.new_type_int': 0x306,
    '@uvm.irbuilder.new_type_float': 0x307,
    '@uvm.irbuilder.new_type_double': 0x308,
    '@uvm.irbuilder.new_type_uptr': 0x309,
    '@uvm.irbuilder.set_type_uptr': 0x30a,
    '@uvm.irbuilder.new_type_ufuncptr': 0x30b,
    '@uvm.irbuilder.set_type_ufuncptr': 0x30c,
    '@uvm.irbuilder.new_type_struct': 0x30d,
    '@uvm.irbuilder.new_type_hybrid': 0x30e,
    '@uvm.irbuilder.new_type_array': 0x30f,
    '@uvm.irbuilder.new_type_vector': 0x310,
    '@uvm.irbuilder.new_type_void': 0x311,
    '@uvm.irbuilder.new_type_ref': 0x312,
    '@uvm.irbuilder.set_type_ref': 0x313,
    '@uvm.irbuilder.new_type_iref': 0x314,
    '@uvm.irbuilder.set_type_iref': 0x315,
    '@uvm.irbuilder.new_type_weakref': 0x316,
    '@uvm.irbuilder.set_type_weakref': 0x317,
    '@uvm.irbuilder.new_type_funcref': 0x318,
    '@uvm.irbuilder.set_type_funcref': 0x319,
    '@uvm.irbuilder.new_type_tagref64': 0x31a,
    '@uvm.irbuilder.new_type_threadref': 0x31b,
    '@uvm.irbuilder.new_type_stackref': 0x31c,
    '@uvm.irbuilder.new_type_framecursorref': 0x31d,
    '@uvm.irbuilder.new_type_irnoderef': 0x31e,
    '@uvm.irbuilder.new_funcsig': 0x31f,
    '@uvm.irbuilder.new_const_int': 0x320,
    '@uvm.irbuilder.new_const_int_ex': 0x321,
    '@uvm.irbuilder.new_const_float': 0x322,
    '@uvm.irbuilder.new_const_double': 0x323,
    '@uvm.irbuilder.new_const_null': 0x324,
    '@uvm.irbuilder.new_const_seq': 0x325,
    '@uvm.irbuilder.new_global_cell': 0x326,
    '@uvm.irbuilder.new_func': 0x327,
    '@uvm.irbuilder.new_func_ver': 0x328,
    '@uvm.irbuilder.new_exp_func': 0x329,
    '@uvm.irbuilder.new_bb': 0x32a,
    '@uvm.irbuilder.new_nor_param': 0x32b,
    '@uvm.irbuilder.new_exc_param': 0x32c,
    '@uvm.irbuilder.new_inst_res': 0x32d,
    '@uvm.irbuilder.add_dest': 0x32e,
    '@uvm.irbuilder.add_keepalives': 0x32f,
    '@uvm.irbuilder.new_binop': 0x330,
    '@uvm.irbuilder.new_cmp': 0x331,
    '@uvm.irbuilder.new_conv': 0x332,
    '@uvm.irbuilder.new_select': 0x333,
    '@uvm.irbuilder.new_branch': 0x334,
    '@uvm.irbuilder.new_branch2': 0x335,
    '@uvm.irbuilder.new_switch': 0x336,
    '@uvm.irbuilder.add_switch_dest': 0x337,
    '@uvm.irbuilder.new_call': 0x338,
    '@uvm.irbuilder.new_tailcall': 0x339,
    '@uvm.irbuilder.new_ret': 0x33a,
    '@uvm.irbuilder.new_throw': 0x33b,
    '@uvm.irbuilder.new_extractvalue': 0x33c,
    '@uvm.irbuilder.new_insertvalue': 0x33d,
    '@uvm.irbuilder.new_extractelement': 0x33e,
    '@uvm.irbuilder.new_insertelement': 0x33f,
    '@uvm.irbuilder.new_shufflevector': 0x340,
    '@uvm.irbuilder.new_new': 0x341,
    '@uvm.irbuilder.new_newhybrid': 0x342,
    '@uvm.irbuilder.new_alloca': 0x343,
    '@uvm.irbuilder.new_allocahybrid': 0x344,
    '@uvm.irbuilder.new_getiref': 0x345,
    '@uvm.irbuilder.new_getfieldiref': 0x346,
    '@uvm.irbuilder.new_getelemiref': 0x347,
    '@uvm.irbuilder.new_shiftiref': 0x348,
    '@uvm.irbuilder.new_getvarpartiref': 0x349,
    '@uvm.irbuilder.new_load': 0x34a,
    '@uvm.irbuilder.new_store': 0x34b,
    '@uvm.irbuilder.new_cmpxchg': 0x34c,
    '@uvm.irbuilder.new_atomicrmw': 0x34d,
    '@uvm.irbuilder.new_fence': 0x34e,
    '@uvm.irbuilder.new_trap': 0x34f,
    '@uvm.irbuilder.new_watchpoint': 0x350,
    '@uvm.irbuilder.new_wpbranch': 0x351,
    '@uvm.irbuilder.new_ccall': 0x352,
    '@uvm.irbuilder.new_newthread': 0x353,
    '@uvm.irbuilder.new_swapstack_ret': 0x354,
    '@uvm.irbuilder.new_swapstack_kill': 0x355,
    '@uvm.irbuilder.set_newstack_pass_values': 0x356,
    '@uvm.irbuilder.set_newstack_throw_exc': 0x357,
    '@uvm.irbuilder.new_comminst': 0x358,
}
## GEN:END:CENUMS

CMuCFP = ctypes.c_void_p

CPtrMuValue = ctypes.POINTER(CMuValue)

CMuValuesFreer = _funcptr(
    None,   # return void
    CPtrMuValue,    # values
    CMuCPtr,        # freerdata
    )

class CMuVM(ctypes.Structure):
    pass    # Incomplete now. _fields_ will be patched later.

CPtrMuVM = ctypes.POINTER(CMuVM)

class CMuCtx(ctypes.Structure):
    pass    # Incomplete now. _fields_ will be patched later.

CPtrMuCtx = ctypes.POINTER(CMuCtx)

CMuTrapHandler = ctypes.CFUNCTYPE(
    # return value
    None,   # return void
    # input params
    ctypes.POINTER(CMuCtx),                 # ctx
    CMuThreadRefValue,                      # thread
    CMuStackRefValue,                       # stack
    ctypes.c_int,                           # wpid
    # output params
    ctypes.POINTER(CMuTrapHandlerResult),   # result
    ctypes.POINTER(CMuStackRefValue),       # new_stack
    ctypes.POINTER(CMuValue),               # values
    ctypes.POINTER(ctypes.c_int),           # nvalues
    ctypes.POINTER(CMuValuesFreer),         # freer
    ctypes.POINTER(CMuCPtr),                # freerdata
    ctypes.POINTER(CMuRefValue),            # exception
    # user-defined argument (input param)
    CMuCPtr,                                # userdata
    )

_MIN_SINT64 = (-1)<<63
_MAX_SINT64 = (1<<63)-1
_MAX_UINT64 = (1<<64)-1

#### High-level types which does type checking at run time.

class _LowLevelTypeWrapper(object):
    # _ctype_ = the low-level type

    def _to_low_level_arg(self):
        raise NotImplementedError()

    @classmethod
    def _from_low_level_retval(cls, v, high_level_struct):
        raise NotImplementedError()

class MuValue(_LowLevelTypeWrapper):
    """Wrapper of the C-level CMuValue.

    Concrete API methods in MuCtx expect subclasses of this type. Use the
    ``cast`` method to cast it to more specific types.
    """

    def delete(self):
        """Delete this MuValue from its MuCtx. Equivalent to ctx.delete_value(self)"""
        self.ctx.delete_value(self)

    def cast(self,ty):
        """Cast this MuValue to a different MuValue subclass."""
        return ty(self.c_mu_value, self.ctx)

    _ctype_ = CMuValue
    def __init__(self, c_mu_value, ctx):
        self.c_mu_value = c_mu_value
        self.ctx = ctx

    def _to_low_level_arg(self):
        return self.c_mu_value

    @classmethod
    def _from_low_level_retval(cls, v, high_level_struct):
        _assert_instance(high_level_struct, MuCtx)
        return cls(v, high_level_struct)

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.delete()

    def __str__(self):
        return "<{} handle={}>".format(type(self).__name__, self.c_mu_value)

    def __repr__(self):
        return str(self)

## GEN:BEGIN:MUVALUE
class MuSeqValue       (MuValue         ): pass
class MuGenRefValue    (MuValue         ): pass
class MuIntValue       (MuValue         ): _ctypes_ = CMuIntValue
class MuFloatValue     (MuValue         ): _ctypes_ = CMuFloatValue
class MuDoubleValue    (MuValue         ): _ctypes_ = CMuDoubleValue
class MuUPtrValue      (MuValue         ): _ctypes_ = CMuUPtrValue
class MuUFPValue       (MuValue         ): _ctypes_ = CMuUFPValue
class MuStructValue    (MuSeqValue      ): _ctypes_ = CMuStructValue
class MuArrayValue     (MuSeqValue      ): _ctypes_ = CMuArrayValue
class MuVectorValue    (MuSeqValue      ): _ctypes_ = CMuVectorValue
class MuRefValue       (MuGenRefValue   ): _ctypes_ = CMuRefValue
class MuIRefValue      (MuGenRefValue   ): _ctypes_ = CMuIRefValue
class MuTagRef64Value  (MuGenRefValue   ): _ctypes_ = CMuTagRef64Value
class MuFuncRefValue   (MuGenRefValue   ): _ctypes_ = CMuFuncRefValue
class MuThreadRefValue (MuGenRefValue   ): _ctypes_ = CMuThreadRefValue
class MuStackRefValue  (MuGenRefValue   ): _ctypes_ = CMuStackRefValue
class MuFCRefValue     (MuGenRefValue   ): _ctypes_ = CMuFCRefValue
class MuIRNode         (MuGenRefValue   ): pass
class MuBundleNode     (MuIRNode        ): _ctypes_ = CMuBundleNode
class MuChildNode      (MuIRNode        ): pass
class MuTypeNode       (MuChildNode     ): _ctypes_ = CMuTypeNode
class MuFuncSigNode    (MuChildNode     ): _ctypes_ = CMuFuncSigNode
class MuVarNode        (MuChildNode     ): pass
class MuGlobalVarNode  (MuVarNode       ): pass
class MuConstNode      (MuGlobalVarNode ): _ctypes_ = CMuConstNode
class MuGlobalNode     (MuGlobalVarNode ): _ctypes_ = CMuGlobalNode
class MuFuncNode       (MuGlobalVarNode ): _ctypes_ = CMuFuncNode
class MuExpFuncNode    (MuGlobalVarNode ): _ctypes_ = CMuExpFuncNode
class MuLocalVarNode   (MuVarNode       ): pass
class MuNorParamNode   (MuLocalVarNode  ): _ctypes_ = CMuNorParamNode
class MuExcParamNode   (MuLocalVarNode  ): _ctypes_ = CMuExcParamNode
class MuInstResNode    (MuLocalVarNode  ): _ctypes_ = CMuInstResNode
class MuFuncVerNode    (MuChildNode     ): _ctypes_ = CMuFuncVerNode
class MuBBNode         (MuChildNode     ): _ctypes_ = CMuBBNode
class MuInstNode       (MuChildNode     ): _ctypes_ = CMuInstNode
## GEN:END:MUVALUE

class NullablePointer:
    pass

# Trap handling

class TrapHandlerResult(object):
    def __init__(self):
        raise NotImplementedError()

class ThreadExit(TrapHandlerResult):
    def __init__(self):
        pass

class Rebind(TrapHandlerResult):
    def __init__(self, stack, how_to_resume):
        self.stack = stack
        self.how_to_resume = how_to_resume

# Binding threads to stacks. Used by new_stack and trap handling

class HowToResume(object):
    def __init__(self, *values):
        raise NotImplementedError()

class PassValues(HowToResume):
    def __init__(self, *values):
        self.values = values

class ThrowExc(HowToResume):
    def __init__(self, exc):
        self.exc = exc

class MuTrapHandler(object):
    def handle_trap(self, ctx, thread, stack, wpid):
        """Handle trap.

        This method is called when a trap is triggered by the micro VM. This
        method should handle the trap and tell the micro VM thread how to
        resume.

        Args:
            ctx: The MuCtx created by the micro VM for the trap handler to use.
                The trap handler should not close it.
            thread: A MuThreadRefValue of the thread that triggered the trap.
            stack: A MuStackRefValue of the current stack of the thread which
                triggered the trap.
            wpid: The watch-point ID if the trap is triggered by a WATCHPOINT.
                If it is triggered by TRAP, wpid==0.

        Returns:
            A TrapHandlerResult object which specifies how the thread that
            caused the trap should continue. Possible values are:

            ThreadExit(): The thread stops. The stack remains unbound.

            Rebind(new_stack, how_to_resume): The thread re-binds to a stack.

            In the case of Rebind:

            new_stack: The new stack the current thread should bind to after
                this trap. It may or may not be the same as the stack argument.

            how_to_resume: A HowToResume object which specifies how to bind a
                thread to the new_stack. Possible values are:

            PassValues(values...): The stack will continue normally, and the
                stack receives the list of values.

            ThrowExc(exc): The stack will continue exceptionally. exc is a
                MuRefValue which refers to the exception to be thrown to the
                stack.

        Examples:
            Stop the current thread::

                return ThreadExit()

            Rebind to the original stack and pass values::

                h1 = ctx.handle_from_xxxxxxxx(xxxxx)
                h2 = ctx.handle_from_xxxxxxxx(xxxxx)
                h3 = ctx.handle_from_xxxxxxxx(xxxxx)
                return Rebind(stack, PassValues(h1, h2, h3))

            Rebind to the original stack and throw an exception::

                h = ctx.new_fixed(ctx.id_of("@MyExceptionType"))
                return Rebind(stack, ThrowExc(h1))
        """
        raise NotImplementedError()

# Expose Python objects to native programs

class _ObjectExposer(object):
    """ Expose Python objects to native programs

    Pointers are not given out, but native programs hold "keys" (i.e. an int
    uniquely assigned to the object) to refer to Python objects.
    """
    def __init__(self):
        self._dic = {}
        self._next_key = 1

    def _get_key(self):
        while True:
            key = self._next_key
            self._next_key = (self._next_key + 1) & _MAX_UINT64
            if key not in self._dic:
                break
        return key

    def expose(self, obj):
        key = self._get_key()
        self._dic[key] = obj
        return key

    def get(self, key):
        return self._dic[key]

    def unexpose(self, key):
        if key in self._dic:
            del self._dic[key]

_trap_user_data_exposer = _ObjectExposer()

def _word_array_malloc(nelems):
    return _libc.malloc(ctypes.sizeof(CMuValue*nelems))

def _word_array_free(addr, freerdata):
    _libc.free(addr)

_THE_LOW_LEVEL_WORD_ARRAY_FREE_PTR = CMuValuesFreer(_word_array_free)
    
def _the_low_level_trap_handler(
        c_ctx, c_thread, c_stack, c_wpid,
        c_result, c_new_stack, c_values, c_nvalues, c_freer, c_freerdata,
        c_exception,
        c_userdata_key):

    userdata = _trap_user_data_exposer.get(c_userdata_key)
    muvm, trap_handler = userdata

    ctx = MuCtx(c_ctx, muvm)
    thread = MuThreadRefValue(c_thread, ctx)
    stack = MuStackRefValue(c_stack, ctx)
    wpid = c_wpid

    result = trap_handler.handle_trap(ctx, thread, stack, wpid)

    _assert_instance(result, TrapHandlerResult)

    if isinstance(result, ThreadExit):
        c_result.contents.value = MuTrapHandlerResult.THREAD_EXIT
        c_new_stack.contents.value = 0
        c_values.contents.value = None
        c_nvalues.contents.value = 0
        c_freer.contents.value = None
        c_freerdata.contents.value = None
        c_exception.contents.value = None
    else: # Rebind
        new_stack = result.stack
        htr = result.how_to_resume
        if isinstance(htr, PassValues):
            values = htr.values
            nvalues = len(values)

            cvalues_array_addr = _word_array_malloc(nvalues)
            cvalues_array = (CMuValue*nvalues).from_address(cvalues_array_addr)

            for i,v in enumerate(values):
                cvalues_array[i] = v.c_mu_value

            c_result.contents.value = MuTrapHandlerResult.REBIND_PASS_VALUES
            c_new_stack.contents.value = new_stack.c_mu_value
            c_values.contents.value = cvalues_array_addr
            c_nvalues.contents.value = nvalues
            c_freer.contents.value = _THE_LOW_LEVEL_WORD_ARRAY_FREE_PTR
            c_freerdata.contents.value = None
            c_exception.contents.value = None

        else:   # ThrowExc
            exc = htr.exc

            c_result.contents.value = MuTrapHandlerResult.REBIND_THROW_EXC
            c_new_stack.contents.value = new_stack.c_mu_value
            c_values.contents.value = None
            c_nvalues.contents.value = 0
            c_freer.contents.value = None
            c_freerdata.contents.value = None
            c_exception.contents.value = exc.c_mu_value

    return

_THE_LOW_LEVEL_TRAP_HANDLER_PTR = CMuTrapHandler(_the_low_level_trap_handler)
        

# Struct of methods

class _StructOfMethodsWrapper(_LowLevelTypeWrapper):
    """ High-level wrapper of "struct-of-method" types. """

    # _high_level_methods need to be assigned externally.
    # _muvm needs to refer to the associated MuVM object.

    def __init__(self, struct_ptr, parent=None):
        """
        struct_ptr: pointer to the underlying C struct

        parent: the Python object that returns this object. For example, If a
        MuVM returns a MuCtx via its ``new_context`` method, then the MuVM is
        the parent of the MuCtx.
        """
        self._struct_ptr = struct_ptr
        self._parent = parent

    def _to_low_level_arg(self):
        return self._struct_ptr

    @classmethod
    def _from_low_level_retval(cls, v, high_level_struct):
        return cls(v, high_level_struct)

    def _high_level_method(self, name):
        return self._high_level_methods[name]

    def _low_level_func(self, name):
        struct_ptr = self._struct_ptr
        ptr_contents = struct_ptr.contents
        func = getattr(ptr_contents, name)
        return func

    def __getattr__(self, name):
        high_level_method = self._high_level_method(name)
        return lambda *a: high_level_method(self, *a)

    def __str__(self):
        return "<{} _struct_ptr={}>".format(type(self).__name__,
                self._struct_ptr)

    def __repr__(self):
        return str(self)

class MuVM(_StructOfMethodsWrapper):
    """An instance of the Mu micro VM."""

    _c_struct_type_ = CMuVM
    _ctype_ = ctypes.POINTER(_c_struct_type_)

    def __init__(self, struct_ptr, dll):
        super(self.__class__, self).__init__(struct_ptr, dll)

        self.muvm = self
        self._mu_error_addr = self._low_level_func("get_mu_error_ptr")(struct_ptr)
        self._mu_error = ctypes.c_int.from_address(self._mu_error_addr)

        self._cur_user_data_key = None

    # Mu writes to the memory location self._mu_error_addr when it throws an
    # exception before returning to C. It is specific to this implementation and
    # is only used internally in the Python binding.

    def _get_mu_error(self):
        return self._mu_error.value

    def _set_mu_error(self, v):
        self._mu_error.value = v

    ## The following overrides the C functions to make them more Pythonic
    
    def set_trap_handler(self, trap_handler):
        """Set the trap handler of this MuVM.

        trap_handler is any Python MuTrapHandler object.
        """

        userdata = (self, trap_handler)

        old_key = self._cur_user_data_key
        new_key = _trap_user_data_exposer.expose(userdata)

        self.set_trap_handler_(_THE_LOW_LEVEL_TRAP_HANDLER_PTR, new_key)

        _trap_user_data_exposer.unexpose(old_key)
        self._cur_user_data_key = new_key

class MuCtx(_StructOfMethodsWrapper):
    """A Mu context.

    It holds MuValues and thread-local resources for the client.
    """

    _c_struct_type_ = CMuCtx
    _ctype_ = ctypes.POINTER(_c_struct_type_)

    def __init__(self, struct_ptr, muvm):
        super(self.__class__, self).__init__(struct_ptr, muvm)

        self.muvm = muvm

    ## The following methods enable the with statement.

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if exc_type != None:
            return False

        self.close_context()

    ## The following overrides the C functions to make them more Pythonic

    def load_bundle(self, bundle_str):
        """Load a Bundle.

        Arguments:
            bundle_str: a str or unicode as the text-based bundle.
        """
        ascii_bundle = _priv._encode(bundle_str, "ascii")
        return self.load_bundle_(ascii_bundle, len(ascii_bundle))

    def load_hail(self, hail_str):
        """Load a HAIL script.

        Arguments:
            bundle_str: a str or unicode as the text-based HAIL script.
        """
        ascii_bundle = _priv._encode(hail_str, "ascii")
        return self.load_hail_(ascii_bundle, len(ascii_bundle))

    def handle_from_int(self, value, length):
        """Convert a Python int to a Mu handle.

        Arguments:
            value: a Python int or long. Must be representable in 64-bit signed
            or unsigned number

            length: the desired bit length of the Mu int value

        Returns:
            a MuIntValue
        """
        _priv._assert_int_like(value)
        _assert_range(length, 0, 64)
        if _MIN_SINT64 <= value <= _MAX_SINT64:
            return self.handle_from_sint64_(value, length)
        elif 0 <= value <= _MAX_UINT64:
            return self.handle_from_uint64_(value, length)
        else:
            raise ValueError("value {} out of 64-bit range".format(value))

    def handle_to_sint(self, value):
        """Convert Mu int handle to a Python int, treating it as signed.

        Arguments:
            value: a MuIntValue.

        Returns:
            a Python int
        """
        return self.handle_to_sint64_(value)

    def handle_to_uint(self, value):
        """Convert Mu int handle to a Python int, treating it as unsigned.

        Arguments:
            value: a MuIntValue.

        Returns:
            a Python int
        """
        return self.handle_to_uint64_(value)

    def delete_values(self, *values):
        """Delete all values in the ``values`` argument. ``values`` is an
        iterable of MuValue."""
        for value in values:
            self.delete_value(value)

    def insert_element(self, value, index, newval):
        """Wrapper of the underlying ``insert_element``. The result is
        automatically cast to the same type as ``value``, which must be either
        ``MuArrayValue`` or ``MuVectorValue``."""
        return self.insert_element_(value, index, newval).cast(type(value))

    def load(self, loc, ord=MuMemOrd.NOT_ATOMIC):
        """Wrapper of the underlying ``load``. The memory order is optional and
        defaults to MuMemOrd.NOT_ATOMIC."""
        _assert_instance(loc, MuIRefValue)
        return self.load_(ord, loc)

    def store(self, loc, newval, ord=MuMemOrd.NOT_ATOMIC):
        """Wrapper of the underlying ``store``. The memory order is optional and
        defaults to MuMemOrd.NOT_ATOMIC."""
        _assert_instance(loc, MuIRefValue)
        return self.store_(ord, loc, newval)

    def cmpxchg(self, loc, expected, desired, weak=False,
            ord_succ=MuMemOrd.SEQ_CST,
            ord_fail=MuMemOrd.SEQ_CST):
        """Wrapper of the underlying ``cmpxchg``.

        It is strong by default.

        The memory orders are optional and defaults to MuMemOrd.SEQ_CST.

        The return value is a pair (value, succ), where value is a MuValue of
        the old value, and succ is a bool which indicates whether this operation
        is successful.
        """
        _assert_instance(loc, MuIRefValue)
        weak = int(weak)
        succ_buf = ctypes.c_int(0)
        rv = self.cmpxchg_(ord_succ, ord_fail, weak, loc, expected, desired,
                ctypes.byref(succ_buf))
        succ = succ_buf.value != 0
        return (rv, succ)

    def atomicrmw(self, op, loc, opnd, ord=MuMemOrd.SEQ_CST):
        """Wrapper of the underlying ``atomicrmw``. The memory order is optional
        and defaults to MuMemOrd.SEQ_CST."""
        _assert_instance(loc, MuIRefValue)
        return self.atomicrmw_(ord, op, loc, opnd)

    def fence(self, ord=MuMemOrd.SEQ_CST):
        """Wrapper of the underlying ``fence``. The memory order is optional and
        defaults to MuMemOrd.SEQ_CST."""
        return self.fence_(ord)

    def new_thread(self, stack, threadlocal, how_to_resume):
        """Wrapper of the underlying ``new_thread``.
        
        This method now takes only a stack, a threadlocal and a ``HowToResume``
        value which can be either ``PassValues`` or ``ThrowExc``.

        Arguments:
            stack: a MuStackRefValue, the initial stack the new thread should
                bind to.
            threadlocal: a MuRefValue or None, the initial thread-local objref
                of the created thread.
            how_to_resume: a HowToResume value. See the docstring of
                MuTrapHandler.
        
        Returns:
            a MuThreadRefValue, referring to the newly created thread.
        """
        
        if threadlocal == None:
            threadlocal = NullablePointer
        else:
            _assert_instance(threadlocal, MuRefValue)

        _assert_instance(how_to_resume, HowToResume)
        if isinstance(how_to_resume, PassValues):
            values = how_to_resume.values
            cvals_ty = CMuValue * len(values)
            cvals = cvals_ty()
            for i,v in enumerate(values):
                cvals[i] = v.c_mu_value
            cnvals = len(values)
            return self.new_thread_nor_(stack, threadlocal, cvals, cnvals)
        else:
            exc = how_to_resume.exc
            cexc = exc.c_mu_value
            return self.new_thread_exc_(stack, threadlocal, cexc)


    def dump_keepalives(self, cursor, nvals):
        """Wrapper of the underlying ``dump_keepalives``.
        
        This method now takes a cursor and the number of keep-alive variables. It
        returns a list of handles.

        Arguments:
            cursor: a MuFCRefValue, the frame cursor to dump values from.

            nvals: a Python int, the number of keep-alive variables. Must be the
                same as the number of actual keep-alive variables of the current
                instruction. Since the client genereted all bundles, it should
                have full knowledge of the number of keep-alive variables and
                their types.
        
        Returns:
            a list of MuValue. Each of them is a keep-alive variable. They are
            in the same order as the KEEPALIVE clause in the Mu IR.
        """
        cvals = (CMuValue * nvals)()
        self.dump_keepalives_(cursor, cvals)
        return [MuValue(cvals[i], self) for i in range(nvals)]

def _to_low_level_type(ty):
    if ty == None:
        return None
    elif ty is bool:
        return CMuBool
    elif issubclass(ty, _LowLevelTypeWrapper):
        return ty._ctype_
    else:
        return ty

def _to_low_level_arg(argtype, arg):
    if isinstance(arg, _LowLevelTypeWrapper):
        return arg._to_low_level_arg()
    elif argtype is bool:
        return 1 if arg else 0
    elif _priv._is_str_like(arg):
        return _priv._encode(arg, "ascii")
    elif arg is NullablePointer:
        return 0
    else:
        return arg

def _from_low_level_retval(restype, low_level_rv, self):
    if isinstance(restype, type) and issubclass(restype, _LowLevelTypeWrapper):
        return restype._from_low_level_retval(low_level_rv, self)
    elif restype is bool:
        return low_level_rv != 0
    elif _priv._is_str_like(low_level_rv):
        return _priv._decode(low_level_rv, "ascii") 
    else:
        return low_level_rv

def _make_high_level_method(name, expected_nargs, restype, argtypes):
    def wrapper(self, *args):
        nargs = len(args)
        if nargs != expected_nargs:
            raise TypeError("{}() takes {} positional argument but "
                    "{} were given".format(name, expected_nargs, nargs))

        low_level_args = []
        for i, arg in enumerate(args):
            argtype = argtypes[i]
            if issubclass(argtype, _LowLevelTypeWrapper):
                # type checking
                if not isinstance(arg, argtype) and arg is not NullablePointer:
                    raise TypeError("Method {}, arg {}, expected type {}, "
                            "actual type: {}, value: {}".format(name, i,
                                argtypes[i], type(arg), arg))

            low_level_arg = _to_low_level_arg(argtype, arg)

            low_level_args.append(low_level_arg)

        struct_ptr = self._struct_ptr
        low_level_func = self._low_level_func(name)

        self.muvm._set_mu_error(0)
        low_level_rv = low_level_func(struct_ptr, *low_level_args)
        mu_error = self.muvm._get_mu_error()

        if mu_error != 0:
            raise RuntimeError("Error in Mu. mu_error={}".format(mu_error))

        rv = _from_low_level_retval(restype, low_level_rv, self)
        return rv

    return wrapper

def _initialize_methods(high_level_class, methods):
    """
    This function does two things:

    1. Populate the high_level_class (such as MuVM) with high-level methods.
    2. Populate the _field_ field of its low-level Structure (such as CMuVM).

    ``high_level_class`` is MuVM or MuCtx.
    
    ``method`` is a list of (name, restype, argtypes), where both restype and
    the elements of argtypes are high-level types, such as MuVM or MuIntValue.
    Type checking will be performed according to these descriptions in the
    generated high-level methods. C-level return values will be wrapped in
    high-level types.
    """

    low_level_class = high_level_class._c_struct_type_

    fields = [("header", ctypes.c_void_p)]
    high_level_methods = {}

    objtype_p = ctypes.POINTER(low_level_class)

    for name, restype, argtypes in methods:
        # make low-level struct field (function pointer)
        low_level_restype = _to_low_level_type(restype)
        low_level_argtypes = [_to_low_level_type(ty) for ty in argtypes]
        logger.error("Python binding: %s :: %s %s", name, low_level_restype, low_level_argtypes)
        funcptr = _funcptr(
                low_level_restype, # return value
                objtype_p, *low_level_argtypes # params. Start with a struct ptr
                )
        fields.append((name, funcptr))

        expected_nargs = len(argtypes)

        # make high-level method
        wrapper = _make_high_level_method(name, expected_nargs, restype,
                argtypes)

        high_level_methods[name] = wrapper

    low_level_class._fields_ = fields
    high_level_class._high_level_methods = high_level_methods

_initialize_methods(MuVM, [
## GEN:BEGIN:MuVM
('new_context', MuCtx, []),
('id_of', CMuID, [CMuName]),
('name_of', CMuName, [CMuID]),
('set_trap_handler_', None, [CMuTrapHandler, CMuCPtr]),
('execute', None, []),
('get_mu_error_ptr', ctypes.c_void_p, []),
## GEN:END:MuVM
    ])

_initialize_methods(MuCtx, [
## GEN:BEGIN:MuCtx
('id_of', CMuID, [CMuName]),
('name_of', CMuName, [CMuID]),
('close_context', None, []),
('load_bundle_', None, [ctypes.c_char_p, CMuArraySize]),
('load_hail_', None, [ctypes.c_char_p, CMuArraySize]),
('handle_from_sint8_', MuIntValue, [ctypes.c_int8, ctypes.c_int]),
('handle_from_uint8_', MuIntValue, [ctypes.c_uint8, ctypes.c_int]),
('handle_from_sint16_', MuIntValue, [ctypes.c_int16, ctypes.c_int]),
('handle_from_uint16_', MuIntValue, [ctypes.c_uint16, ctypes.c_int]),
('handle_from_sint32_', MuIntValue, [ctypes.c_int32, ctypes.c_int]),
('handle_from_uint32_', MuIntValue, [ctypes.c_uint32, ctypes.c_int]),
('handle_from_sint64_', MuIntValue, [ctypes.c_int64, ctypes.c_int]),
('handle_from_uint64_', MuIntValue, [ctypes.c_uint64, ctypes.c_int]),
('handle_from_uint64s_', MuIntValue, [ctypes.c_void_p, CMuArraySize, ctypes.c_int]),
('handle_from_float', MuFloatValue, [ctypes.c_float]),
('handle_from_double', MuDoubleValue, [ctypes.c_double]),
('handle_from_ptr', MuUPtrValue, [CMuID, CMuCPtr]),
('handle_from_fp', MuUFPValue, [CMuID, CMuCFP]),
('handle_to_sint8_', ctypes.c_int8, [MuIntValue]),
('handle_to_uint8_', ctypes.c_uint8, [MuIntValue]),
('handle_to_sint16_', ctypes.c_int16, [MuIntValue]),
('handle_to_uint16_', ctypes.c_uint16, [MuIntValue]),
('handle_to_sint32_', ctypes.c_int32, [MuIntValue]),
('handle_to_uint32_', ctypes.c_uint32, [MuIntValue]),
('handle_to_sint64_', ctypes.c_int64, [MuIntValue]),
('handle_to_uint64_', ctypes.c_uint64, [MuIntValue]),
('handle_to_float', ctypes.c_float, [MuFloatValue]),
('handle_to_double', ctypes.c_double, [MuDoubleValue]),
('handle_to_ptr', CMuCPtr, [MuUPtrValue]),
('handle_to_fp', CMuCFP, [MuUFPValue]),
('handle_from_const', MuValue, [CMuID]),
('handle_from_global', MuIRefValue, [CMuID]),
('handle_from_func', MuFuncRefValue, [CMuID]),
('handle_from_expose', MuValue, [CMuID]),
('delete_value', None, [MuValue]),
('ref_eq', bool, [MuGenRefValue, MuGenRefValue]),
('ref_ult', bool, [MuIRefValue, MuIRefValue]),
('extract_value', MuValue, [MuStructValue, ctypes.c_int]),
('insert_value', MuStructValue, [MuStructValue, ctypes.c_int, MuValue]),
('extract_element', MuValue, [MuSeqValue, MuIntValue]),
('insert_element_', MuSeqValue, [MuSeqValue, MuIntValue, MuValue]),
('new_fixed', MuRefValue, [CMuID]),
('new_hybrid', MuRefValue, [CMuID, MuIntValue]),
('refcast', MuGenRefValue, [MuGenRefValue, CMuID]),
('get_iref', MuIRefValue, [MuRefValue]),
('get_field_iref', MuIRefValue, [MuIRefValue, ctypes.c_int]),
('get_elem_iref', MuIRefValue, [MuIRefValue, MuIntValue]),
('shift_iref', MuIRefValue, [MuIRefValue, MuIntValue]),
('get_var_part_iref', MuIRefValue, [MuIRefValue]),
('load_', MuValue, [CMuMemOrd, MuIRefValue]),
('store_', None, [CMuMemOrd, MuIRefValue, MuValue]),
('cmpxchg_', MuValue, [CMuMemOrd, CMuMemOrd, bool, MuIRefValue, MuValue, MuValue, ctypes.c_void_p]),
('atomicrmw_', MuValue, [CMuMemOrd, CMuAtomicRMWOptr, MuIRefValue, MuValue]),
('fence_', None, [CMuMemOrd]),
('new_stack', MuStackRefValue, [MuFuncRefValue]),
('new_thread_nor_', MuThreadRefValue, [MuStackRefValue, MuRefValue, ctypes.c_void_p, CMuArraySize]),
('new_thread_exc_', MuThreadRefValue, [MuStackRefValue, MuRefValue, MuRefValue]),
('kill_stack', None, [MuStackRefValue]),
('set_threadlocal', None, [MuThreadRefValue, MuRefValue]),
('get_threadlocal', MuRefValue, [MuThreadRefValue]),
('new_cursor', MuFCRefValue, [MuStackRefValue]),
('next_frame', None, [MuFCRefValue]),
('copy_cursor', MuFCRefValue, [MuFCRefValue]),
('close_cursor', None, [MuFCRefValue]),
('cur_func', CMuID, [MuFCRefValue]),
('cur_func_ver', CMuID, [MuFCRefValue]),
('cur_inst', CMuID, [MuFCRefValue]),
('dump_keepalives_', None, [MuFCRefValue, ctypes.c_void_p]),
('pop_frames_to', None, [MuFCRefValue]),
('push_frame', None, [MuStackRefValue, MuFuncRefValue]),
('tr64_is_fp', bool, [MuTagRef64Value]),
('tr64_is_int', bool, [MuTagRef64Value]),
('tr64_is_ref', bool, [MuTagRef64Value]),
('tr64_to_fp', MuDoubleValue, [MuTagRef64Value]),
('tr64_to_int', MuIntValue, [MuTagRef64Value]),
('tr64_to_ref', MuRefValue, [MuTagRef64Value]),
('tr64_to_tag', MuIntValue, [MuTagRef64Value]),
('tr64_from_fp', MuTagRef64Value, [MuDoubleValue]),
('tr64_from_int', MuTagRef64Value, [MuIntValue]),
('tr64_from_ref', MuTagRef64Value, [MuRefValue, MuIntValue]),
('enable_watchpoint', None, [CMuWPID]),
('disable_watchpoint', None, [CMuWPID]),
('pin', MuUPtrValue, [MuValue]),
('unpin', None, [MuValue]),
('expose', MuValue, [MuFuncRefValue, CMuCallConv, MuIntValue]),
('unexpose', None, [CMuCallConv, MuValue]),
('new_bundle', MuBundleNode, []),
('load_bundle_from_node', None, [MuBundleNode]),
('abort_bundle_node', None, [MuBundleNode]),
('get_node', MuChildNode, [MuBundleNode, CMuID]),
('get_id', CMuID, [MuBundleNode, MuChildNode]),
('set_name', None, [MuBundleNode, MuChildNode, CMuName]),
('new_type_int', MuTypeNode, [MuBundleNode, ctypes.c_int]),
('new_type_float', MuTypeNode, [MuBundleNode]),
('new_type_double', MuTypeNode, [MuBundleNode]),
('new_type_uptr', MuTypeNode, [MuBundleNode]),
('set_type_uptr', None, [MuTypeNode, MuTypeNode]),
('new_type_ufuncptr', MuTypeNode, [MuBundleNode]),
('set_type_ufuncptr', None, [MuTypeNode, MuFuncSigNode]),
('new_type_struct', MuTypeNode, [MuBundleNode, ctypes.c_void_p, CMuArraySize]),
('new_type_hybrid', MuTypeNode, [MuBundleNode, ctypes.c_void_p, CMuArraySize, MuTypeNode]),
('new_type_array', MuTypeNode, [MuBundleNode, MuTypeNode, ctypes.c_uint64]),
('new_type_vector', MuTypeNode, [MuBundleNode, MuTypeNode, ctypes.c_uint64]),
('new_type_void', MuTypeNode, [MuBundleNode]),
('new_type_ref', MuTypeNode, [MuBundleNode]),
('set_type_ref', None, [MuTypeNode, MuTypeNode]),
('new_type_iref', MuTypeNode, [MuBundleNode]),
('set_type_iref', None, [MuTypeNode, MuTypeNode]),
('new_type_weakref', MuTypeNode, [MuBundleNode]),
('set_type_weakref', None, [MuTypeNode, MuTypeNode]),
('new_type_funcref', MuTypeNode, [MuBundleNode]),
('set_type_funcref', None, [MuTypeNode, MuFuncSigNode]),
('new_type_tagref64', MuTypeNode, [MuBundleNode]),
('new_type_threadref', MuTypeNode, [MuBundleNode]),
('new_type_stackref', MuTypeNode, [MuBundleNode]),
('new_type_framecursorref', MuTypeNode, [MuBundleNode]),
('new_type_irnoderef', MuTypeNode, [MuBundleNode]),
('new_funcsig', MuFuncSigNode, [MuBundleNode, ctypes.c_void_p, CMuArraySize, ctypes.c_void_p, CMuArraySize]),
('new_const_int', MuConstNode, [MuBundleNode, MuTypeNode, ctypes.c_uint64]),
('new_const_int_ex', MuConstNode, [MuBundleNode, MuTypeNode, ctypes.c_void_p, CMuArraySize]),
('new_const_float', MuConstNode, [MuBundleNode, MuTypeNode, ctypes.c_float]),
('new_const_double', MuConstNode, [MuBundleNode, MuTypeNode, ctypes.c_double]),
('new_const_null', MuConstNode, [MuBundleNode, MuTypeNode]),
('new_const_seq', MuConstNode, [MuBundleNode, MuTypeNode, ctypes.c_void_p, CMuArraySize]),
('new_global_cell', MuGlobalNode, [MuBundleNode, MuTypeNode]),
('new_func', MuFuncNode, [MuBundleNode, MuFuncSigNode]),
('new_func_ver', MuFuncVerNode, [MuBundleNode, MuFuncNode]),
('new_exp_func', MuExpFuncNode, [MuBundleNode, MuFuncNode, CMuCallConv, MuConstNode]),
('new_bb', MuBBNode, [MuFuncVerNode]),
('new_nor_param', MuNorParamNode, [MuBBNode, MuTypeNode]),
('new_exc_param', MuExcParamNode, [MuBBNode]),
('new_inst_res', MuInstResNode, [MuInstNode]),
('add_dest', None, [MuInstNode, CMuDestKind, MuBBNode, ctypes.c_void_p, CMuArraySize]),
('add_keepalives', None, [MuInstNode, ctypes.c_void_p, CMuArraySize]),
('new_binop', MuInstNode, [MuBBNode, CMuBinOptr, MuTypeNode, MuVarNode, MuVarNode]),
('new_cmp', MuInstNode, [MuBBNode, CMuCmpOptr, MuTypeNode, MuVarNode, MuVarNode]),
('new_conv', MuInstNode, [MuBBNode, CMuConvOptr, MuTypeNode, MuTypeNode, MuVarNode]),
('new_select', MuInstNode, [MuBBNode, MuTypeNode, MuTypeNode, MuVarNode, MuVarNode, MuVarNode]),
('new_branch', MuInstNode, [MuBBNode]),
('new_branch2', MuInstNode, [MuBBNode, MuVarNode]),
('new_switch', MuInstNode, [MuBBNode, MuTypeNode, MuVarNode]),
('add_switch_dest', None, [MuInstNode, MuConstNode, MuBBNode, ctypes.c_void_p, CMuArraySize]),
('new_call', MuInstNode, [MuBBNode, MuFuncSigNode, MuVarNode, ctypes.c_void_p, CMuArraySize]),
('new_tailcall', MuInstNode, [MuBBNode, MuFuncSigNode, MuVarNode, ctypes.c_void_p, CMuArraySize]),
('new_ret', MuInstNode, [MuBBNode, ctypes.c_void_p, CMuArraySize]),
('new_throw', MuInstNode, [MuBBNode, MuVarNode]),
('new_extractvalue', MuInstNode, [MuBBNode, MuTypeNode, ctypes.c_int, MuVarNode]),
('new_insertvalue', MuInstNode, [MuBBNode, MuTypeNode, ctypes.c_int, MuVarNode, MuVarNode]),
('new_extractelement', MuInstNode, [MuBBNode, MuTypeNode, MuTypeNode, MuVarNode, MuVarNode]),
('new_insertelement', MuInstNode, [MuBBNode, MuTypeNode, MuTypeNode, MuVarNode, MuVarNode, MuVarNode]),
('new_shufflevector', MuInstNode, [MuBBNode, MuTypeNode, MuTypeNode, MuVarNode, MuVarNode, MuVarNode]),
('new_new', MuInstNode, [MuBBNode, MuTypeNode]),
('new_newhybrid', MuInstNode, [MuBBNode, MuTypeNode, MuTypeNode, MuVarNode]),
('new_alloca', MuInstNode, [MuBBNode, MuTypeNode]),
('new_allocahybrid', MuInstNode, [MuBBNode, MuTypeNode, MuTypeNode, MuVarNode]),
('new_getiref', MuInstNode, [MuBBNode, MuTypeNode, MuVarNode]),
('new_getfieldiref', MuInstNode, [MuBBNode, bool, MuTypeNode, ctypes.c_int, MuVarNode]),
('new_getelemiref', MuInstNode, [MuBBNode, bool, MuTypeNode, MuTypeNode, MuVarNode, MuVarNode]),
('new_shiftiref', MuInstNode, [MuBBNode, bool, MuTypeNode, MuTypeNode, MuVarNode, MuVarNode]),
('new_getvarpartiref', MuInstNode, [MuBBNode, bool, MuTypeNode, MuVarNode]),
('new_load', MuInstNode, [MuBBNode, bool, CMuMemOrd, MuTypeNode, MuVarNode]),
('new_store', MuInstNode, [MuBBNode, bool, CMuMemOrd, MuTypeNode, MuVarNode, MuVarNode]),
('new_cmpxchg', MuInstNode, [MuBBNode, bool, bool, CMuMemOrd, CMuMemOrd, MuTypeNode, MuVarNode, MuVarNode, MuVarNode]),
('new_atomicrmw', MuInstNode, [MuBBNode, bool, CMuMemOrd, CMuAtomicRMWOptr, MuTypeNode, MuVarNode, MuVarNode]),
('new_fence', MuInstNode, [MuBBNode, CMuMemOrd]),
('new_trap', MuInstNode, [MuBBNode, ctypes.c_void_p, CMuArraySize]),
('new_watchpoint', MuInstNode, [MuBBNode, CMuWPID, ctypes.c_void_p, CMuArraySize]),
('new_wpbranch', MuInstNode, [MuBBNode, CMuWPID]),
('new_ccall', MuInstNode, [MuBBNode, CMuCallConv, MuTypeNode, MuFuncSigNode, MuVarNode, ctypes.c_void_p, CMuArraySize]),
('new_newthread', MuInstNode, [MuBBNode, MuVarNode, MuVarNode]),
('new_swapstack_ret', MuInstNode, [MuBBNode, MuVarNode, ctypes.c_void_p, CMuArraySize]),
('new_swapstack_kill', MuInstNode, [MuBBNode, MuVarNode]),
('set_newstack_pass_values', None, [MuInstNode, ctypes.c_void_p, ctypes.c_void_p, CMuArraySize]),
('set_newstack_throw_exc', None, [MuInstNode, MuVarNode]),
('new_comminst', MuInstNode, [MuBBNode, CMuCommInst, ctypes.c_void_p, CMuArraySize, ctypes.c_void_p, CMuArraySize, ctypes.c_void_p, CMuArraySize, ctypes.c_void_p, CMuArraySize]),
## GEN:END:MuCtx
    ])

class DelayedDisposer(object):
    """Automatically delete MuValues in the scope.

    Suggested usage::

        with mu.new_context() as ctx:
            ...

            with DelayedDisposer() as x:
                h1 = x << ctx.handle_from_int(1, 64)
                ...
            # h1 is deleted here

            for i in range(100):
                with DelayedDisposer() as x:
                    hi = x << ctx.handle_from_int(i, 64)
                    ...
                # hi is deleted here
    """
    def __init__(self):
        self.garbages = []

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if exc_type != None:
            return False

        self.delete_all()
        return False

    def add(self, handle):
        """Add handle as a MuValue to be disposed. If handle is not a MuValue,
        treat it as a list of handles."""
        if isinstance(handle, MuValue):
            self.garbages.append(handle)
        else:
            for h in handle:
                self.add(h)
        return handle

    def __lshift__(self, rhs):
        """The same as self.add(rhs)"""
        return self.add(rhs)

    def delete_all(self):
        """Delete all added handles"""
        for h in reversed(self.garbages):
            h.delete();

class MuRefImpl2StartDLL(object):
    """The factory object of MuVM instances."""

    def __init__(self, dll):
        """
        dll is a CDLL object of the "libmurefimpl2start.so" library, or a
        pathname to it. In the latter case, a CDLL will be created.
        """
        if _priv._is_str_like(dll):
            dll = ctypes.CDLL(dll)

        dll.mu_refimpl2_new.restype = CPtrMuVM
        dll.mu_refimpl2_new.argtypes = []

        dll.mu_refimpl2_new_ex.restype = CPtrMuVM
        dll.mu_refimpl2_new_ex.argtypes = [ctypes.c_char_p]

        dll.mu_refimpl2_close.restype = None
        dll.mu_refimpl2_close.argtypes = [CPtrMuVM]

        self.dll = dll

    def mu_refimpl2_new(self):
        """Create a MuVM instance using the default configuration."""
        ptr = self.dll.mu_refimpl2_new()
        return MuVM(ptr, self)

    def mu_refimpl2_new_ex(self, **kwargs):
        """Create a MuVM instance using custom configuration.

        Currently supported keyword arguments:
            sosSize: small object space size (bytes, must be 4096-byte aligned)
            losSize: large object space size (bytes, must be 4096-byte aligned)
            globalSize: global space size (bytes, must be 4096-byte aligned)
            stackSize: stack size (bytes)
            staticCheck: enable or disable static checks (bool)
            sourceInfo: enable or disable source information (bool). Disable
                this if the program is big.

            vmLog: log level for the micro VM
            gcLog: log level fof the garbage collector
            
        possible values for log levels (strings, case insensitive):
            ALL, TRACE, DEBUG, INFO, WARN, ERROR, OFF

        Setting to WARN will disable almost all logs. Set vmLog to INFO to see
        the execution of each instruction; Set gcLog to DEBUG to see GC logs.
        """
        conf = "".join("{}={}\n".format(k,to_vmconf_str(v))
                for k,v in kwargs.items())
        ptr = self.dll.mu_refimpl2_new_ex(_priv._encode(conf, "utf8"))
        return MuVM(ptr, self)

    def mu_refimpl2_close(self, muvm):
        """Close a MuVM instance. Currently does nothing, i.e. MuVM are never
        really closed."""
        self.dll.mu_refimpl2_close(muvm._struct_ptr)

def to_vmconf_str(obj):
    if isinstance(obj, bool):
        return "true" if obj else "false"
    return str(obj)

# vim: ts=4 sw=4 et sts=4 ai tw=80
