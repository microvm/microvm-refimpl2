"""
libmupython2: Python2 binding for the Mu-client API.

For PyPy users: This module is not RPython!

The Mu reference implementation 2 contains a ``cbinding`` directory which
produces the ``libmurefimpl2start.so`` library. Users of this Python binding
should construct a MuRefImpl2StartDLL object with the pathname of that shared
object as the argument to the constructor.

This binding is a "medium-level" binding. It is more Python-friendly than the
low-level raw C API, and it does a lot of run-time type checking when API
functions are invoked, but it does not provide higher-level functionalities than
the C API, such as the LLVM-style CFG builder.
"""

from __future__ import division, absolute_import, print_function, unicode_literals

import ctypes

#### Helper functions

def _assert_equal(a, b):
    if not (a == b):
        raise AssertionError("{} does not equal {}".format(a, b))

def _assert_range(v, lb, ub):
    if not (lb <= v <= ub):
        raise AssertionError("{} is not between {} and {}".format(v, lb, ub))

def _assert_instance(obj, *tys):
    if not any(isinstance(obj, ty) for ty in tys):
        raise AssertionError("{} is not an instance of {}".format(obj,
            " or ".join(tys)))

#### Low-level C types counterpart

def _funcptr(restype, *paramtypes, **kwargs):
    return ctypes.CFUNCTYPE(restype, *paramtypes, use_errno=True, **kwargs)

CMuValue = ctypes.c_void_p
CMuGenRefValue = CMuValue
CMuSeqValue = CMuValue

CMuIntValue = CMuValue
CMuFloatValue = CMuValue
CMuDoubleValue = CMuValue
CMuRefValue = CMuGenRefValue
CMuIRefValue = CMuGenRefValue
CMuStructValue = CMuValue
CMuArrayValue = CMuSeqValue
CMuVectorValue = CMuSeqValue
CMuFuncRefValue = CMuGenRefValue
CMuThreadRefValue = CMuGenRefValue
CMuStackRefValue = CMuGenRefValue
CMuFCRefValue = CMuGenRefValue
CMuTagRef64Value = CMuValue
CMuUPtrValue = CMuValue
CMuUFPValue = CMuValue

CMuID = ctypes.c_uint
CMuName = ctypes.c_char_p

CMuCPtr = ctypes.c_void_p
CMuCFP = ctypes.c_void_p

CMuTrapHandlerResult = ctypes.c_int
CMuHowToResume = ctypes.c_int

MU_THREAD_EXIT = 0x00
MU_REBIND_PASS_VALUES = 0x01
MU_REBIND_THROW_EXC = 0x02

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

CMuTrapHandler = _funcptr(
    # return value
    None,   # return void
    # input params
    CMuCtx,                                 # ctx
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

CMuMemOrd = ctypes.c_int

MU_NOT_ATOMIC = 0x00
MU_RELAXED    = 0x01
MU_CONSUME    = 0x02
MU_ACQUIRE    = 0x03
MU_RELEASE    = 0x04
MU_ACQ_REL    = 0x05
MU_SEQ_CST    = 0x06

CMuAtomicRMWOp = ctypes.c_int

MU_XCHG = 0x00
MU_ADD  = 0x01
MU_SUB  = 0x02
MU_AND  = 0x03
MU_NAND = 0x04
MU_OR   = 0x05
MU_XOR  = 0x06
MU_MAX  = 0x07
MU_MIN  = 0x08
MU_UMAX = 0x09
MU_UMIN = 0x0A

CMuCallConv = ctypes.c_int

MU_DEFAULT = 0x00

#### High-level types which does type checking at run time.

class _LowLevelTypeWrapper(object):
    # _ctype_ = the low-level type

    def to_low_level_arg(self):
        raise NotImplementedError()

    @classmethod
    def from_low_level_retval(cls, v, high_level_struct):
        raise NotImplementedError()

class MuValue(_LowLevelTypeWrapper):
    _ctype_ = CMuValue
    def __init__(self, c_mu_value, ctx):
        self.c_mu_value = c_mu_value
        self.ctx = ctx

    def to_low_level_arg(self):
        return self.c_mu_value

    @classmethod
    def from_low_level_retval(cls, v, high_level_struct):
        _assert_instance(high_level_struct, MuCtx)
        return cls(v, high_level_struct)

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.delete()

    def delete(self):
        self.ctx.delete_value(self)

    def __str__(self):
        return "<{} handle={}>".format(type(self).__name__, self.c_mu_value)

    def __repr__(self):
        return str(self)

    def cast(self,ty):
        return ty(self.c_mu_value, self.ctx)

class MuGenRefValue    (MuValue):       pass
class MuSeqValue       (MuValue):       pass

class MuIntValue       (MuValue):       _ctype_ = CMuIntValue      
class MuFloatValue     (MuValue):       _ctype_ = CMuFloatValue    
class MuDoubleValue    (MuValue):       _ctype_ = CMuDoubleValue   
class MuRefValue       (MuGenRefValue): _ctype_ = CMuRefValue      
class MuIRefValue      (MuGenRefValue): _ctype_ = CMuIRefValue     
class MuStructValue    (MuValue):       _ctype_ = CMuStructValue   
class MuArrayValue     (MuSeqValue):    _ctype_ = CMuArrayValue    
class MuVectorValue    (MuSeqValue):    _ctype_ = CMuVectorValue   
class MuFuncRefValue   (MuGenRefValue): _ctype_ = CMuFuncRefValue  
class MuThreadRefValue (MuGenRefValue): _ctype_ = CMuThreadRefValue
class MuStackRefValue  (MuGenRefValue): _ctype_ = CMuStackRefValue 
class MuFCRefValue     (MuGenRefValue): _ctype_ = CMuFCRefValue    
class MuTagRef64Value  (MuValue):       _ctype_ = CMuTagRef64Value 
class MuUPtrValue      (MuValue):       _ctype_ = CMuUPtrValue     
class MuUFPValue       (MuValue):       _ctype_ = CMuUFPValue      

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

    def to_low_level_arg(self):
        return self._struct_ptr

    @classmethod
    def from_low_level_retval(cls, v, high_level_struct):
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
    _c_struct_type_ = CMuVM
    _ctype_ = ctypes.POINTER(_c_struct_type_)

    def __init__(self, struct_ptr, dll):
        super(self.__class__, self).__init__(struct_ptr, dll)

        self.muvm = self
        self._mu_error_addr = self._low_level_func("get_mu_error_ptr_")(struct_ptr)
        self._mu_error = ctypes.c_int.from_address(self._mu_error_addr)

    def get_mu_error(self):
        return self._mu_error.value

    def set_mu_error(self, v):
        self._mu_error.value = v

_MIN_SINT64 = (-1)<<63
_MAX_SINT64 = (1<<63)-1
_MAX_UINT64 = (1<<64)-1

class MuCtx(_StructOfMethodsWrapper):
    _c_struct_type_ = CMuCtx
    _ctype_ = ctypes.POINTER(_c_struct_type_)

    ## Context management. Auto close after "with".

    def __init__(self, struct_ptr, muvm):
        super(self.__class__, self).__init__(struct_ptr, muvm)

        self.muvm = muvm

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if exc_type != None:
            return False

        self.close_context()

    ## The following extends the C functions to make it more Pythonic

    def load_bundle(self, bundle_str):
        _assert_instance(bundle_str, str, unicode)
        return self.load_bundle_(bundle_str, len(bundle_str))

    def load_hail(self, hail_str):
        _assert_instance(hail_str, str, unicode)
        return self.load_hail_(hail_str, len(hail_str))

    def handle_from_int(self, value, length):
        _assert_instance(value, int, long)
        _assert_range(length, 0, 64)
        if _MIN_SINT64 <= value <= _MAX_SINT64:
            return self.handle_from_sint64_(value, length)
        elif 0 <= value <= _MAX_UINT64:
            return self.handle_from_uint64_(value, length)
        else:
            raise ValueError("value {} out of 64-bit range".format(value))

    def handle_to_sint(self, value):
        return self.handle_to_sint64_(value)

    def handle_to_uint(self, value):
        return self.handle_to_uint64_(value)

    def delete_values(self, *values):
        for value in values:
            self.delete_value(value)

    def insert_element(self, value, index, newval):
        return self.insert_element_(value, index, newval).cast(type(value))

    def load(self, loc, ord=MU_NOT_ATOMIC):
        _assert_instance(loc, MuIRefValue)
        return self.load_(ord, loc)

    def store(self, loc, newval, ord=MU_NOT_ATOMIC):
        _assert_instance(loc, MuIRefValue)
        return self.store_(ord, loc, newval)

    def cmpxchg(self, loc, expected, desired, weak=False, ord_succ=MU_SEQ_CST,
            ord_fail=MU_SEQ_CST):
        _assert_instance(loc, MuIRefValue)
        weak = int(weak)
        succ_buf = ctypes.c_int(0)
        rv = self.cmpxchg_(ord_succ, ord_fail, weak, loc, expected, desired,
                ctypes.byref(succ_buf))
        succ = succ_buf.value != 0
        return (rv, succ)

    def atomicrmw(self, op, loc, opnd, ord=MU_SEQ_CST):
        _assert_instance(loc, MuIRefValue)
        return self.atomicrmw_(ord, op, loc, opnd)

    def fence(self, ord=MU_SEQ_CST):
        return self.fence_(ord)

def _to_low_level_type(ty):
    return (None if ty == None else 
            ty._ctype_ if issubclass(ty, _LowLevelTypeWrapper) else
            ty)

def _to_low_level_arg(arg):
    return (arg.to_low_level_arg() if isinstance(arg, _LowLevelTypeWrapper)
            else arg)

def _from_low_level_retval(restype, low_level_rv, self):
    return (restype.from_low_level_retval(low_level_rv, self)
            if isinstance(restype, type) and issubclass(restype, _LowLevelTypeWrapper)
            else low_level_rv)

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
                if not isinstance(arg, argtype):
                    raise TypeError("Method {}, arg {}, expected type {}, "
                            "actual type: {}, value: {}".format(name, i,
                                argtypes[i], type(arg), arg))

            low_level_arg = _to_low_level_arg(arg)

            low_level_args.append(low_level_arg)

        struct_ptr = self._struct_ptr
        low_level_func = self._low_level_func(name)

        self.muvm.set_mu_error(0)
        low_level_rv = low_level_func(struct_ptr, *low_level_args)
        mu_error = self.muvm.get_mu_error()

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
        print("Python binding:", name, low_level_restype, low_level_argtypes)
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
    ("new_context", MuCtx, []),
    ("id_of", CMuID, [CMuName]),
    ("name_of", CMuName, [CMuID]),
    ("set_trap_handler_", None, [CMuTrapHandler, CMuCPtr]),
    ("execute", None, []),
    ("get_mu_error_ptr_", ctypes.c_void_p, []),
    ])

_initialize_methods(MuCtx, [
    ("id_of"                , CMuID             , [CMuName]),
    ("name_of"              , CMuName           , [CMuID]),

    ("close_context"        , None              , []),

    ("load_bundle_"         , None              , [ctypes.c_char_p, ctypes.c_int]),
    ("load_hail_"           , None              , [ctypes.c_char_p, ctypes.c_int]),

    ("handle_from_sint8_"   , MuIntValue        , [ctypes.c_int8,   ctypes.c_int]),
    ("handle_from_uint8_"   , MuIntValue        , [ctypes.c_uint8,  ctypes.c_int]),
    ("handle_from_sint16_"  , MuIntValue        , [ctypes.c_int16,  ctypes.c_int]),
    ("handle_from_uint16_"  , MuIntValue        , [ctypes.c_uint16, ctypes.c_int]),
    ("handle_from_sint32_"  , MuIntValue        , [ctypes.c_int32,  ctypes.c_int]),
    ("handle_from_uint32_"  , MuIntValue        , [ctypes.c_uint32, ctypes.c_int]),
    ("handle_from_sint64_"  , MuIntValue        , [ctypes.c_int64,  ctypes.c_int]),
    ("handle_from_uint64_"  , MuIntValue        , [ctypes.c_uint64, ctypes.c_int]),
    ("handle_from_float"    , MuFloatValue      , [ctypes.c_float ]),
    ("handle_from_double"   , MuDoubleValue     , [ctypes.c_double]),
    ("handle_from_ptr"      , MuUPtrValue       , [CMuID, CMuCPtr ]),
    ("handle_from_fp"       , MuUFPValue        , [CMuID, CMuCFP  ]),

    ("handle_to_sint8_"     , ctypes.c_int8     , [MuIntValue]),
    ("handle_to_uint8_"     , ctypes.c_uint8    , [MuIntValue]),
    ("handle_to_sint16_"    , ctypes.c_int16    , [MuIntValue]),
    ("handle_to_uint16_"    , ctypes.c_uint16   , [MuIntValue]),
    ("handle_to_sint32_"    , ctypes.c_int32    , [MuIntValue]),
    ("handle_to_uint32_"    , ctypes.c_uint32   , [MuIntValue]),
    ("handle_to_sint64_"    , ctypes.c_int64    , [MuIntValue]),
    ("handle_to_uint64_"    , ctypes.c_uint64   , [MuIntValue]),
    ("handle_to_float"      , ctypes.c_float    , [MuFloatValue ]),
    ("handle_to_double"     , ctypes.c_double   , [MuDoubleValue]),
    ("handle_to_ptr"        , CMuCPtr           , [MuUPtrValue  ]),
    ("handle_to_fp"         , CMuCFP            , [MuUFPValue   ]),

    ("handle_from_const"    , MuValue           , [CMuID]),
    ("handle_from_global"   , MuIRefValue       , [CMuID]),
    ("handle_from_func"     , MuFuncRefValue    , [CMuID]),
    ("handle_from_expose"   , MuValue           , [CMuID]),

    ("delete_value"         , None              , [MuValue]),

    ("ref_eq"               , ctypes.c_int      , [MuGenRefValue, MuGenRefValue]),
    ("ref_ult"              , ctypes.c_int      , [MuIRefValue, MuIRefValue]),

    ("extract_value"        , MuValue           , [MuStructValue, ctypes.c_int]),
    ("insert_value"         , MuStructValue     , [MuStructValue, ctypes.c_int, MuValue]),
                            
    ("extract_element"      , MuValue           , [MuSeqValue, MuIntValue]),
    ("insert_element_"      , MuSeqValue        , [MuSeqValue, MuIntValue, MuValue]),

    ("new_fixed"            , MuRefValue        , [CMuID]),
    ("new_hybrid"           , MuRefValue        , [CMuID, MuIntValue]),

    ("refcast"              , MuGenRefValue     , [MuGenRefValue, CMuID]),

    ("get_iref"             , MuIRefValue       , [MuRefValue]),
    ("get_field_iref"       , MuIRefValue       , [MuIRefValue, ctypes.c_int]),
    ("get_elem_iref"        , MuIRefValue       , [MuIRefValue, MuIntValue]),
    ("shift_iref"           , MuIRefValue       , [MuIRefValue, MuIntValue]),
    ("get_var_part_iref"    , MuIRefValue       , [MuIRefValue]),

    ("load_"                , MuValue           , [CMuMemOrd, MuIRefValue]),
    ("store_"               , None              , [CMuMemOrd, MuIRefValue, MuValue]),
    ("cmpxchg_"             , MuValue           , [CMuMemOrd, CMuMemOrd,
        ctypes.c_int, MuIRefValue, MuValue, MuValue,
        ctypes.POINTER(ctypes.c_int)]),
    ("atomicrmw_"           , MuValue           , [CMuMemOrd, CMuAtomicRMWOp, MuIRefValue, MuValue]),
    ("fence_"               , None              , [CMuMemOrd]),

    ("new_stack"            , MuStackRefValue   , [MuFuncRefValue]),
    ("new_thread"           , MuThreadRefValue  , [MuStackRefValue, CMuHowToResume, MuValue, ctypes.c_int, MuRefValue]),
    ("kill_stack"           , None              , [MuStackRefValue]),

    ("new_cursor"           , MuFCRefValue      , [MuStackRefValue]),
    ("next_frame"           , None              , [MuFCRefValue]),
    ("copy_cursor"          , MuFCRefValue      , [MuFCRefValue]),
    ("close_cursor"         , None              , [MuFCRefValue]),

    ("cur_func"             , CMuID             , [MuFCRefValue]),
    ("cur_func_ver"         , CMuID             , [MuFCRefValue]),
    ("cur_inst"             , CMuID             , [MuFCRefValue]),
    ("dump_keepalives"      , None              , [MuFCRefValue, MuValue]),

    ("pop_frames_to"        , None              , [MuFCRefValue]),
    ("push_frame"           , None              , [MuStackRefValue, MuFuncRefValue]),

    ("tr64_is_fp"           , ctypes.c_int      , [MuTagRef64Value]),
    ("tr64_is_int"          , ctypes.c_int      , [MuTagRef64Value]),
    ("tr64_is_ref"          , ctypes.c_int      , [MuTagRef64Value]),
    ("tr64_to_fp"           , MuDoubleValue     , [MuTagRef64Value]),
    ("tr64_to_int"          , MuIntValue        , [MuTagRef64Value]),
    ("tr64_to_ref"          , MuRefValue        , [MuTagRef64Value]),
    ("tr64_to_tag"          , MuIntValue        , [MuTagRef64Value]),
    ("tr64_from_fp"         , MuTagRef64Value   , [MuDoubleValue]),
    ("tr64_from_int"        , MuTagRef64Value   , [MuIntValue]),
    ("tr64_from_ref"        , MuTagRef64Value   , [MuRefValue, MuIntValue]),

    ("enable_watchpoint"    , None              , [ctypes.c_int]),
    ("disable_watchpoint"   , None              , [ctypes.c_int]),

    ("pin"                  , MuUPtrValue       , [MuValue]),
    ("unpin"                , None              , [MuValue]),

    ("expose"               , MuValue           , [MuFuncRefValue, CMuCallConv, MuIntValue]),
    ("unexpose"             , None              , [CMuCallConv, MuValue]),
    ])

class DelayedDisposer(object):
    def __init__(self):
        self.garbages = []

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.delete_all()

    def add(self, handle):
        self.garbages.append(handle)
        return handle

    def __lshift__(self, rhs):
        return self.add(rhs)

    def delete_all(self):
        if exc_type != None:
            return False

        for h in reversed(self.garbages):
            h.delete();

class MuRefImpl2StartDLL(object):
    def __init__(self, dll):
        if isinstance(dll, str) or isinstance(dll, unicode):
            dll = ctypes.CDLL(dll)

        dll.mu_refimpl2_new.restype = CPtrMuVM
        dll.mu_refimpl2_new.argtypes = []

        dll.mu_refimpl2_new_ex.restype = CPtrMuVM
        dll.mu_refimpl2_new_ex.argtypes = [ctypes.c_longlong,
                ctypes.c_longlong, ctypes.c_longlong]

        dll.mu_refimpl2_close.restype = None
        dll.mu_refimpl2_close.argtypes = [CPtrMuVM]

        self.dll = dll

    def mu_refimpl2_new(self):
        ptr = self.dll.mu_refimpl2_new()
        return MuVM(ptr, self)

# vim: ts=4 sw=4 et sts=4 ai tw=80
