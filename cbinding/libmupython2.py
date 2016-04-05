from __future__ import division, absolute_import, print_function, unicode_literals

import ctypes

def _funcptr(restype, *paramtypes, **kwargs):
    return ctypes.CFUNCTYPE(restype, *paramtypes, **kwargs)

CMuValue = ctypes.c_void_p
CMuIntValue = CMuValue
CMuFloatValue = CMuValue
CMuDoubleValue = CMuValue
CMuRefValue = CMuValue
CMuIRefValue = CMuValue
CMuStructValue = CMuValue
CMuArrayValue = CMuValue
CMuVectorValue = CMuValue
CMuFuncRefValue = CMuValue
CMuThreadRefValue = CMuValue
CMuStackRefValue = CMuValue
CMuFCRefValue = CMuValue
CMuTagRef64Value = CMuValue
CMuUPtrValue = CMuValue
CMuUFPValue = CMuValue

CMuID = ctypes.c_uint
CMuName = ctypes.c_char_p

CMuCPtr = ctypes.c_void_p
CMuCFP = ctypes.c_void_p

CMuTrapHandlerResult = ctypes.c_int
CMuHowToResume = ctypes.c_int

C_MU_THREAD_EXIT = 0x00
C_MU_REBIND_PASS_VALUES = 0x01
C_MU_REBIND_THROW_EXC = 0x02

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

C_MU_NOT_ATOMIC = 0x00
C_MU_RELAXED    = 0x01
C_MU_CONSUME    = 0x02
C_MU_ACQUIRE    = 0x03
C_MU_RELEASE    = 0x04
C_MU_ACQ_REL    = 0x05
C_MU_SEQ_CST    = 0x06

CMuAtomicRMWOp = ctypes.c_int

C_MU_XCHG = 0x00
C_MU_ADD  = 0x01
C_MU_SUB  = 0x02
C_MU_AND  = 0x03
C_MU_NAND = 0x04
C_MU_OR   = 0x05
C_MU_XOR  = 0x06
C_MU_MAX  = 0x07
C_MU_MIN  = 0x08
C_MU_UMAX = 0x09
C_MU_UMIN = 0x0A

CMuCallConv = ctypes.c_int

C_MU_DEFAULT = 0x00

def _fields_of_struct_of_methods(objtype, methods):
    """
    This function constructs the _fields_ field of MuVM, MuCtx and MuBuilder
    structs. All of them starts with a (void*) header and many function pointer,
    all of which take a pointer of this struct as the first argument.

    ``objtype`` is the type of the struct (such as CMuVM, CMuCtx).

    ``methods`` is a list of (name, restype, paramtypes) where paramtypes do
    not include the first argument, which is implicitly
    ctypes.POINTER(objtype).
    """

    fields = [("header", ctypes.c_void_p)]

    objtype_p = ctypes.POINTER(objtype)

    for name, restype, paramtypes in methods:
        funcptr = _funcptr(restype, objtype_p, *paramtypes)
        fields.append((name, funcptr))

    return fields

CMuVM._fields_ = _fields_of_struct_of_methods(CMuVM, [
    ("new_context", CPtrMuCtx, []),
    ("id_of", CMuID, [CMuName]),
    ("name_of", CMuName, [CMuID]),
    ("set_trap_handler", None, [CMuTrapHandler, CMuCPtr]),
    ("execute", None, []),
    ])

CMuCtx._fields_ = _fields_of_struct_of_methods(CMuCtx, [
    ("id_of", CMuID, [CMuName]),
    ("name_of", CMuName, [CMuID]),

    ("close_context", None, []),

    ("load_bundle", None, [ctypes.c_char_p, ctypes.c_int]),
    ("load_hail", None, [ctypes.c_char_p, ctypes.c_int]),

    ("handle_from_sint8",  CMuIntValue, [ctypes.c_byte,      ctypes.c_int]),
    ("handle_from_uint8",  CMuIntValue, [ctypes.c_ubyte,     ctypes.c_int]),
    ("handle_from_sint16", CMuIntValue, [ctypes.c_short,     ctypes.c_int]),
    ("handle_from_uint16", CMuIntValue, [ctypes.c_ushort,    ctypes.c_int]),
    ("handle_from_sint32", CMuIntValue, [ctypes.c_int,       ctypes.c_int]),
    ("handle_from_uint32", CMuIntValue, [ctypes.c_uint,      ctypes.c_int]),
    ("handle_from_sint64", CMuIntValue, [ctypes.c_longlong,  ctypes.c_int]),
    ("handle_from_uint64", CMuIntValue, [ctypes.c_ulonglong, ctypes.c_int]),

    # TODO
    ])

class _StructOfMethodsWrapper(object):
    def __init__(self, _struct_ptr):
        self._struct_ptr = _struct_ptr
        print("_struct_ptr:", _struct_ptr)

    def _low_level_method(self, name):
        struct_ptr = self._struct_ptr
        method = getattr(self._struct_ptr.contents, name)
        def wrapper(*args):
            return method(struct_ptr, *args)
        return wrapper

    def __getattr__(self, name):
        return self._low_level_method(name)

class MuVM(_StructOfMethodsWrapper):
    _struct_type_ = CMuVM

    def new_context(self):
        ptr = self._low_level_method("new_context")()
        return MuCtx(ptr)

class MuCtx(_StructOfMethodsWrapper):
    _struct_type_ = CMuCtx

    def load_bundle_from_str(self, bundle):
        return self.load_bundle(bundle, len(bundle))

    def load_hail_from_str(self, hail):
        return self.load_hail(hail, len(hail))

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
        return MuVM(ptr)
