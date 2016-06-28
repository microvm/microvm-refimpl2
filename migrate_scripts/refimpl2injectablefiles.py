import injecttools
import os.path

_my_dir = os.path.dirname(__file__)
_refimpl2_root = os.path.join(_my_dir, "..")

def _make_injectable_file_set(m):
    m2 = {os.path.join(_refimpl2_root, k): v for k,v in m.items()}
    return InjectableFileSet(m2)

injectable_files = injecttools.make_injectable_file_set(_refimpl2_root, [
    ("comminsts.scala", "src/main/scala/uvm/comminsts/comminsts.scala",
        ["IRBUILDER_COMMINSTS"]),
    ("internals.scala", "src/main/scala/uvm/refimpl/internals.scala",
        ["IRBUILDER_RETVALS"]),
    ("ir-ci-exec", "src/main/scala/uvm/refimpl/itpr/IRBuilderCommInstExecutor.scala",
        ["IRBUILDER_IMPL"]),
    ])
