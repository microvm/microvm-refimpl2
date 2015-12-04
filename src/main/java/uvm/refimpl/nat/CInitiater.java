package uvm.refimpl.nat;

import uvm.refimpl.MicroVM;
import uvm.refimpl.MicroVM$;

/** Static functions for the convenient of C programs that start Mu via JNI. */
public class CInitiater {
    /** Called by the native program, this function creates a Mu instance. */
    static long mu_refimpl2_new() {
        MicroVM mvm = new MicroVM(MicroVM$.MODULE$.DEFAULT_HEAP_SIZE(), MicroVM$.MODULE$.DEFAULT_GLOBAL_SIZE(),
                MicroVM$.MODULE$.DEFAULT_STACK_SIZE());
        long fak = NativeClientSupport$.MODULE$.exposeMicroVM(mvm);
        return fak;
    }

    /**
     * Called by the native program, this function creates a Mu instance with
     * extra arguments.
     */
    static long mu_refimpl2_new_ex(long heap_size, long global_size, long stack_size) {
        MicroVM mvm = new MicroVM(heap_size, global_size, stack_size);
        long fak = NativeClientSupport$.MODULE$.exposeMicroVM(mvm);
        return fak;
    }

    /**
     * Called by the native program, this function closes and deallocates a Mu
     * instance.
     */
    static void mu_refimpl2_close(long mvmFak) {
        NativeClientSupport$.MODULE$.unexposeMicroVM(mvmFak);
        // does not really deallocate it.
    }
}
