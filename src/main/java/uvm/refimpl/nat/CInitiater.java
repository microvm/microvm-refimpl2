package uvm.refimpl.nat;

/** Static functions for the convenient of C programs that start Mu via JNI. */
public class CInitiater {
    
    /** Called by the native program, this function creates a Mu instance. */
    static long mu_refimpl2_new() {
        return ScalaCInitiater$.MODULE$.mu_refimpl2_new();
    }

    /**
     * Called by the native program, this function creates a Mu instance with
     * extra arguments.
     */
    static long mu_refimpl2_new_ex(String vmConf) {
        return ScalaCInitiater$.MODULE$.mu_refimpl2_new_ex(vmConf);
    }

    /**
     * Called by the native program, this function closes and deallocates a Mu
     * instance.
     */
    static void mu_refimpl2_close(long mvmFak) {
        ScalaCInitiater$.MODULE$.mu_refimpl_close(mvmFak);
    }
}
