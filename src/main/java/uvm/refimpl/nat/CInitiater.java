package uvm.refimpl.nat;

import java.io.ByteArrayOutputStream;

import org.slf4j.LoggerFactory;

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.encoder.PatternLayoutEncoder;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.ConsoleAppender;
import uvm.refimpl.MicroVM;
import uvm.refimpl.MicroVM$;

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
