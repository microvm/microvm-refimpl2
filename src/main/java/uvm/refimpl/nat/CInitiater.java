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
    
    private static boolean isLogConfigured = false;
    
    private static void configureLog() {
        if (isLogConfigured) {
            return;
        }
        isLogConfigured = true;
        
        LoggerContext lc = (LoggerContext) LoggerFactory.getILoggerFactory();
        
        lc.reset();

        ConsoleAppender<ILoggingEvent> ca = new ConsoleAppender<ILoggingEvent>();
        ca.setContext(lc);
        ca.setName("console");
        ca.setTarget("System.err");
        PatternLayoutEncoder pl = new PatternLayoutEncoder();
        pl.setContext(lc);
        pl.setPattern("%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n");
        pl.start();

        ca.setEncoder(pl);
        ca.start();
        Logger rootLogger = lc.getLogger(Logger.ROOT_LOGGER_NAME);
        rootLogger.addAppender(ca);
    }
    
    /** Called by the native program, this function creates a Mu instance. */
    static long mu_refimpl2_new() {
        configureLog();
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
        configureLog();
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
