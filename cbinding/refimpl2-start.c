#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <jni.h>

#include "refimpl2-start.h"
#include "classpath.h"

static JavaVM *jvm;
static JNIEnv *env;
static jclass cinitiater_cls;
static jmethodID new_mid;
static jmethodID new_ex_mid;
static jmethodID close_mid;

static char *cinitiater_class = "uvm/refimpl/nat/CInitiater";
static char *new_method = "mu_refimpl2_new";
static char *new_ex_method = "mu_refimpl2_new_ex";
static char *close_method = "mu_refimpl2_close";

static int refimpl2_start_debug;

static void init_jvm() {
    char *debug_env = getenv("REFIMPL2_START_DEBUG");

    if (debug_env != NULL) {
        refimpl2_start_debug = 1;
    }

    JavaVMInitArgs vm_args;

    JavaVMOption options[1];

    char *cpoptionstr = (char*)calloc(classpath_txt_len + 100, 1);
    strcat(cpoptionstr, "-Djava.class.path=");
    strncat(cpoptionstr, (const char*)classpath_txt, classpath_txt_len);
    options[0].optionString = cpoptionstr;

    if (refimpl2_start_debug) {
        printf("Classpath option: '%s'\n", cpoptionstr);
    }

    vm_args.version = JNI_VERSION_1_8;
    vm_args.nOptions = 1;
    vm_args.options = options;
    vm_args.ignoreUnrecognized = JNI_FALSE;

    int rv = JNI_CreateJavaVM(&jvm, (void**)&env, &vm_args);

    if (rv != JNI_OK) {
        printf("ERROR: Failed to create JVM: %d\n", rv);
        exit(1);
    }

    free(cpoptionstr);

    jclass cinitiater_cls = (*env)->FindClass(env, cinitiater_class);

    if (cinitiater_cls == NULL) {
        printf("ERROR: class %s cannot be found.\n", cinitiater_class);
        exit(1);
    }

    new_mid = (*env)->GetStaticMethodID(env, cinitiater_cls, new_method, "()J");
    if (new_mid == NULL) {
        printf("ERROR: method %s cannot be found.\n", new_method);
        exit(1);
    }

    new_ex_mid = (*env)->GetStaticMethodID(env, cinitiater_cls, new_ex_method, "(Ljava/lang/String;)J");

    if (new_ex_mid == NULL) {
        printf("ERROR: method %s cannot be found.\n", new_ex_method);
        exit(1);
    }

    close_mid = (*env)->GetStaticMethodID(env, cinitiater_cls, close_method, "(J)V");

    if (close_mid == NULL) {
        printf("ERROR: method %s cannot be found.\n", close_method);
        exit(1);
    }

}

MuVM *mu_refimpl2_new() {
    if (jvm == NULL) {
        init_jvm();
    }

    uintptr_t rv = (*env)->CallStaticLongMethod(env, cinitiater_cls, new_mid);

    return (MuVM*)rv;
}

MuVM *mu_refimpl2_new_ex(const char *gc_conf) {
    if (jvm == NULL) {
        init_jvm();
    }

    jstring conf_str = (*env)->NewStringUTF(env, gc_conf);

    if (conf_str == NULL) {
        printf("ERROR: Cannot convert gc_conf to Java String\n");
        exit(1);
    }

    uintptr_t rv = (*env)->CallStaticLongMethod(env, cinitiater_cls, new_ex_mid,
            conf_str);

    (*env)->DeleteLocalRef(env, conf_str);

    return (MuVM*)rv;
}

void mu_refimpl2_close(MuVM *mvm) {
    if (jvm == NULL) {
        init_jvm();
    }

    (*env)->CallStaticVoidMethod(env, cinitiater_cls, close_mid, mvm);
}
