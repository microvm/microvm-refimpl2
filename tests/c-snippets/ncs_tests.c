#include <stdio.h>
#include <muapi.h>

void test_basic(MuVM *mvm) {
    printf("[C] header: %p\n", mvm->header);
    printf("[C] new_context: %p\n", mvm->new_context);
    printf("[C] id_of: %p\n", mvm->id_of);
    printf("[C] name_of: %p\n", mvm->name_of);
    printf("[C] set_trap_handler: %p\n", mvm->set_trap_handler);
}

