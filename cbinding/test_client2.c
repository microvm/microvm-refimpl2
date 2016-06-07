#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>     // write

#include <refimpl2-start.h>
#include <muapi.h>

const char *hw_string = "Hello world!\n";
const char *hw2_string = "Goodbye world!\n";

const char *gc_conf =
"sosSize=524288\n"
"losSize=524288\n"
"globalSize=1048576\n"
"stackSize=32768\n"
"vmLog=DEBUG\n"
;

int main() {
    MuVM *mvm = mu_refimpl2_new_ex(gc_conf);

    //MuCtx *ctx = mvm->new_context(mvm);
    //mvm->execute(mvm);


    mu_refimpl2_close(mvm);

    return 0;
}
