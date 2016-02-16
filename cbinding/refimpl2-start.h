#ifndef __REFIMPL2_START_H__
#define __REFIMPL2_START_H__

#include <stdint.h>

#include "muapi.h"

#ifdef __cplusplus
extern "C" {
#endif

MuVM *mu_refimpl2_new();
MuVM *mu_refimpl2_new_ex(int64_t heap_size, int64_t global_size, int64_t stack_size);

void mu_refimpl2_close(MuVM *mvm);

#ifdef __cplusplus
}
#endif

#endif // __REFIMPL2_START_H__
