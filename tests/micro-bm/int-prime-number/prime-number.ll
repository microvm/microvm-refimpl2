; ModuleID = 'prime-number.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.7.0"

@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

; Function Attrs: nounwind readnone ssp uwtable
define i32 @isPrime(i64 %a) #0 {
entry:
  %cmp5 = icmp sgt i64 %a, 2
  br i1 %cmp5, label %for.body, label %return

for.cond:                                         ; preds = %for.body
  %cmp = icmp slt i64 %inc, %a
  br i1 %cmp, label %for.body, label %return

for.body:                                         ; preds = %entry, %for.cond
  %i.06 = phi i64 [ %inc, %for.cond ], [ 2, %entry ]
  %rem = srem i64 %a, %i.06
  %cmp1 = icmp eq i64 %rem, 0
  %inc = add nsw i64 %i.06, 1
  br i1 %cmp1, label %return, label %for.cond

return:                                           ; preds = %for.cond, %for.body, %entry
  %retval.0 = phi i32 [ 1, %entry ], [ 1, %for.cond ], [ 0, %for.body ]
  ret i32 %retval.0
}

; Function Attrs: nounwind ssp uwtable
define i32 @main() #1 {
entry:
  br label %for.body.i

for.cond.i:                                       ; preds = %for.body.i
  %cmp.i = icmp slt i64 %inc.i, 1000000000000000001
  br i1 %cmp.i, label %for.body.i, label %isPrime.exit

for.body.i:                                       ; preds = %for.cond.i, %entry
  %i.06.i = phi i64 [ %inc.i, %for.cond.i ], [ 2, %entry ]
  %rem.i = srem i64 1000000000000000001, %i.06.i
  %cmp1.i = icmp eq i64 %rem.i, 0
  %inc.i = add nsw i64 %i.06.i, 1
  br i1 %cmp1.i, label %isPrime.exit, label %for.cond.i

isPrime.exit:                                     ; preds = %for.cond.i, %for.body.i
  %retval.0.i = phi i32 [ 0, %for.body.i ], [ 1, %for.cond.i ]
  %call1 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i64 0, i64 0), i32 %retval.0.i) #3
  ret i32 0
}

; Function Attrs: nounwind
declare i32 @printf(i8* nocapture readonly, ...) #2

attributes #0 = { nounwind readnone ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4 (tags/RELEASE_34/final)"}
