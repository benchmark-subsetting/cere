; ModuleID = 'c_timers.c'
target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64--linux-gnu"

module asm "\09.ident\09\22GCC: (Debian 4.7.3-4) 4.7.3 LLVM: 3.3\22"

@elapsed = common global [64 x double] zeroinitializer, align 32
@start = common global [64 x double] zeroinitializer, align 32

; Function Attrs: nounwind uwtable
define double @elapsed_time() #0 {
entry:
  %t = alloca double, align 8
  call void @wtime_(double* %t) #1
  %0 = load double* %t, align 8, !tbaa !0
  %1 = bitcast double* %t to i8*
  call void @llvm.lifetime.end(i64 8, i8* %1)
  ret double %0
}

declare void @wtime_(double*)

; Function Attrs: nounwind
declare void @llvm.lifetime.end(i64, i8* nocapture) #1

; Function Attrs: nounwind uwtable
define void @timer_clear(i32 %n) unnamed_addr #0 {
entry:
  %0 = sext i32 %n to i64
  %1 = getelementptr inbounds [64 x double]* @elapsed, i64 0, i64 %0
  store double 0.000000e+00, double* %1, align 8, !tbaa !0
  ret void
}

; Function Attrs: nounwind uwtable
define void @timer_start(i32 %n) unnamed_addr #0 {
entry:
  %t.i = alloca double, align 8
  call void @wtime_(double* %t.i) #1
  %0 = load double* %t.i, align 8, !tbaa !0
  %1 = bitcast double* %t.i to i8*
  call void @llvm.lifetime.end(i64 8, i8* %1) #1
  %2 = sext i32 %n to i64
  %3 = getelementptr inbounds [64 x double]* @start, i64 0, i64 %2
  store double %0, double* %3, align 8, !tbaa !0
  ret void
}

; Function Attrs: nounwind uwtable
define void @timer_stop(i32 %n) unnamed_addr #0 {
entry:
  %t.i = alloca double, align 8
  call void @wtime_(double* %t.i) #1
  %0 = load double* %t.i, align 8, !tbaa !0
  %1 = bitcast double* %t.i to i8*
  call void @llvm.lifetime.end(i64 8, i8* %1) #1
  %2 = sext i32 %n to i64
  %3 = getelementptr inbounds [64 x double]* @start, i64 0, i64 %2
  %4 = load double* %3, align 8, !tbaa !0
  %5 = fsub double %0, %4
  %6 = getelementptr inbounds [64 x double]* @elapsed, i64 0, i64 %2
  %7 = load double* %6, align 8, !tbaa !0
  %8 = fadd double %7, %5
  store double %8, double* %6, align 8, !tbaa !0
  ret void
}

; Function Attrs: nounwind readonly uwtable
define double @timer_read(i32 %n) unnamed_addr #2 {
entry:
  %0 = sext i32 %n to i64
  %1 = getelementptr inbounds [64 x double]* @elapsed, i64 0, i64 %0
  %2 = load double* %1, align 8, !tbaa !0
  ret double %2
}

attributes #0 = { nounwind uwtable }
attributes #1 = { nounwind }
attributes #2 = { nounwind readonly uwtable }

!0 = metadata !{metadata !"alias set 7: double", metadata !1}
!1 = metadata !{metadata !1}
