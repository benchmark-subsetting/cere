; ModuleID = 'c_print_results.c'
target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64--linux-gnu"

module asm "\09.ident\09\22GCC: (Debian 4.7.3-4) 4.7.3 LLVM: 3.3\22"

@.cst = linker_private unnamed_addr constant [27 x i8] c"\0A\0A %s Benchmark Completed\0A\00", align 8
@.cst1 = linker_private unnamed_addr constant [46 x i8] c" Class           =                        %c\0A\00", align 64
@.cst2 = linker_private unnamed_addr constant [37 x i8] c" Size            =             %12d\0A\00", align 64
@.cst3 = linker_private unnamed_addr constant [47 x i8] c" Size            =              %3dx %3dx %3d\0A\00", align 64
@.cst4 = linker_private unnamed_addr constant [37 x i8] c" Iterations      =             %12d\0A\00", align 64
@.cst5 = linker_private unnamed_addr constant [39 x i8] c" Time in seconds =             %12.2f\0A\00", align 64
@.cst6 = linker_private unnamed_addr constant [39 x i8] c" Mop/s total     =             %12.2f\0A\00", align 64
@.cst7 = linker_private unnamed_addr constant [25 x i8] c" Operation type  = %24s\0A\00", align 8
@.cst8 = linker_private unnamed_addr constant [44 x i8] c" Verification    =               SUCCESSFUL\00", align 64
@.cst9 = linker_private unnamed_addr constant [44 x i8] c" Verification    =             UNSUCCESSFUL\00", align 64
@.cst10 = linker_private unnamed_addr constant [37 x i8] c" Version         =             %12s\0A\00", align 64
@.cst11 = linker_private unnamed_addr constant [37 x i8] c" Compile date    =             %12s\0A\00", align 64
@.cst12 = linker_private unnamed_addr constant [19 x i8] c"\0A Compile options:\00", align 8
@.cst13 = linker_private unnamed_addr constant [23 x i8] c"    CC           = %s\0A\00", align 8
@.cst14 = linker_private unnamed_addr constant [23 x i8] c"    CLINK        = %s\0A\00", align 8
@.cst15 = linker_private unnamed_addr constant [23 x i8] c"    C_LIB        = %s\0A\00", align 8
@.cst16 = linker_private unnamed_addr constant [23 x i8] c"    C_INC        = %s\0A\00", align 8
@.cst17 = linker_private unnamed_addr constant [23 x i8] c"    CFLAGS       = %s\0A\00", align 8
@.cst18 = linker_private unnamed_addr constant [23 x i8] c"    CLINKFLAGS   = %s\0A\00", align 8
@.cst19 = linker_private unnamed_addr constant [2 x i8] c"\0A\00", align 8
@.cst20 = linker_private unnamed_addr constant [39 x i8] c" Please send all errors/feedbacks to:\0A\00", align 64
@.cst21 = linker_private unnamed_addr constant [22 x i8] c" NPB Development Team\00", align 8
@.cst22 = linker_private unnamed_addr constant [19 x i8] c" npb@nas.nasa.gov\0A\00", align 8

; Function Attrs: nounwind uwtable
define void @c_print_results(i8* %name, i8 signext %class, i32 %n1, i32 %n2, i32 %n3, i32 %niter, double %t, double %mops, i8* %optype, i32 %passed_verification, i8* %npbversion, i8* %compiletime, i8* %cc, i8* %clink, i8* %c_lib, i8* %c_inc, i8* %cflags, i8* %clinkflags) unnamed_addr #0 {
entry:
  %0 = tail call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([27 x i8]* @.cst, i64 0, i64 0), i8* %name) #1
  %1 = sext i8 %class to i32
  %2 = tail call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([46 x i8]* @.cst1, i64 0, i64 0), i32 %1) #1
  %3 = or i32 %n3, %n2
  %4 = icmp eq i32 %3, 0
  br i1 %4, label %"3", label %"4"

"3":                                              ; preds = %entry
  %5 = tail call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([37 x i8]* @.cst2, i64 0, i64 0), i32 %n1) #1
  br label %"5"

"4":                                              ; preds = %entry
  %6 = tail call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([47 x i8]* @.cst3, i64 0, i64 0), i32 %n1, i32 %n2, i32 %n3) #1
  br label %"5"

"5":                                              ; preds = %"4", %"3"
  %7 = tail call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([37 x i8]* @.cst4, i64 0, i64 0), i32 %niter) #1
  %8 = tail call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([39 x i8]* @.cst5, i64 0, i64 0), double %t) #1
  %9 = tail call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([39 x i8]* @.cst6, i64 0, i64 0), double %mops) #1
  %10 = tail call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([25 x i8]* @.cst7, i64 0, i64 0), i8* %optype) #1
  %11 = icmp eq i32 %passed_verification, 0
  br i1 %11, label %"7", label %"6"

"6":                                              ; preds = %"5"
  %12 = tail call i32 @puts(i8* getelementptr inbounds ([44 x i8]* @.cst8, i64 0, i64 0)) #1
  br label %"8"

"7":                                              ; preds = %"5"
  %13 = tail call i32 @puts(i8* getelementptr inbounds ([44 x i8]* @.cst9, i64 0, i64 0)) #1
  br label %"8"

"8":                                              ; preds = %"7", %"6"
  %14 = tail call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([37 x i8]* @.cst10, i64 0, i64 0), i8* %npbversion) #1
  %15 = tail call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([37 x i8]* @.cst11, i64 0, i64 0), i8* %compiletime) #1
  %16 = tail call i32 @puts(i8* getelementptr inbounds ([19 x i8]* @.cst12, i64 0, i64 0)) #1
  %17 = tail call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([23 x i8]* @.cst13, i64 0, i64 0), i8* %cc) #1
  %18 = tail call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([23 x i8]* @.cst14, i64 0, i64 0), i8* %clink) #1
  %19 = tail call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([23 x i8]* @.cst15, i64 0, i64 0), i8* %c_lib) #1
  %20 = tail call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([23 x i8]* @.cst16, i64 0, i64 0), i8* %c_inc) #1
  %21 = tail call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([23 x i8]* @.cst17, i64 0, i64 0), i8* %cflags) #1
  %22 = tail call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([23 x i8]* @.cst18, i64 0, i64 0), i8* %clinkflags) #1
  %23 = tail call i32 @puts(i8* getelementptr inbounds ([2 x i8]* @.cst19, i64 0, i64 0)) #1
  %24 = tail call i32 @puts(i8* getelementptr inbounds ([39 x i8]* @.cst20, i64 0, i64 0)) #1
  %25 = tail call i32 @puts(i8* getelementptr inbounds ([22 x i8]* @.cst21, i64 0, i64 0)) #1
  %26 = tail call i32 @puts(i8* getelementptr inbounds ([19 x i8]* @.cst22, i64 0, i64 0)) #1
  ret void
}

; Function Attrs: nounwind
declare i32 @printf(i8* nocapture, ...) #1

; Function Attrs: nounwind
declare i32 @puts(i8* nocapture) #1

attributes #0 = { nounwind uwtable }
attributes #1 = { nounwind }
