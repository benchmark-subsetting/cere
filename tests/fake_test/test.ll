; ModuleID = 'test.ll'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i8*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type { %struct._IO_marker*, %struct._IO_FILE*, i32 }

@stderr = external global %struct._IO_FILE*
@.str = private unnamed_addr constant [11 x i8] c"Size = %d\0A\00", align 1
@.str1 = private unnamed_addr constant [15 x i8] c"Checksum : %d\0A\00", align 1
@.str2 = private constant [23 x i8] c"__extracted__checksum_\00", align 1
@.str3 = private constant [19 x i8] c"__extracted__init_\00", align 1

; Function Attrs: nounwind uwtable
define i32 @checksum(i32* %a, i32 %size) #0 {
  %1 = alloca i32*, align 8
  %2 = alloca i32, align 4
  %check = alloca i32, align 4
  %i = alloca i32, align 4
  store i32* %a, i32** %1, align 8
  store i32 %size, i32* %2, align 4
  store i32 0, i32* %check, align 4
  store i32 0, i32* %i, align 4
  br label %codeRepl

codeRepl:                                         ; preds = %0
  call void @__extracted__checksum_(i32* %i, i32* %2, i32** %1, i32* %check)
  br label %3

; <label>:3                                       ; preds = %codeRepl
  %4 = load i32* %check, align 4
  ret i32 %4
}

declare i32 @fprintf(%struct._IO_FILE*, i8*, ...) #1

; Function Attrs: nounwind uwtable
define void @init(i32* %a, i32 %size) #0 {
  %1 = alloca i32*, align 8
  %2 = alloca i32, align 4
  %i = alloca i32, align 4
  store i32* %a, i32** %1, align 8
  store i32 %size, i32* %2, align 4
  store i32 0, i32* %i, align 4
  br label %codeRepl

codeRepl:                                         ; preds = %0
  call void @__extracted__init_(i32* %i, i32* %2, i32** %1)
  br label %3

; <label>:3                                       ; preds = %codeRepl
  ret void
}

; Function Attrs: nounwind uwtable
define i32 @main(i32 %argc, i8** %argv) #0 {
  call void @run__extracted__init_()
  call void @run__extracted__checksum_()
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i8**, align 8
  %size = alloca i32, align 4
  %4 = alloca i8*
  %check = alloca i32, align 4
  %5 = alloca i32
  store i32 0, i32* %1
  store i32 %argc, i32* %2, align 4
  store i8** %argv, i8*** %3, align 8
  store i32 512, i32* %size, align 4
  %6 = load i32* %size, align 4
  %7 = zext i32 %6 to i64
  %8 = call i8* @llvm.stacksave()
  store i8* %8, i8** %4
  %9 = alloca i32, i64 %7, align 16
  %10 = load i32* %size, align 4
  call void @init(i32* %9, i32 %10)
  %11 = load i32* %size, align 4
  %12 = call i32 @checksum(i32* %9, i32 %11)
  store i32 %12, i32* %check, align 4
  %13 = load %struct._IO_FILE** @stderr, align 8
  %14 = load i32* %check, align 4
  %15 = call i32 (%struct._IO_FILE*, i8*, ...)* @fprintf(%struct._IO_FILE* %13, i8* getelementptr inbounds ([15 x i8]* @.str1, i32 0, i32 0), i32 %14)
  store i32 0, i32* %1
  store i32 1, i32* %5
  %16 = load i8** %4
  call void @llvm.stackrestore(i8* %16)
  %17 = load i32* %1
  ret i32 %17
}

; Function Attrs: nounwind
declare i8* @llvm.stacksave() #2

; Function Attrs: nounwind
declare void @llvm.stackrestore(i8*) #2

; Function Attrs: nounwind
define internal void @__extracted__checksum_(i32* %i, i32*, i32**, i32* %check) #2 {
newFuncRoot:
  br label %2

.exitStub:                                        ; preds = %2
  ret void

; <label>:2                                       ; preds = %17, %newFuncRoot
  %3 = load i32* %i, align 4
  %4 = load i32* %0, align 4
  %5 = icmp slt i32 %3, %4
  br i1 %5, label %6, label %.exitStub

; <label>:6                                       ; preds = %2
  %7 = load %struct._IO_FILE** @stderr, align 8
  %8 = load i32* %0, align 4
  %9 = call i32 (%struct._IO_FILE*, i8*, ...)* @fprintf(%struct._IO_FILE* %7, i8* getelementptr inbounds ([11 x i8]* @.str, i32 0, i32 0), i32 %8)
  %10 = load i32* %i, align 4
  %11 = sext i32 %10 to i64
  %12 = load i32** %1, align 8
  %13 = getelementptr inbounds i32* %12, i64 %11
  %14 = load i32* %13, align 4
  %15 = load i32* %check, align 4
  %16 = add nsw i32 %15, %14
  store i32 %16, i32* %check, align 4
  br label %17

; <label>:17                                      ; preds = %6
  %18 = load i32* %i, align 4
  %19 = add nsw i32 %18, 1
  store i32 %19, i32* %i, align 4
  br label %2
}

; Function Attrs: nounwind
define internal void @__extracted__init_(i32* %i, i32*, i32**) #2 {
newFuncRoot:
  br label %2

.exitStub:                                        ; preds = %2
  ret void

; <label>:2                                       ; preds = %13, %newFuncRoot
  %3 = load i32* %i, align 4
  %4 = load i32* %0, align 4
  %5 = icmp slt i32 %3, %4
  br i1 %5, label %6, label %.exitStub

; <label>:6                                       ; preds = %2
  %7 = load i32* %i, align 4
  %8 = add nsw i32 %7, 1
  %9 = load i32* %i, align 4
  %10 = sext i32 %9 to i64
  %11 = load i32** %1, align 8
  %12 = getelementptr inbounds i32* %11, i64 %10
  store i32 %8, i32* %12, align 4
  br label %13

; <label>:13                                      ; preds = %6
  %14 = load i32* %i, align 4
  %15 = add nsw i32 %14, 1
  store i32 %15, i32* %i, align 4
  br label %2
}

define void @run__extracted__checksum_() {
entry:
  %0 = zext i32 4 to i64
  %vla = alloca i8*, i64 %0
  call void @load(i8* getelementptr inbounds ([23 x i8]* @.str2, i32 0, i32 0), i32 4, i8** %vla)
  %arrayidx = getelementptr i8** %vla, i32 0
  %1 = load i8** %arrayidx
  %i = bitcast i8* %1 to i32*
  %arrayidx1 = getelementptr i8** %vla, i32 1
  %2 = load i8** %arrayidx1
  %3 = bitcast i8* %2 to i32*
  %arrayidx2 = getelementptr i8** %vla, i32 2
  %4 = load i8** %arrayidx2
  %5 = bitcast i8* %4 to i32**
  %arrayidx3 = getelementptr i8** %vla, i32 3
  %6 = load i8** %arrayidx3
  %check = bitcast i8* %6 to i32*
  call void @__extracted__checksum_(i32* %i, i32* %3, i32** %5, i32* %check)
  ret void
}

declare void @load(i8*, i32, i8**)

define void @run__extracted__init_() {
entry:
  %0 = zext i32 3 to i64
  %vla = alloca i8*, i64 %0
  call void @load(i8* getelementptr inbounds ([19 x i8]* @.str3, i32 0, i32 0), i32 3, i8** %vla)
  %arrayidx = getelementptr i8** %vla, i32 0
  %1 = load i8** %arrayidx
  %i = bitcast i8* %1 to i32*
  %arrayidx1 = getelementptr i8** %vla, i32 1
  %2 = load i8** %arrayidx1
  %3 = bitcast i8* %2 to i32*
  %arrayidx2 = getelementptr i8** %vla, i32 2
  %4 = load i8** %arrayidx2
  %5 = bitcast i8* %4 to i32**
  call void @__extracted__init_(i32* %i, i32* %3, i32** %5)
  ret void
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf"="true" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf"="true" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }
