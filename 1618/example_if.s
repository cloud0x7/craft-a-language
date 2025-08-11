    .section	__TEXT,__text,regular,pure_instructions

    .global _foo
_foo:
    .cfi_startproc
## bb.0
    pushq	%rbp
    movq	%rsp, %rbp
    movl	%edi, -4(%rbp)
    movl	%esi, -8(%rbp)
    movl	-4(%rbp), %eax
    cmpl	-8(%rbp), %eax
    jle	LBB0_2
## bb.1
    movl	-4(%rbp), %r10d
    addl	$5, %r10d
    movl	%r10d, %eax
    jmp	LBB0_3
LBB0_2:
    movl	-4(%rbp), %r11d
    subl	$5, %r11d
    movl	%r11d, %eax
LBB0_3:
    popq	%rbp
    retq
    .cfi_endproc

    .global _main
_main:
    .cfi_startproc
## bb.0
    pushq	%rbp
    movq	%rsp, %rbp
    subq	$32, %rsp
    movl	%eax, -4(%rbp)
    movl	%r10d, -8(%rbp)
    movl	$15, %edi
    movl	$10, %esi
    callq	_foo
    movl	-4(%rbp), %r10d
    movl	-8(%rbp), %r11d
    movl	%eax, %r10d
    movl	%r10d, -4(%rbp)
    movl	%r11d, -8(%rbp)
    movl	%r10d, %edi
    callq	_println
    movl	-4(%rbp), %r10d
    movl	-8(%rbp), %r11d
    movl	%r10d, -4(%rbp)
    movl	%r11d, -8(%rbp)
    movl	$10, %edi
    movl	$10, %esi
    callq	_foo
    movl	-4(%rbp), %r10d
    movl	-8(%rbp), %r11d
    movl	%eax, %r11d
    movl	%r10d, -4(%rbp)
    movl	%r11d, -8(%rbp)
    movl	%r11d, %edi
    callq	_println
    movl	-4(%rbp), %r10d
    movl	-8(%rbp), %r11d
    addq	$32, %rsp
    popq	%rbp
    retq
    .cfi_endproc
