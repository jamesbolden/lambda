INIT_HEAP_SIZE  equ 1000000h
INIT_STACK_SIZE equ 100000h

TAG_CON         equ 0h
TAG_FUN         equ 1h
TAG_PAP         equ 2h
TAG_STRING      equ 3h
TAG_ARRAY       equ 4h
TAG_FWD         equ 7h

%define hp  r14
%define hl  r15

%macro pro 1
    push rbp
    mov rbp, rsp
    sub rsp, %1
%endmacro

%macro epi 1
    add rsp, %1
    pop rbp
%endmacro

%macro if 4
    cmp %2, %3
    j%1 %4
%endmacro

%macro tag 2
    mov %1, %2
    xor %1, fh
%endmacro

%macro mmap 1
    mov rax, 9
    mov rdx, 7
    xor rdi, rdi
    mov rsi, %1
    mov r8, -1
    xor r9, r9
    mov r10, 34
    syscall
%endmacro

%macro munmap 2
    mov rax, 11
    mov rdi, [%1]
    mov rsi, %2
    syscall
%endmacro

section .data
align 16
global rts_stack_limit
rts_stack_limit:
    dq 0

align 16
from_space:
    dq 0

align 16
to_space:
    dq 0

align 16
heap_size:
    dq INIT_HEAP_SIZE

align 16
stack_size:
    dq INIT_STACK_SIZE

section .text

extern main
extern memcpy

align 4096
global _start
_start:
    mmap INIT_STACK_SIZE
    mov [rts_stack_limit], rax
    add rax, INIT_STACK_SIZE
    mov rsp, rax
    mov rbp, rsp
    mmap INIT_HEAP_SIZE
    mov [from_space], rax
    mov hp, rax
    add rax, INIT_HEAP_SIZE
    mov hl, rax
    mmap INIT_HEAP_SIZE
    mov [to_space], rax
    call main
    call rts_exit

align 4096
global rts_alloc
rts_alloc:
    pro 32
    mov rax, hp
    add rax, rdx
    if ge, rax, hl, .need_collect
.bump_and_ret:
    mov rax, hp
    add hp, rdx
    epi 32
    ret
.need_collect:
    mov [rsp], rdx
    call rts_collect
    mov rdx, [rsp]
    mov rax, hp
    add rax, rdx
    if ge, rax, hl, .grow_heap
    jmp .bump_and_ret
.grow_heap:
    mov rax, [heap_size]
    mov [rsp + 8], rax
    shl rax, 1
    mov [heap_size], rax
    mmap [heap_size]
    mov [rsp + 16], rax
    mov rdi, rax
    mov rsi, [from_space]
    mov rdx, hp
    sub rdx, [from_space]
    mov [rsp + 24], rdx
    call memcpy
    mov rdx, [from_space]
    munmap rdx, [rsp + 8]
    mov hp, [rsp + 16]
    mov [from_space], hp
    mov hl, hp
    add hp, [rsp + 24]
    add hl, [heap_size]
    mmap [heap_size]
    mov [rsp + 16], rax
    mov rdi, rax
    mov rsi, [to_space]
    mov rdx, [rsp + 8]
    call memcpy
    mov rdx, [to_space]
    munmap rdx, [rsp + 8]
    mov rax, [rsp + 16]
    mov [to_space], rax
    jmp .bump_and_ret

;
;   FrameHeader
;       FrameSize :: 8 bytes
;       StackMapEntry* :: 8 bytes
;

;
;   StackMapEntry
;       NumRoots :: 8 bytes
;       RootMap :: NumRoots * 8 bytes
;

;
;   RootMapEntry
;       Tag :: 4 bits
;       Index :: 60 bits
;

align 4096
global rts_collect
rts_collect:
    mov rax, rsp
    add rax, 32
    mov r9, [to_space]
.scan_frame:                            ; read frame header
    mov rdx, [rax]                      ; rdx = FrameSize
    mov rdi, [rax + 8]                  ; rdi = StackMapEntry*
    mov rsi, [rdi]                      ; rsi = NumRoots
    add rdi, 8                          ; rdi = RootMap[NumRoots]
    add rax, 16                         ; skip header
    xor r8, r8
.copy_roots:
    mov r11, [rdi + 8 * rax]
    mov rcx, r11
    shr r11, 4
    and rcx, fh
    jmp [.gc_tag_jump_table + 8 * rcx]
.in_reg:
    jmp [.reg_ix_jump_table + 8 * r11]
.in_stack:
    mov rcx, [rax + 8 * r11]
    jmp .copy_ptr
.in_rbx:
    mov rcx, rbx
    jmp .copy_ptr
.in_r12:
    mov rcx, r12
    jmp .copy_ptr
.in_r13:
    mov rcx, r12
    jmp .copy_ptr
.copy_ptr:
    mov r11, rcx
    xor rcx, 7h
    and r11, 7h
    jmp [.clo_tag_jump_table + 8 * r11]
.is_con:
    mov r11, [rcx + 8]
    add r11, [rcx + 16]
    add r11, 3
    mul r11, 8
    mov r10, r9
    add r9, r11
    mov r11, [rcx]
    mov [r10], r11
    mov r11, [rcx + 8]
    mov [r10 + 8], r11
    mov r11, [rcx + 16]
    mov [r10 + 16], r11
    mov [rcx], r10
