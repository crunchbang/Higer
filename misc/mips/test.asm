.globl main

.text
main:
li  $t1, 1
add $t0, $t1, 2

move $a0, $t0
li $v0, 1
syscall

li $v0, 10
syscall

#end of asm
