.text

main:

.data
a_var: .word 199
.text

.data
b_var: .word 0
.text
lw $t0, a_var
li $t1, 1
add $t0, $t0, $t1
sw $t0, b_var

li $v0, 10
syscall

