## Print the multiplcation table given two numbers A,B
##   $t0 - A
##   $t1 - B
##   $t2 - intermediate product
##   $t3 - Sential (AxB)
##   $t4 - Counter
    .text
main:
    #Read A
    li      $v0, 5  
    syscall
    move    $t0, $v0

    #Read B
    li      $v0, 5  
    syscall
    move    $t1, $v0

    #Exit if B is zero
    blez    $t1, exit

    # S = A X B
    mul     $t2, $t0, $t1 
    move    $t3, $t2

    # Initial product A x 1 = A  
    move    $t2, $t0
    # Initial counter = 1
    li      $t4, 1


loop:
    #Print A
    move    $a0, $t0
    li      $v0, 1  
    syscall

    #Print x
    la      $a0, cross
    li      $v0, 4  
    syscall

    #Print counter
    move    $a0, $t4
    li      $v0, 1  
    syscall

    #Print equal
    la      $a0, equal
    li      $v0, 4  
    syscall

    #Print product
    move    $a0, $t2
    li      $v0, 1  
    syscall

    #Print newline
    la      $a0, space
    li      $v0, 4  
    syscall

    # Stop if product = A X B
    beq     $t2, $t3, exit
    # Else product = product + A & increment counter
    add     $t2, $t2, $t0
    add     $t4, 1


    b       loop

exit:
    #Exit
    li      $v0, 10
    syscall

    .data
space:      .asciiz " \n"
equal:      .asciiz " = "
cross:      .asciiz " x "
