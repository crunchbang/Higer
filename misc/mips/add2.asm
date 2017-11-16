main:
    #Read 1st number
    li      $v0, 5  
    syscall
    move    $t1, $v0

    #Read 2nd number
    li      $v0, 5  
    syscall
    move    $t2, $v0

    #Add the numbers
    add     $t0, $t1, $t2

    #Print the sum
    move    $a0, $t0
    li      $v0, 1  
    syscall

    #Exit
    li    $v0, 10
    syscall


