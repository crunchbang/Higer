## Recursive function to compute factorial
##      factorial:
##          $t0 - intermediate result
##          $a0 - n  (input)
##          $v0 - n! (output)
##

    .data
n:      .word   5
error:  .asciiz "Invalid input\n"  

    .text
main:

# Hardcode input
#    lw      $a0, n

# Get n from user
    li      $v0, 5
    syscall
    move    $a0, $v0 

# Call factorial
    jal     factorial
# Show result
    beqz    $v0, main_error

    move    $a0, $v0
    li      $v0, 1
    syscall
    b       exit
# Error
main_error:
    la      $a0, error
    li      $v0, 4
    syscall
# Exit
exit:
    li      $v0, 10
    syscall

factorial:
# Save args & return address on stack
    sub     $sp, $sp, 4
    sw      $ra, 0($sp)
# Check base condition
    li      $v0, 0
    blt     $a0, 0, return 

    li      $v0, 1
    beq     $a0, 0, return
# Save state
    sub     $sp, $sp, 4
    sw      $a0, 0($sp)
# Compute arg for recursive call
    sub     $a0, $a0, 1
# Call factorial(n-1)
    jal     factorial
# Restore state
    lw      $a0, 0($sp)
    add     $sp, $sp, 4
# Compute factorial(n)
    mul     $v0, $v0, $a0
# Restore return address
return:
    lw      $ra, 0($sp)
    add     $sp, $sp, 4
    j       $ra 
# End of factorial
