## Program to check if the input is palindrome or not (Case sensitive)
## A - address of the first char in the string
## B - address of the last char in the string
##  $t1 - A
##  $t2 - B
##  $t3 - char at A
##  $t4 - char at B
##  $v0 - syscall parameter
##  $a0 - argument
##  $a1 - argument

    .data
inp_str:        .space  1024
yes:            .asciiz "Yes\n"
no:             .asciiz "No\n"

    .text
main:
# get input from user
    la      $a0, inp_str
    li      $a1, 1024
    li      $v0, 8
    syscall

# set up A
    la      $t1, inp_str

# set up B
    la      $t2, inp_str
find_last_char:
    lb      $t4, ($t2)
    beqz    $t4, find_last_char_end
    addu    $t2, $t2, 1
    b       find_last_char
find_last_char_end:
    subu    $t2, $t2, 2 # skip back over \n & \0

# check until A>=B or char(A) != char(B)
pal_loop:
    bge     $t1, $t2, pal_yes
    lb      $t3, ($t1)
    lb      $t4, ($t2)
    bne     $t3, $t4, pal_no

    addu    $t1, $t1, 1
    subu    $t2, $t2, 1
    b       pal_loop

# print yes
pal_yes:
    la      $a0, yes
    li      $v0, 4
    syscall
    b       end

# print no
pal_no:
    la      $a0, no
    li      $v0, 4
    syscall
    b       end

# exit
end:
    li      $v0, 10
    syscall

# end of palindrome.asm
