    .text
main:
    la      $a0, hello_str
    li      $v0, 4
    syscall
    
    li      $v0, 10
    syscall

    .data
hello_str:  .asciiz "Hello World\n"   
    
