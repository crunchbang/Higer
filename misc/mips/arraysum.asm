	.data
arr1:	.word	1 2 3				# array with values 1,2,3
sum:	.word	0					# sum = 0

	.text
main:	la		$t0, arr1			# load base address of array to register
		li		$t1, 3				# set count = 3
		li		$t2, 1				# set value to decrement by 1
		lw		$t3, sum			# load value of sum(0) to $t3

loop:	beqz	$t1, exit			# jump to exit when count becomes 0
		lw		$t4, ($t0)			# load the content of array location to $t4
		addi	$t0, $t0, 4			# increment the array location by 4
		sub		$t1, $t1, $t2       # decrement count
		add 	$t3, $t3, $t4       # modify the value of sum
		b 		loop                # go to label : loop

exit:	sw		$t3, sum            # store content of $t3 to sum
		li		$v0, 1
		move	$a0, $t3
		syscall                     # print sum

		li		$v0, 10
		syscall                     # exit