	.meta source "\"autos/test.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	seti r14, #0
	set r5, r14
	invoke 5, 12, 0
	seti r13, #1
	goto_ne L3, r12, r13
L2:
	set r10, r5
	seti r11, #3
	goto_le L9, r10, r11
L8:
	seti r9, #0
	invoke 4, 9, 0
	goto L10
L9:
L10:
	goto L4
L3:
	set r7, r5
	seti r8, #3
	goto_ne L6, r7, r8
L5:
	seti r6, #1
	invoke 4, 6, 0
	goto L7
L6:
L7:
L4:
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
