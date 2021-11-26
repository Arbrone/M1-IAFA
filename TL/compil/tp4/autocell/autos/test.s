	.meta source "\"autos/test.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	seti r12, #68
	set r5, r12
	set r10, r5
	seti r11, #1
	goto_ne L3, r10, r11
L2:
	seti r9, #42
	set r6, r9
	goto L4
L3:
	seti r8, #15
	set r7, r8
L4:
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
