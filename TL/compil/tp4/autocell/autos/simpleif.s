	.meta source "\"autos/simpleif.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 17, 6
	set r5, r17
	invoke 5, 16, 2
	set r6, r16
	set r14, r5
	set r15, r6
	goto_eq L9, r14, r15
L8:
	seti r13, #1
	invoke 4, 13, 0
	goto L10
L9:
L10:
	set r10, r5
	set r11, r6
	add r10, r10, r11
	seti r12, #2
	goto_ne L3, r10, r12
L2:
	invoke 5, 8, 3
	invoke 5, 9, 5
	goto_lt L6, r8, r9
L5:
	seti r7, #2
	invoke 4, 7, 0
	goto L7
L6:
L7:
	goto L4
L3:
L4:
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
