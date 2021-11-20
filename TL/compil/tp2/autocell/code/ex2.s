@ Type exercise 2 here
	SETI R0, #0
	SETI R1, #1
	@ get the size of the grid
	@ R2 =  width && R3 = height
	INVOKE 1, 2, 3
	SUB R2, R2, R1
	SUB R3, R3, R1
	@ northwest cell : 0 0
	INVOKE 3, 0, 0 
	INVOKE 4, 1, 0
	@ northeast cell : n-1 0 
	INVOKE 3, 2, 0
	INVOKE 4, 1, 0
	@ southwest cell : 0 n-1
	INVOKE 3, 0, 3
	INVOKE 4, 1, 0
	@ southeast cell : n-1 n-1
	INVOKE 3, 2, 3
	INVOKE 4, 1, 0
	STOP
