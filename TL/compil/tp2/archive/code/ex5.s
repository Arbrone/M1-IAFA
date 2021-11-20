@ Type exercise 5 here
	SETI R0, #0
	SETI R1, #1
	SETI R4, #0
	SETI R5, #0
	@ R2 = width && R3 = height
	INVOKE 1, 2, 3
LINE:
	GOTO_GE END, R4, R3
	GOTO COLUMN
	GOTO LINE

TRANS:
	MUL R5, R5, R0
	ADD R4, R4, R1
	GOTO LINE

COLUMN:
	GOTO_GE TRANS, R5, R2
	INVOKE 3, 4, 5
	@ get current value
	INVOKE 5, 6, 0 
	GOTO_EQ ISONE, R6, R1
    GOTO ISZERO

ISONE:
	INVOKE 4, 0, 0
	ADD R5, R5, R1
	GOTO COLUMN

ISZERO:
    @ get NW value
	INVOKE 5, 6, 2 
	GOTO_EQ SETONE, R6, R1

    @ get N value
	INVOKE 5, 6, 1 
	GOTO_EQ SETONE, R6, R1

    @ get NE value
	INVOKE 5, 6, 8
	GOTO_EQ SETONE, R6, R1

    @ get W value
	INVOKE 5, 6, 3 
	GOTO_EQ SETONE, R6, R1

    @ get E value
	INVOKE 5, 6, 7
	GOTO_EQ SETONE, R6, R1

    @ get SW value
	INVOKE 5, 6, 4
	GOTO_EQ SETONE, R6, R1

    @ get S value
	INVOKE 5, 6, 5
	GOTO_EQ SETONE, R6, R1

    @ get SE value
	INVOKE 5, 6, 6
	GOTO_EQ SETONE, R6, R1

	ADD R5, R5, R1
	GOTO COLUMN

SETONE:
    INVOKE 4, 1, 0
    ADD R5, R5, R1
	GOTO COLUMN

END:
	STOP
