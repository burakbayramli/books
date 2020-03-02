NAME          EXAMPLE1_3
ROWS
 N  OBJ
 G  R1
 E  R2
 L  R3
 E  R4
COLUMNS
    X1        R2        4              R3                 -3
    X1        R4        4              OBJ                -2
    X2        R1        2              R2                 -3
    X2        R3        2              OBJ                 4
    X3        R1        3              R2                  8
    X3        R4        -1             OBJ                -2
    X4        R2        -1             R3                 -4
    X4        R4        4              OBJ                 2
RHS
    B1        R1        6              R2                  20
    B1        R3       -8              R4                  18
BOUNDS
 LO BND1      X1		1
 LO BND1      X3		2
 UP BND1      X3		10
ENDATA