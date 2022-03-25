SUBROUTINE StaticEquilibriumStructures()
    IMPLICIT NONE    
    CHARACTER(LEN=60) :: input_p41_1, output_p41_1,input_p41_2, output_p41_2 
    CHARACTER(LEN=60) :: input_p42_1, output_p42_1,input_p42_2, output_p42_2 
    CHARACTER(LEN=60) :: input_p43_1, output_p43_1,input_p43_2, output_p43_2   
    CHARACTER(LEN=60) :: input_p44_1, output_p44_1,input_p44_2, output_p44_2  
    CHARACTER(LEN=60) :: input_p45_1, output_p45_1,input_p45_2, output_p45_2
    CHARACTER(LEN=60) :: input_p46_1, output_p46_1,input_p46_2, output_p46_2  
    CHARACTER(LEN=60) :: input_p47, output_p47      
!***********************************************************************************************        
    input_p41_1 = './/subordinates//Static-Equilibrium-Structures//p41_1.dat'
    output_p41_1 = './/subordinates//Static-Equilibrium-Structures//p41_1.res'
    input_p41_2 = './/subordinates//Static-Equilibrium-Structures//p41_2.dat'
    output_p41_2 = './/subordinates//Static-Equilibrium-Structures//p41_2.res'                
    CALL p41(input_p41_1, output_p41_1)
    CALL p41(input_p41_2, output_p41_2)
!***********************************************************************************************
    input_p42_1 = './/subordinates//Static-Equilibrium-Structures//p42_1.dat'
    output_p42_1 = './/subordinates//Static-Equilibrium-Structures//p42_1.res'
    input_p42_2 = './/subordinates//Static-Equilibrium-Structures//p42_2.dat'
    output_p42_2 = './/subordinates//Static-Equilibrium-Structures//p42_2.res'
    CALL p42(input_p42_1, output_p42_1)
    CALL p42(input_p42_2, output_p42_2)
!***********************************************************************************************
    input_p43_1 = './/subordinates//Static-Equilibrium-Structures//p43_1.dat'
    output_p43_1 = './/subordinates//Static-Equilibrium-Structures//p43_1.res'
    input_p43_2 = './/subordinates//Static-Equilibrium-Structures//p43_2.dat'
    output_p43_2 = './/subordinates//Static-Equilibrium-Structures//p43_2.res'
    CALL p43(input_p43_1, output_p43_1)
    CALL p43(input_p43_2, output_p43_2)     
!***********************************************************************************************
    input_p44_1 = './/subordinates//Static-Equilibrium-Structures//p44_1.dat'
    output_p44_1 = './/subordinates//Static-Equilibrium-Structures//p44_1.res'
    input_p44_2 = './/subordinates//Static-Equilibrium-Structures//p44_2.dat'
    output_p44_2 = './/subordinates//Static-Equilibrium-Structures//p44_2.res'
    CALL p44(input_p44_1, output_p44_1)
    CALL p44(input_p44_2, output_p44_2)     
!***********************************************************************************************
    input_p45_1 = './/subordinates//Static-Equilibrium-Structures//p45_1.dat'
    output_p45_1 = './/subordinates//Static-Equilibrium-Structures//p45_1.res'
    input_p45_2 = './/subordinates//Static-Equilibrium-Structures//p45_2.dat'
    output_p45_2 = './/subordinates//Static-Equilibrium-Structures//p45_2.res'
    CALL p45(input_p45_1, output_p45_1)
    CALL p45(input_p45_2, output_p45_2)     
!***********************************************************************************************
    input_p46_1 = './/subordinates//Static-Equilibrium-Structures//p46_1.dat'
    output_p46_1 = './/subordinates//Static-Equilibrium-Structures//p46_1.res'
    input_p46_2 = './/subordinates//Static-Equilibrium-Structures//p46_2.dat'
    output_p46_2 = './/subordinates//Static-Equilibrium-Structures//p46_2.res'
    CALL p46(input_p46_1, output_p46_1)
    CALL p46(input_p46_2, output_p46_2) 
!***********************************************************************************************
    input_p47 = './/subordinates//Static-Equilibrium-Structures//p47.dat'
    output_p47 = './/subordinates//Static-Equilibrium-Structures//p47.res'
    CALL p47(input_p47, output_p47)           
END SUBROUTINE StaticEquilibriumStructures