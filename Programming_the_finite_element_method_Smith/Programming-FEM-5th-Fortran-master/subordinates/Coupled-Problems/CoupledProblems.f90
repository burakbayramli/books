SUBROUTINE CoupledProblems()
    IMPLICIT NONE    
    CHARACTER(LEN=60) :: input_p91, output_p91
    CHARACTER(LEN=60) :: input_p92, output_p92    
    CHARACTER(LEN=60) :: input_p93, output_p93    
    CHARACTER(LEN=60) :: input_p94, output_p94
    CHARACTER(LEN=60) :: input_p95, output_p95
    CHARACTER(LEN=60) :: input_p96_1, output_p96_1, input_p96_2, output_p96_2       
!***********************************************************************************************
    input_p91 = './/subordinates//Coupled-Problems//p91.dat'
    output_p91 = './/subordinates//Coupled-Problems//p91.res'
    CALL p91(input_p91, output_p91)   
!***********************************************************************************************
    input_p92 = './/subordinates//Coupled-Problems//p92.dat'
    output_p92 = './/subordinates//Coupled-Problems//p92.res'
    CALL p92(input_p92, output_p92)  
!***********************************************************************************************
    input_p93 = './/subordinates//Coupled-Problems//p93.dat'
    output_p93 = './/subordinates//Coupled-Problems//p93.res'
    CALL p93(input_p93, output_p93)   
!***********************************************************************************************
    input_p94 = './/subordinates//Coupled-Problems//p94.dat'
    output_p94 = './/subordinates//Coupled-Problems//p94.res'
    CALL p94(input_p94, output_p94)    
!***********************************************************************************************
    input_p95 = './/subordinates//Coupled-Problems//p95.dat'
    output_p95 = './/subordinates//Coupled-Problems//p95.res'
    CALL p95(input_p95, output_p95)   
!***********************************************************************************************
    input_p96_1 = './/subordinates//Coupled-Problems//p96_1.dat'
    output_p96_1 = './/subordinates//Coupled-Problems//p96_1.res'
    CALL p96(input_p96_1, output_p96_1)  
    input_p96_2 = './/subordinates//Coupled-Problems//p96_2.dat'
    output_p96_2 = './/subordinates//Coupled-Problems//p96_2.res'
    CALL p96(input_p96_2, output_p96_2)           
END SUBROUTINE CoupledProblems