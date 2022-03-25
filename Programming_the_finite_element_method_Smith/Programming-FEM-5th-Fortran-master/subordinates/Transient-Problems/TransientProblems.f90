SUBROUTINE TransientProblems()
    IMPLICIT NONE   
    CHARACTER(LEN=60) :: input_p81_1, output_p81_1,input_p81_2, output_p81_2,input_p81_3, &
                         output_p81_3, input_p81_4, output_p81_4,input_p81_5, output_p81_5
    CHARACTER(LEN=60) :: input_p82, output_p82    
    CHARACTER(LEN=60) :: input_p83, output_p83    
    CHARACTER(LEN=60) :: input_p84_1, output_p84_1, input_p84_2, output_p84_2
    CHARACTER(LEN=60) :: input_p85, output_p85    
    CHARACTER(LEN=60) :: input_p86, output_p86
    CHARACTER(LEN=60) :: input_p87, output_p87
    CHARACTER(LEN=60) :: input_p88, output_p88    
    CHARACTER(LEN=60) :: input_p89, output_p89    
    CHARACTER(LEN=60) :: input_p810, output_p810
    CHARACTER(LEN=60) :: input_p811, output_p811   

!***********************************************************************************************
    input_p81_1 = './/subordinates//Transient-Problems//p81_1.dat'
    output_p81_1 = './/subordinates//Transient-Problems//p81_1.res'
    CALL p81(input_p81_1, output_p81_1)   
    input_p81_2 = './/subordinates//Transient-Problems//p81_2.dat'
    output_p81_2 = './/subordinates//Transient-Problems//p81_2.res'
    CALL p81(input_p81_2, output_p81_2)   
    input_p81_3 = './/subordinates//Transient-Problems//p81_3.dat'
    output_p81_3 = './/subordinates//Transient-Problems//p81_3.res'
    CALL p81(input_p81_3, output_p81_3)   
    input_p81_4 = './/subordinates//Transient-Problems//p81_4.dat'
    output_p81_4 = './/subordinates//Transient-Problems//p81_4.res'
    CALL p81(input_p81_4, output_p81_4)   
    input_p81_5 = './/subordinates//Transient-Problems//p81_5.dat'
    output_p81_5 = './/subordinates//Transient-Problems//p81_5.res'
    CALL p81(input_p81_5, output_p81_5)                   
!***********************************************************************************************
    input_p82 = './/subordinates//Transient-Problems//p82.dat'
    output_p82 = './/subordinates//Transient-Problems//p82.res'
    CALL p82(input_p82, output_p82)  
!***********************************************************************************************
    input_p83 = './/subordinates//Transient-Problems//p83.dat'
    output_p83 = './/subordinates//Transient-Problems//p83.res'
    CALL p83(input_p83, output_p83)   
!***********************************************************************************************
    input_p84_1 = './/subordinates//Transient-Problems//p84_1.dat'
    output_p84_1 = './/subordinates//Transient-Problems//p84_1.res'
    CALL p84(input_p84_1, output_p84_1)    
    input_p84_2 = './/subordinates//Transient-Problems//p84_2.dat'
    output_p84_2 = './/subordinates//Transient-Problems//p84_2.res'
    CALL p84(input_p84_2, output_p84_2)       
!***********************************************************************************************
    input_p85 = './/subordinates//Transient-Problems//p85.dat'
    output_p85 = './/subordinates//Transient-Problems//p85.res'
    CALL p85(input_p85, output_p85)   
!***********************************************************************************************
    input_p86 = './/subordinates//Transient-Problems//p86.dat'
    output_p86= './/subordinates//Transient-Problems//p86.res'
    CALL p86(input_p86, output_p86)   
!***********************************************************************************************
    input_p87 = './/subordinates//Transient-Problems//p87.dat'
    output_p87 = './/subordinates//Transient-Problems//p87.res'
    CALL p87(input_p87, output_p87)  
!***********************************************************************************************
    input_p88= './/subordinates//Transient-Problems//p88.dat'
    output_p88 = './/subordinates//Transient-Problems//p88.res'
    CALL p88(input_p88, output_p88)   
!***********************************************************************************************
    input_p89 = './/subordinates//Transient-Problems//p89.dat'
    output_p89 = './/subordinates//Transient-Problems//p89.res'
    CALL p89(input_p89, output_p89)    
!***********************************************************************************************
    input_p810 = './/subordinates//Transient-Problems//p810.dat'
    output_p810 = './/subordinates//Transient-Problems//p810.res'
    CALL p810(input_p810, output_p810)       
!***********************************************************************************************
    input_p811 = './/subordinates//Transient-Problems//p811.dat'
    output_p811 = './/subordinates//Transient-Problems//p811.res'
    CALL p811(input_p811, output_p811)  
        
END SUBROUTINE TransientProblems