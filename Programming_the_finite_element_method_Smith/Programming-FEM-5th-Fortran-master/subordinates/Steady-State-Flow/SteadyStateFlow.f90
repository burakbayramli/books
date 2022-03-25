SUBROUTINE SteadyStateFlow()
    IMPLICIT NONE    
    CHARACTER(LEN=60) :: input_p71_1, output_p71_1, input_p71_2, output_p71_2  
    CHARACTER(LEN=60) :: input_p72_1, output_p72_1, input_p72_2, output_p72_2 
    CHARACTER(LEN=60) :: input_p73_1, output_p73_1, input_p73_2, output_p73_2   
    CHARACTER(LEN=60) :: input_p74, output_p74  
    CHARACTER(LEN=60) :: input_p75, output_p75       
!***********************************************************************************************
    input_p71_1 = './/subordinates//Steady-State-Flow//p71_1.dat'
    output_p71_1 = './/subordinates//Steady-State-Flow//p71_1.res'
    CALL p71(input_p71_1, output_p71_1)  
    input_p71_2 = './/subordinates//Steady-State-Flow//p71_2.dat'
    output_p71_2 = './/subordinates//Steady-State-Flow//p71_2.res'
    CALL p71(input_p71_2, output_p71_2)  
!***********************************************************************************************
    input_p72_1 = './/subordinates//Steady-State-Flow//p72_1.dat'
    output_p72_1 = './/subordinates//Steady-State-Flow//p72_1.res'
    CALL p72(input_p72_1, output_p72_1) 
    input_p72_2 = './/subordinates//Steady-State-Flow//p72_2.dat'
    output_p72_2 = './/subordinates//Steady-State-Flow//p72_2.res'
    CALL p72(input_p72_2, output_p72_2)    
!***********************************************************************************************
    input_p73_1 = './/subordinates//Steady-State-Flow//p73_1.dat'
    output_p73_1 = './/subordinates//Steady-State-Flow//p73_1.res'
    CALL p73(input_p73_1, output_p73_1) 
    input_p73_2 = './/subordinates//Steady-State-Flow//p73_2.dat'
    output_p73_2 = './/subordinates//Steady-State-Flow//p73_2.res'
    CALL p73(input_p73_2, output_p73_2)    
!***********************************************************************************************
    input_p74 = './/subordinates//Steady-State-Flow//p74.dat'
    output_p74 = './/subordinates//Steady-State-Flow//p74.res'
    CALL p74(input_p74, output_p74)     
!***********************************************************************************************
    input_p75 = './/subordinates//Steady-State-Flow//p75.dat'
    output_p75 = './/subordinates//Steady-State-Flow//p75.res'
    CALL p75(input_p75, output_p75)     
        
END SUBROUTINE SteadyStateFlow