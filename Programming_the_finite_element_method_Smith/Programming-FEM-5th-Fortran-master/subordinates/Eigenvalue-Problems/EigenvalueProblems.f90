SUBROUTINE EigenvalueProblems()
    IMPLICIT NONE    
    CHARACTER(LEN=60) :: input_p101, output_p101  
    CHARACTER(LEN=60) :: input_p102_1, output_p102_1, input_p102_2, output_p102_2 
    CHARACTER(LEN=60) :: input_p103, output_p103   
    CHARACTER(LEN=60) :: input_p104, output_p104        
!***********************************************************************************************
    input_p101 = './/subordinates//Eigenvalue-Problems//p101.dat'
    output_p101 = './/subordinates//Eigenvalue-Problems//p101.res'
    CALL p101(input_p101, output_p101)  
!***********************************************************************************************
    input_p102_1 = './/subordinates//Eigenvalue-Problems//p102_1.dat'
    output_p102_1 = './/subordinates//Eigenvalue-Problems//p102_1.res'
    CALL p102(input_p102_1, output_p102_1) 
    input_p102_2 = './/subordinates//Eigenvalue-Problems//p102_2.dat'
    output_p102_2 = './/subordinates//Eigenvalue-Problems//p102_2.res'
    CALL p102(input_p102_2, output_p102_2)    
!!***********************************************************************************************
!    input_p103 = './/subordinates//Eigenvalue-Problems//p103.dat'
!    output_p103 = './/subordinates//Eigenvalue-Problems//p103.res'
!    CALL p103(input_p103, output_p103)   
!!***********************************************************************************************
!    input_p104 = './/subordinates//Eigenvalue-Problems//p104.dat'
!    output_p104 = './/subordinates//Eigenvalue-Problems//p104.res'
!    CALL p104(input_p104, output_p104)      
        
END SUBROUTINE EigenvalueProblems