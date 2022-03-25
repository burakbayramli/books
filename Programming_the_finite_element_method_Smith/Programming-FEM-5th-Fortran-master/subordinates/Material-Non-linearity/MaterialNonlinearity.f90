SUBROUTINE MaterialNonlinearity()
    IMPLICIT NONE   
    CHARACTER(LEN=60) :: input_p61, output_p61
    CHARACTER(LEN=60) :: input_p62, output_p62    
    CHARACTER(LEN=60) :: input_p63, output_p63    
    CHARACTER(LEN=60) :: input_p64, output_p64
    CHARACTER(LEN=60) :: input_p65, output_p65    
    CHARACTER(LEN=60) :: input_p66, output_p66
    CHARACTER(LEN=60) :: input_p67, output_p67
    CHARACTER(LEN=60) :: input_p68, output_p68    
    CHARACTER(LEN=60) :: input_p69, output_p69    
    CHARACTER(LEN=60) :: input_p610, output_p610
    CHARACTER(LEN=60) :: input_p611_1, output_p611_1, input_p611_2, &
                         output_p611_2, input_p611_3, output_p611_3    
    CHARACTER(LEN=60) :: input_p612, output_p612 
    CHARACTER(LEN=60) :: input_p613, output_p613   
!***********************************************************************************************
!    input_p61 = './/subordinates//Material-Non-linearity//p61.dat'
!    output_p61 = './/subordinates//Material-Non-linearity//p61.res'
!    CALL p61(input_p61, output_p61)   
!!***********************************************************************************************
!    input_p62 = './/subordinates//Material-Non-linearity//p62.dat'
!    output_p62 = './/subordinates//Material-Non-linearity//p62.res'
!    CALL p62(input_p62, output_p62)  
!!***********************************************************************************************
!    input_p63 = './/subordinates//Material-Non-linearity//p63.dat'
!    output_p63 = './/subordinates//Material-Non-linearity//p63.res'
!    CALL p63(input_p63, output_p63)   
!***********************************************************************************************
    input_p64 = './/subordinates//Material-Non-linearity//p64.dat'
    output_p64 = './/subordinates//Material-Non-linearity//p64.res'
    CALL p64(input_p64, output_p64)    
!***********************************************************************************************
    input_p65 = './/subordinates//Material-Non-linearity//p65.dat'
    output_p65 = './/subordinates//Material-Non-linearity//p65.res'
    CALL p65(input_p65, output_p65)   
!***********************************************************************************************
    input_p66 = './/subordinates//Material-Non-linearity//p66.dat'
    output_p66= './/subordinates//Material-Non-linearity//p66.res'
    CALL p66(input_p66, output_p66)   
!***********************************************************************************************
    input_p67 = './/subordinates//Material-Non-linearity//p67.dat'
    output_p67 = './/subordinates//Material-Non-linearity//p67.res'
    CALL p67(input_p67, output_p67)  
!***********************************************************************************************
    input_p68= './/subordinates//Material-Non-linearity//p68.dat'
    output_p68 = './/subordinates//Material-Non-linearity//p68.res'
    CALL p68(input_p68, output_p68)   
!***********************************************************************************************
    input_p69 = './/subordinates//Material-Non-linearity//p69.dat'
    output_p69 = './/subordinates//Material-Non-linearity//p69.res'
    CALL p69(input_p69, output_p69)    
!***********************************************************************************************
    input_p610 = './/subordinates//Material-Non-linearity//p610.dat'
    output_p610 = './/subordinates//Material-Non-linearity//p610.res'
    CALL p610(input_p610, output_p610)       
!***********************************************************************************************
    input_p611_1 = './/subordinates//Material-Non-linearity//p611_1.dat'
    output_p611_1 = './/subordinates//Material-Non-linearity//p611_1.res'
    CALL p611(input_p611_1, output_p611_1)  
    input_p611_2 = './/subordinates//Material-Non-linearity//p611_2.dat'
    output_p611_2 = './/subordinates//Material-Non-linearity//p611_2.res'
    CALL p611(input_p611_2, output_p611_2) 
    input_p611_3 = './/subordinates//Material-Non-linearity//p611_3.dat'
    output_p611_3 = './/subordinates//Material-Non-linearity//p611_3.res'
    CALL p611(input_p611_3, output_p611_3)   
!***********************************************************************************************
    !input_p612 = './/subordinates//Material-Non-linearity//p612.dat'
    !output_p612 = './/subordinates//Material-Non-linearity//p612.res'
    !CALL p612(input_p612, output_p612)       
!***********************************************************************************************
    input_p613 = './/subordinates//Material-Non-linearity//p613.dat'
    output_p613 = './/subordinates//Material-Non-linearity//p613.res'
    CALL p613(input_p613, output_p613)                                   
END SUBROUTINE MaterialNonlinearity