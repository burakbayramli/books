SUBROUTINE ForcedVibrations()
    IMPLICIT NONE    
    CHARACTER(LEN=60) :: input_p111, output_p111  
    CHARACTER(LEN=60) :: input_p112, output_p112 
    CHARACTER(LEN=60) :: input_p113, output_p113  
    CHARACTER(LEN=60) :: input_p114, output_p114  
    CHARACTER(LEN=60) :: input_p115, output_p115  
    CHARACTER(LEN=60) :: input_p116, output_p116 
    CHARACTER(LEN=60) :: input_p117, output_p117  
    CHARACTER(LEN=60) :: input_p118, output_p118      
!***********************************************************************************************
    input_p111 = './/subordinates//Forced-Vibrations//p111.dat'
    output_p111 = './/subordinates//Forced-Vibrations//p111.res'
    CALL p111(input_p111, output_p111)  
!***********************************************************************************************
    input_p112 = './/subordinates//Forced-Vibrations//p112.dat'
    output_p112 = './/subordinates//Forced-Vibrations//p112.res'
    CALL p112(input_p112, output_p112) 
!***********************************************************************************************
    input_p113 = './/subordinates//Forced-Vibrations//p113.dat'
    output_p113 = './/subordinates//Forced-Vibrations//p113.res'
    CALL p113(input_p113, output_p113) 
!***********************************************************************************************
    input_p114 = './/subordinates//Forced-Vibrations//p114.dat'
    output_p114 = './/subordinates//Forced-Vibrations//p114.res'
    CALL p114(input_p114, output_p114)     
!***********************************************************************************************
    input_p115 = './/subordinates//Forced-Vibrations//p115.dat'
    output_p115 = './/subordinates//Forced-Vibrations//p115.res'
    CALL p115(input_p115, output_p115)     
!***********************************************************************************************
    input_p116 = './/subordinates//Forced-Vibrations//p116.dat'
    output_p116 = './/subordinates//Forced-Vibrations//p116.res'
    CALL p116(input_p116, output_p116)     
!***********************************************************************************************
    input_p117 = './/subordinates//Forced-Vibrations//p117.dat'
    output_p117 = './/subordinates//Forced-Vibrations//p117.res'
    CALL p117(input_p117, output_p117)     
!***********************************************************************************************
    input_p118 = './/subordinates//Forced-Vibrations//p118.dat'
    output_p118 = './/subordinates//Forced-Vibrations//p118.res'
    CALL p118(input_p118, output_p118)                   
END SUBROUTINE ForcedVibrations