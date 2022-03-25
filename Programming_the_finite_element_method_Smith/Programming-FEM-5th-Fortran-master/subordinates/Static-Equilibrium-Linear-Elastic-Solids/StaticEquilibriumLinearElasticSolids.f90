SUBROUTINE StaticEquilibriumLinearElasticSolids()
    IMPLICIT NONE    
    CHARACTER(LEN=70) :: input_p51_1, output_p51_1, input_p51_2, output_p51_2,input_p51_3, &
                         output_p51_3,input_p51_4, output_p51_4,input_p51_5, output_p51_5
    CHARACTER(LEN=70) :: input_p52, output_p52    
    CHARACTER(LEN=70) :: input_p53, output_p53    
    CHARACTER(LEN=70) :: input_p54_1, output_p54_1, input_p54_2, output_p54_2, input_p54_3, output_p54_3
    CHARACTER(LEN=70) :: input_p55, output_p55
    CHARACTER(LEN=70) :: input_p56_1, output_p56_1, input_p56, output_p56   
    !CHARACTER(LEN=60) :: input_p57, output_p57   
!***********************************************************************************************
    input_p51_1 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p51_1.dat'
    output_p51_1 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p51_1.res'
    CALL p51(input_p51_1, output_p51_1)   
    input_p51_2 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p51_2.dat'
    output_p51_2 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p51_2.res'
    CALL p51(input_p51_2, output_p51_2)   
    input_p51_3 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p51_3.dat'
    output_p51_3 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p51_3.res'
    CALL p51(input_p51_3, output_p51_3)   
    input_p51_4 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p51_4.dat'
    output_p51_4 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p51_4.res'
    CALL p51(input_p51_4, output_p51_4)     
    input_p51_5 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p51_5.dat'
    output_p51_5 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p51_5.res'
    CALL p51(input_p51_5, output_p51_5)            
!***********************************************************************************************
    input_p52 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p52.dat'
    output_p52 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p52.res'
    CALL p52(input_p52, output_p52)  
!***********************************************************************************************
    input_p53 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p53.dat'
    output_p53 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p53.res'
    CALL p53(input_p53, output_p53)   
!***********************************************************************************************
    input_p54_1 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p54_1.dat'
    output_p54_1 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p54_1.res'
    CALL p54(input_p54_1, output_p54_1)   
    input_p54_2 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p54_2.dat'
    output_p54_2 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p54_2.res'
    CALL p54(input_p54_2, output_p54_2)  
    input_p54_3 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p54_3.dat'
    output_p54_3 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p54_3.res'
    CALL p54(input_p54_3, output_p54_3)      
!***********************************************************************************************
    input_p55 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p55.dat'
    output_p55 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p55.res'
    CALL p55(input_p55, output_p55)   
!***********************************************************************************************
    !input_p56 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p56.dat'
    !output_p56 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p56.res'
    !CALL p56(input_p56, output_p56)  
    !input_p56_1 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p56_1.dat'
    !output_p56_1 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p56_1.res'
    !CALL p56(input_p56_1, output_p56_1)           
!***********************************************************************************************
    !input_p57 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p57.dat'
    !output_p57 = './/subordinates//Static-Equilibrium-Linear-Elastic-Solids//p57.res'
    !CALL p57(input_p57, output_p57)      
    
END SUBROUTINE StaticEquilibriumLinearElasticSolids