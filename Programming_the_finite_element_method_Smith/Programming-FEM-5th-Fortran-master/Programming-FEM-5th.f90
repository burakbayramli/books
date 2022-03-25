!  ProgrammingFEM5th.f90 
!
!  Author  :  lcy
!  Start   :  2019/12/01
!  Email   :  cunyicom@outlook.com
!  GitHub  :  https://github.com/cunyizju/Programming-FEM-5th-Fortran
!  
!  Acknowledgement :  Prof. I M Smith, Prof. D V Griffiths ( The authors of Progrmming the finite element method )
!                     Prof. X Y Shang ( My supervisor )
!****************************************************************************************************************
!
!  PROGRAM :  ProgrammingFEM5th
!
!  PURPOSE :  Progrmming the finite element method using Microsoft Visual Studio and Intel Fortran.
!             Containing the following problems
!               Static Equilibrium Structures
!               Static Equilibrium Linear Elastic Solids  
!               Material Nonlinearity
!               Steady State Flow
!               Transient Problems
!               Coupled Problems
!               Eigenvalue Problems
!               Forced Vibrations
!
!Fortran online compiler: https://www.tutorialspoint.com/compile_fortran_online.php
!gfortran: https://gcc.gnu.org/onlinedocs/gcc-7.1.0/gfortran/index.html#SEC_Contents
!****************************************************************************************************************

program ProgrammingFEM5th
!---------------------------------- declaration statement --------------------------------------    
    IMPLICIT NONE
    
    CALL StaticEquilibriumStructures()
    !CALL StaticEquilibriumLinearElasticSolids()  
    !CALL MaterialNonlinearity() !Require more than half an hour. It is recommended to run this subroutine alone.
    !CALL SteadyStateFlow()
    !CALL TransientProblems()
    !CALL CoupledProblems()
    !CALL EigenvalueProblems()
    !CALL ForcedVibrations()
end program ProgrammingFEM5th
    
   