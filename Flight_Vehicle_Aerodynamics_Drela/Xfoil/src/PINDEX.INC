
C
C---- Pointers for referencing polar force coefficients
C     First 4 pointers must be main polar plot variables.
C
      PARAMETER (
     &    IAL =  1,    ! alpha
     &    ICL =  2,    ! CL
     &    ICD =  3,    ! CD
     &    ICM =  4,    ! Cm
     &    ICW =  5,    ! CDwave
     &    ICV =  6,    ! CDvisc
     &    ICP =  7,    ! CDpres
     &    IMA =  8,    ! Mach
     &    IRE =  9,    ! Re
     &    INC = 10,    ! Ncrit
     &    ICH = 11,    ! Hinge moment
     &    IMC = 12,    ! Minimum Cp on surface
     &    ICDH   = 13,   ! CDh  (engine thrust coeff.)
     &    ICMDOT = 14 )  ! Cm_dot
      PARAMETER (IPTOT=14)
C
C
C---------------------
C  Pointers for referencing polar airfoil-side quantities
C
      PARAMETER (
     &    JTP = 1,     ! trip
     &    JTN = 2 )    ! transition
      PARAMETER (JPTOT=2)

      CHARACTER*10 CPOLNAME(IPTOT)
      CHARACTER*5 CPOLSNAME(JPTOT)
      CHARACTER*6 CPOLFORM(IPTOT), CPOLSFORM(JPTOT)
C
      DATA CPOLNAME  /
     & 'alpha     ',
     & 'CL        ',
     & 'CD        ',
     & 'CM        ',
     & 'CDw       ',
     & 'CDv       ',
     & 'CDp       ',
     & 'Mach      ',
     & 'Re        ',
     & 'Ncrit     ',
     & 'Chinge    ', 
     & 'Cpmin     ',
     & 'CDh       ',
     & 'Cmdot     '  /
      DATA CPOLFORM  /
     & 'F7.3  ',   !    alpha
     & 'F9.4  ',   !     CL  
     & 'F10.5 ',   !     CD  
     & 'F9.4  ',   !     CM  
     & 'F10.5 ',   !     CDw 
     & 'F10.5 ',   !     CDv 
     & 'F10.5 ',   !     CDp 
     & 'F8.4  ',   !     Mach
     & 'E11.3 ',   !     Re  
     & 'F7.3  ',   !    Ncrit
     & 'F10.5 ',   !    Chinge
     & 'F9.4  ',   !    Cpmin 
     & 'F11.5 ',   !     CDh
     & 'F9.5  ' /  !     Cmdot 

      DATA CPOLSNAME /
     & 'Xtrip',
     & 'Xtr  ' /
      DATA CPOLSFORM  /
     & 'F9.4  ',    !    Xtrip
     & 'F9.4  '  /  !    Xtr
