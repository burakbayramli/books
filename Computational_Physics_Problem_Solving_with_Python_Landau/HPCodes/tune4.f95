!  tune4.f95: matrix algebra with RISC tuning                  !

      Program  tune4   
      PARAMETER (ldim = 2050)   
      Implicit Double Precision (a-h,o-z)   
      Dimension ham(ldim,ldim),coef(ldim),sigma(ldim),diag(ldim)     
!       set up Hamiltonian and starting vector   
      Do  i = 1,ldim   
         Do  j = 1,ldim   
             If( Abs(j-i) > 10) Then   
                ham(j,i) = 0.0   
             Else   
                ham(j,i) = 0.3**Abs(j-i)   
             EndIf   
         End Do
      End Do
   
!      start iterating towards the solution   
      Do  i = 1,ldim   
         ham(i,i) = i   
         coef(i)  = 0.0   
         diag(i)  = ham(i,i)   
      End Do   
      coef(1) = 1.0   
      err = 1.0   
      iter = 0   
 20   If(iter<15 .and. err>1.0e-6) Then   
        iter = iter+1  
      ener = 0.0   
      ovlp1 = 0.0   
      ovlp2 = 0.0   
      Do    i = 1,ldim-1,2   
         ovlp1 = ovlp1+coef(i)*coef(i)   
         ovlp2 = ovlp2+coef(i+1)*coef(i+1)   
         t1    = 0.0   
         t2    = 0.0   
         Do    j = 1,ldim   
            t1 = t1 + coef(j)*ham(j,i)   
            t2 = t2 + coef(j)*ham(j,i+1)   
         End Do   
      sigma(i)   = t1   
      sigma(i+1) = t2   
      ener       = ener + coef(i)*t1 + coef(i)*t2   
      End Do  
      ovlp = ovlp1 + ovlp2   
      ener = ener/ovlp   
      fact  = 1.0/Sqrt(ovlp)   
      coef(1)  =  fact*coef(1)   
      err  =  0.0   
      Do    i = 2,ldim   
         t         =   fact*coef(i)   
         u         =   fact*sigma(i) - ener*t   
         step      =   u/(ener - diag(i))   
         coef(i)   =   t + step   
         err       =   err + step*step   
      End Do   
      err = Sqrt(err)   
      Write(*,'(1x,i2,7f10.5)') iter,ener,err,coef(1)   
      GoTo 20    
      EndIf  
      Stop
      End  Program  tune4    
