ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  tune1.f: a matrix algebra program with basic optimization           c
c                                                                      c
c  UNIX (DEC OSF, IBM AIX): f77 tune1.f                                c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc   
c      
      Program  tune1   
      PARAMETER (ldim = 2050)   
      Implicit Double Precision (a-h,o-z)   
      Dimension ham(ldim,ldim),coef(ldim),sigma(ldim)   
c   
c       set up Hamiltonian and starting vector
c   
      Do 10 i = 1,ldim   
         Do 11 j = 1,ldim   
             If( Abs(j-i) .gt. 10) Then   
                ham(j,i) = 0.0   
             Else   
                ham(j,i) = 0.3**Abs(j-i)   
             EndIf   
 11      Continue   
         ham(i,i) = i   
         coef(i) = 0.0   
 10   Continue   
      coef(1) = 1.0   
c  
c      start iterating towards the solution   
      err = 1.0   
      iter = 0
 20   if(iter.lt.15 .and. err.gt.1.0e-6) Then   
         iter = iter+1   
c     compute energy, norm of current approximation,\&  normalize   
c   
      ener = 0.0   
      ovlp = 0.0   
      Do 21   i = 1,ldim   
         ovlp = ovlp+coef(i)*coef(i)   
         sigma(i) = 0.0   
         Do 30   j = 1,ldim   
            sigma(i) = sigma(i)+coef(j)*ham(j,i)   
 30      Continue   
        ener = ener+coef(i)*sigma(i)   
 21   Continue   
      ener = ener/ovlp   
      fact  = 1.0/Sqrt(ovlp)   
      coef(1)  =  fact*coef(1)   
      err  =  0.0   
      Do 22   i = 2,ldim   
         t         =   fact*coef(i)   
         u         =   fact*sigma(i) - ener*t   
         step      =   u/(ener - ham(i,i))   
         coef(i)   =   t + step   
         err       =   err + step*step
 22   Continue   
      err = Sqrt(err)   
      Write(*,'(1x,i2,7f10.5)') iter,ener,err,coef(1)   
      GoTo 20    
      EndIf   
      Stop   
      End   
