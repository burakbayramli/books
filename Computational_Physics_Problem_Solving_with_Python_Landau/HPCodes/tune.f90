!   From "COMPUTATIONAL PHYSICS", 3rd Ed, Enlarged Python eTextBook  
    by RH Landau, MJ Paez, and CC Bordeianu
    Copyright Wiley-VCH Verlag GmbH & Co. KGaA, Berlin;  Copyright R Landau,
    Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2015.
    Support by National Science Foundation
			
! 	 tune.f90: matrix algebra program to be tuned for performance			

Program	 tune		

  parameter (ldim = 2050)		
	Implicit Double precision (a - h, o - z)		
	dimension ham(ldim, ldim), coef(ldim), sigma(ldim)		
                                       ! set up H and starting vector
	Do i = 1, ldim	 
	  Do j = 1, ldim		
		  If ( abs(j - i)  >  10) then		
			  ham(j, i) = 0.		
			else	 
			  ham(j, i) = 0.3**Abs(j - i)		
			EndIf		
	  End Do
		ham(i, i) = i		
		coef(i) = 0.	 
	End Do	 
	coef(1) = 1.		
                                                    ! start iterating
	err = 1.		
	iter = 0	 
 20		If (iter< 15 .and. err >1.e-6) then	 
	iter = iter + 1	
                                 ! compute current energy & normalize
	ener = 0.	 
	ovlp = 0.	 
	Do		i = 1, ldim	 
	  ovlp = ovlp + coef(i)*coef(i)	 
		sigma(i) = 0.		
		Do		 j = 1, ldim		
		  sigma(i) = sigma(i) + coef(j)*ham(j, i)	 
		End Do		
		ener = ener + coef(i)*sigma(i)
	End Do 
	ener = ener/ovlp	 
	Do		I = 1, ldim	 
    coef(i) = coef(i)/Sqrt(ovlp)		
		sigma(i) = sigma(i)/Sqrt(ovlp)		
	End Do	 
                                      ! compute update and error norm
	err = 0.		
	Do	i = 1, ldim	 
	  If (i == 1) goto 22	 
		step = (sigma(i) - ener*coef(i))/(ener - ham(i, i))		
		coef(i) = coef(i) + step		
		err = err + step**2	
 22 Continue
 23 End Do
   err = sqrt(err)	 
     write(*, '(1x, i2, 7f10.5)') iter, ener, err, coef(1)	 
	   goto 20		 
    Endif		
  Stop	 
End Program tune	

