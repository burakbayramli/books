c  From "COMPUTATIONAL PHYSICS", 3rd Ed, Enlarged Python eTextBook  
c  by RH Landau, MJ Paez, and CC Bordeianu
c  Copyright Wiley-VCH Verlag GmbH & Co. KGaA, Berlin;  Copyright R Landau,
c  Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2015.
c  Support by National Science Foundation
         
c  laplace.f: Solution of Laplace equation with finite differences 
c             Output in gnuplot 3D grid format used by

      Program laplace
			
      	Implicit none
      	Integer max
      	Parameter(max = 40)
      	Real*8 x, p(max, max)
      	Integer i, j, iter, y 
				
      	Open(8, File = 'laplace.dat', Status = 'Unknown')
c                                        side with constant potential
   	    Do 10 i = 1, max
   	      p(i, 1) = 100.
 10 	  Continue   
c                                                 iteration algorithm
  	    Do 20 iter = 1, 1000
  	      Do 30 i = 2, (max-1)
  	        Do 40 j = 2, (max-1)
  	    p(i, j) = 0.25*(p(i+1,j)+p(i-1,j) +p(i,j+1) + p(i,j-1))
 40	        Continue
 30	      Continue
 20	    Continue	    
c                                         output in gnuplot 3D format
	      Do 50 i = 1, max
	        Do 60 j = 1, max
	          Write (8, 22) p(i, j)
 60	      Continue
 	        Write (8, 22)
 50	    Continue
 22 	  Format(f10.6)
 	      Close(8)   
 	      Stop 'data saved in laplace.dat'
  	  End
