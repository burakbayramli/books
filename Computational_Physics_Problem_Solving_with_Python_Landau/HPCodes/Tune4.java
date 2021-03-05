/* From "COMPUTATIONAL PHYSICS", 3rd Ed, Enlarged Python eTextBook  
    by RH Landau, MJ Paez, and CC Bordeianu
    Copyright Wiley-VCH Verlag GmbH & Co. KGaA, Berlin;  Copyright R Landau,
    Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2015.
    Support by National Science Foundation"""
*/
// Tune4.java: matrix algebra program, basic optimization      

public class Tune4  {
    
    public static void main(String[] argv)  { 
      final int Ldim = 2051;   
      int i, j, iter = 0; 
      double [][] ham = new double [Ldim] [Ldim]; double [] diag = new double [Ldim]; 
      double [] coef  = new double [Ldim]; double [] sigma = new double [Ldim];   
      double err, ener, ovlp, ovlp1, ovlp2, step = 0., fact, time, t, t1, t2, u; 
      time = System.currentTimeMillis();                        // Store initial time
      for ( i = 1;  i <= Ldim-1;  i++ )   {                     // Set up Hamiltonian
        for ( j = 1;  j <= Ldim-1;  j++ )  if (Math.abs(j-i) >10) ham[j][i] = 0. ; 
          else  ham[j][i] = Math.pow(0.3, Math.abs(j-i));    
      }                                                 // Iterate towards solution
      for (i=1;  i<Ldim-1;  i++) {ham[i][i] = i; coef[i] = 0.; diag[i] = ham [i][i];}
      coef[1] = 1.;  err = 1.;  iter = 0 ;  
      while (iter  < 15 && err > 1.e-6)  {      // Compute current energy & normalize
        iter = iter + 1; ener = 0. ; ovlp1 = 0.;   ovlp2 = 0.; 
        for ( i= 1;  i <= Ldim-2;  i = i + 2  ) {   
          ovlp1 = ovlp1 + coef[i]*coef[i] ;   
          ovlp2 = ovlp2 + coef[i+1]*coef[i+1] ;   
          t1 =  t2 = 0.; 
          for ( j=1;  j <= Ldim-1;  j++ )  { t1 = t1 + coef[j]*ham[j][i]; 
                                             t2 = t2 + coef[j]*ham[j][i+1];  }
          sigma[i] = t1;  sigma[i + 1] = t2;   
          ener = ener + coef[i]*t1 + coef[i+1]*t2 ; 
        }   
        ovlp = ovlp1 + ovlp2 ; 
        ener = ener/ovlp; 
        fact = 1./Math.sqrt(ovlp); 
        coef[1] = fact*coef[1]; 
        err = 0.;                                              // Update & error norm
        for (i = 2; i <= Ldim-1; i++) { t = fact*coef[i];  u = fact*sigma[i]-ener*t;
             step = u/(ener-diag[i]);  coef[i] = t + step;  err = err +  step*step; }
        err = Math.sqrt(err) ;  
        System.out.println ("iter, ener, err "+iter+", " + ener + ", " + err);
      }   
      time = (System.currentTimeMillis() - time)/1000; 
      System.out.println("time = " + time + "s");                     // Elapsed time
} }
