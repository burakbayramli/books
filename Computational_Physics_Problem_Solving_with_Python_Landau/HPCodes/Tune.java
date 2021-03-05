/*  From "COMPUTATIONAL PHYSICS", 3rd Ed, Enlarged Python eTextBook  
    by RH Landau, MJ Paez, and CC Bordeianu
    Copyright Wiley-VCH Verlag GmbH & Co. KGaA, Berlin;  Copyright R Landau,
    Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2015.
    Support by National Science Foundation                           
   */
// Tune.java: eigenvalue solution for performace tuning

public class Tune { 

  public static void main(String[] argv) { 
    final int Ldim = 2051;   
    int i, j, iter = 0; 
    double [][] ham = new double [Ldim] [Ldim];double[] coef = new double [Ldim]; 
    double [] sigma = new double [Ldim];double time, err, ener, ovlp, step = 0.; 
    time = System.currentTimeMillis();                             // Initialize time
      for ( i = 1;  i <= Ldim-1;  i++ )  {                    // Init matrix & vector
        for ( j = 1;  j <= Ldim-1;  j++ )  { 
          if (Math.abs(j-i) >10)  ham[j][i] = 0. ;  
          else  ham[j][i] = Math.pow(0.3, Math.abs(j-i));  
        }   
        ham[i][i] = i ;   coef[i] = 0.;      
      } 
      coef[1] = 1.;   err = 1.;   iter = 0 ;                       // Start iteration
      while (iter  < 15 && err > 1.e-6)  { 
        iter = iter + 1;  ener = 0.;   ovlp = 0.;  
        for ( i= 1;  i <= Ldim-1;  i++ )  {                  // Compute E & normalize
          ovlp = ovlp + coef[i]*coef[i];   sigma[i] = 0.;    
          for (j= 1; j <= Ldim-1;  j++)  sigma[i] = sigma[i]+coef[j]*ham[j][i]; 
          ener = ener + coef[i]*sigma[i] ;    
        }  
        ener = ener/ovlp; 
        for (  i = 1;  i <= Ldim-1;  i++ )  { coef[i] = coef[i]/Math.sqrt(ovlp) ;   
                                              sigma[i] = sigma[i]/Math.sqrt(ovlp); }    
        err = 0.;   
        for ( i = 2;  i <= Ldim-1;  i++ )  {                                // Update
          step = (sigma[i] - ener*coef[i])/(ener-ham[i][i]);
          coef[i] = coef[i] + step; err = err +  step*step ;  
				} 
        err = Math.sqrt(err) ;  
        System.out.println ("iter, ener, err " + iter + ",  " + ener + ",  " + err);
      }              
      time = (System.currentTimeMillis() - time)/1000.;               // Elapsed time
      System.out.println("time = " + time + "s");  
} } 
