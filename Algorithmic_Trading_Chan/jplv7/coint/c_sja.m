function jc =  c_sja(n,p)
% PURPOSE: find critical values for Johansen maximum eigenvalue statistic
% ------------------------------------------------------------
% USAGE:  jc = c_sja(n,p)
% where:    n = dimension of the VAR system
%           p = order of time polynomial in the null-hypothesis
%                 p = -1, no deterministic part
%                 p =  0, for constant term
%                 p =  1, for constant plus time-trend
%                 p >  1  returns no critical values
% ------------------------------------------------------------
% RETURNS: a (3x1) vector of percentiles for the maximum eigenvalue
%          statistic for: [90% 95% 99%]               
% ------------------------------------------------------------
% NOTES: for n > 12, the function returns a (3x1) vector of zeros.
%        The values returned by the function were generated using
%        a method described in MacKinnon (1996), using his FORTRAN
%        program johdist.f                        
% ------------------------------------------------------------
% SEE ALSO: johansen()
% ------------------------------------------------------------
% References: MacKinnon, Haug, Michelis (1996) 'Numerical distribution 
% functions of likelihood ratio tests for cointegration', 
% Queen's University Institute for Economic Research Discussion paper.
% -------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com



jcp0 = [ 2.9762  4.1296  6.9406
         9.4748 11.2246 15.0923
        15.7175 17.7961 22.2519
        21.8370 24.1592 29.0609
        27.9160 30.4428 35.7359
        33.9271 36.6301 42.2333
        39.9085 42.7679 48.6606
        45.8930 48.8795 55.0335
        51.8528 54.9629 61.3449
        57.7954 61.0404 67.6415
        63.7248 67.0756 73.8856
        69.6513 73.0946 80.0937];
  
jcp1 = [ 2.7055   3.8415   6.6349  
        12.2971  14.2639  18.5200  
        18.8928  21.1314  25.8650  
        25.1236  27.5858  32.7172  
        31.2379  33.8777  39.3693  
        37.2786  40.0763  45.8662  
        43.2947  46.2299  52.3069  
        49.2855  52.3622  58.6634  
        55.2412  58.4332  64.9960  
        61.2041  64.5040  71.2525  
        67.1307  70.5392  77.4877
        73.0563  76.5734  83.7105];
        
jcp2 = [ 2.7055   3.8415   6.6349        
        15.0006  17.1481  21.7465        
        21.8731  24.2522  29.2631
        28.2398  30.8151  36.1930
        34.4202  37.1646  42.8612
        40.5244  43.4183  49.4095
        46.5583  49.5875  55.8171
        52.5858  55.7302  62.1741
        58.5316  61.8051  68.5030
        64.5292  67.9040  74.7434
        70.4630  73.9355  81.0678
        76.4081  79.9878  87.2395];

   if ((p > 1) | (p < -1));
    jc = zeros(1,3);
   elseif ((n > 12) | (n < 1));
    jc = zeros(1,3);
   elseif p == -1
    jc = jcp0(n,:);
   elseif p == 0
    jc = jcp1(n,:);
   elseif p == 1
    jc = jcp2(n,:);
   end;
   
