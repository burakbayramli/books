function jc = c_sjt(n,p)
% PURPOSE: find critical values for Johansen trace statistic
% ------------------------------------------------------------
% USAGE:  jc = c_sjt(n,p)
% where:    n = dimension of the VAR system
%               NOTE: routine doesn't work for n > 12
%           p = order of time polynomial in the null-hypothesis
%                 p = -1, no deterministic part
%                 p =  0, for constant term
%                 p =  1, for constant plus time-trend
%                 p >  1  returns no critical values
% ------------------------------------------------------------
% RETURNS: a (3x1) vector of percentiles for the trace 
%          statistic for [90% 95% 99%] 
% ------------------------------------------------------------
% NOTES: for n > 12, the function returns a (3x1) vector of zeros.
%        The values returned by the function were generated using
%        a method described in MacKinnon (1996), using his FORTRAN
%        program johdist.f                                 
% ------------------------------------------------------------
% SEE ALSO: johansen()
% ------------------------------------------------------------
% % References: MacKinnon, Haug, Michelis (1996) 'Numerical distribution 
% functions of likelihood ratio tests for cointegration', 
% Queen's University Institute for Economic Research Discussion paper.
% -------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

% these are the values from Johansen's 1995 book
% for comparison to the MacKinnon values
%jcp0 = [ 2.98   4.14   7.02
%        10.35  12.21  16.16
%        21.58  24.08  29.19
%        36.58  39.71  46.00
%        55.54  59.24  66.71
%        78.30  86.36  91.12
%       104.93 109.93 119.58
%       135.16 140.74 151.70
%       169.30 175.47 187.82
%       207.21 214.07 226.95
%       248.77 256.23 270.47
%       293.83 301.95 318.14];
%


jcp0 = [ 2.9762   4.1296   6.9406
        10.4741  12.3212  16.3640
        21.7781  24.2761  29.5147
        37.0339  40.1749  46.5716
        56.2839  60.0627  67.6367
        79.5329  83.9383  92.7136
       106.7351 111.7797 121.7375
       137.9954 143.6691 154.7977
       173.2292 179.5199 191.8122
       212.4721 219.4051 232.8291
       255.6732 263.2603 277.9962
       302.9054 311.1288 326.9716];


jcp1 = [  2.7055   3.8415   6.6349
         13.4294  15.4943  19.9349
         27.0669  29.7961  35.4628
         44.4929  47.8545  54.6815
         65.8202  69.8189  77.8202
         91.1090  95.7542 104.9637
        120.3673 125.6185 135.9825
        153.6341 159.5290 171.0905
        190.8714 197.3772 210.0366
        232.1030 239.2468 253.2526
        277.3740 285.1402 300.2821
        326.5354 334.9795 351.2150];

jcp2 = [   2.7055   3.8415   6.6349
          16.1619  18.3985  23.1485
          32.0645  35.0116  41.0815
          51.6492  55.2459  62.5202
          75.1027  79.3422  87.7748
         102.4674 107.3429 116.9829
         133.7852 139.2780 150.0778
         169.0618 175.1584 187.1891
         208.3582 215.1268 228.2226
         251.6293 259.0267 273.3838
         298.8836 306.8988 322.4264
         350.1125 358.7190 375.3203];


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
   