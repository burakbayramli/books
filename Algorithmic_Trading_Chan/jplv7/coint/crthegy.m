function crit = crthegy(type,det,nobs,sig)
% PURPOSE: return critical values for the statistics used in 
%          Hylleberg, Engle, Granger and Yoo(1990) and 
%          Ghysels, Lee and Noh (1994)
% ---------------------------------------------------------
% USAGE:   crit=  crthegy(type,det,nobs,sig)
% where:  'type' = component tested in the algorithm
%                   ('pi1', 'pi2', 'pi3', 'pi4', 'theta14',
%                  'theta24', 'theta34')
%            det = 1, no deterministic components
%                = 2, constant (default)
%                = 3, constant & 3 seasonal dummies
%                = 4, constant & trend
%                = 5, constant & 3 seasonal dummies & trend
%          nobs  = # of observations
%          sig   = significance level for all test 
%                  [1% 5% 10% 90% 95% 99%] 
%---------------------------------------------------------          
% RETURNS: crit = a critical values
% ---------------------------------------------------------                                
% References: Hylleberg, Engle, Granger and Yoo(1990), Journal of
%             Econometrics, 44, pg 215-38
%             Ghysels, Lee and Noh (1994), Journal of Econometrics
%             62, 415-442

%Written by:
% Carlos Alberto Castro
% National Planning Department
% Bogota, Colombia
% Email: ccastro@dnp.gov.co & ccastroir@cable.net.co

if (nargin ~= 4)
 error('Wrong # of arguments to crtd');
end;

switch type
         
case {'pi1'}
    pflag=1;
   
   %det=1 (No Intercept, No Seasonal Dummy, No Trend)
   %rows  48  100  136  200
         %columns  1%   2.5%    5%        10%    (size of the test)
   zt(:,:,1) = [ -2.72	-2.29	-1.95	-1.59								
                 -2.60	-2.26	-1.97	-1.61								
                 -2.62	-2.25	-1.93	-1.59								
                 -2.62	-2.23	-1.94	-1.62];
   %det=2 (Intercept, No Seasonal Dummy, No Trend)             
   %rows  48  100  136  200
         %columns  1%   2.5%    5%        10%    (size of the test)
   zt(:,:,2) = [ -3.66	-3.25	-2.96	-2.62								
                 -3.47	-3.14	-2.88	-2.58								
                 -3.51	-3.17	-2.89	-2.58								
                -3.48	-3.13	-2.87	-2.57];
   %det=3 (Intercept, Seasonal Dummy, No Trend)             
   %rows  48  100  136  200
         %columns  1%   2.5%    5%        10%    (size of the test)
   zt(:,:,3) = [ -3.77	-3.39	-3.08	-2.72
                 -3.55	-3.22	-2.95	-2.63
                 -3.56	-3.23	-2.94	-2.62
                 -3.51	-3.18	-2.91	-2.59];
   %det=4 (Intercept, No Seasonal Dummy, Trend)             
   %rows  48  100  136  200
         %columns  1%   2.5%    5%        10%    (size of the test)
   zt(:,:,4) = [-4.23	-3.85	-3.56	-3.21
                -4.07	-3.73	-3.47	-3.16
                -4.09	-3.75	-3.46	-3.16
                -4.05	-3.70	-3.44	-3.15];
   
   %det=5 (Intercept, Seasonal Dummy, Trend)             
   %rows  48  100  136  200
         %columns  1%   2.5%    5%        10%    (size of the test)
   zt(:,:,5) = [-4.46	-4.04	-3.71	-3.37
                -4.09	-3.80	-3.53	-3.32
                -4.15	-3.80	-3.52	-3.21
                -4.05	-3.74	-3.49	-3.18];
            
case {'pi2'}
    pflag=2;
   
   %det=1 (No Intercept, No Seasonal Dummy, No Trend)
   %rows  48  100  136  200
         %columns  1%   2.5%    5%        10%    (size of the test)
   zt(:,:,1) = [ -2.67	-2.27	-1.95	-1.60
                 -2.61	-2.22	-1.92	-1.57
                 -2.60	-2.23	-1.94	-1.61
                 -2.60	-2.24	-1.95	-1.61];
   %det=2 (Intercept, No Seasonal Dummy, No Trend)             
   %rows  48  100  136  200
         %columns  1%   2.5%    5%        10%    (size of the test)
   zt(:,:,2) = [ -2.68	-2.27	-1.95	-1.60
                 -2.61	-2.24	-1.95	-1.60
                 -2.60	-2.21	-1.91	-1.58
                 -2.58	-2.22	-1.92	-1.59];
   %det=3 (Intercept, Seasonal Dummy, No Trend)             
   %rows  48  100  136  200
         %columns  1%   2.5%    5%        10%    (size of the test)
   zt(:,:,3) = [ -3.75	-3.37	-3.04	-2.69
                 -3.60	-3.22	-2.94	-2.63
                 -3.49	-3.15	-2.90	-2.59
                 -3.50	-3.16	-2.89	-2.60];
   %det=4 (Intercept, No Seasonal Dummy, Trend)             
   %rows  48  100  136  200
         %columns  1%   2.5%    5%        10%    (size of the test)
   zt(:,:,4) = [-2.65	-2.24	-1.91	-1.57
                -2.58	-2.24	-1.94	-1.60
                -2.65	-2.25	-1.96	-1.63
                -2.59	-2.25	-1.95	-1.62];
   %det=5 (Intercept, Seasonal Dummy, Trend)             
   %rows  48  100  136  200
         %columns  1%   2.5%    5%        10%    (size of the test)
   zt(:,:,5) = [-3.80	-3.41	-3.08 	-2.73
                -3.60	-3.22	-2.94	-2.63
                -3.57	-3.18	-2.93	-2.61
                -3.52	-3.18	-2.91	-2.60];
            
  case {'pi3'}
      pflag=3;
   
   %det=1 (No Intercept, No Seasonal Dummy, No Trend)
   %rows  48  100  136  200
         %columns  1%   2.5%    5%        10%    (size of the test)
   zt(:,:,1) = [ -2.66	-2.23	-1.93	-1.52
                 -2.55	-2.18	-1.90	-1.53
                 -2.58	-2.21	-1.92	-1.56
                 -2.58	-2.34	-1.92	-1.55];
   %det=2 (Intercept, No Seasonal Dummy, No Trend)             
   %rows  48  100  136  200
         %columns  1%   2.5%    5%        10%    (size of the test)
   zt(:,:,2) = [-2.64	-2.23	-1.90	-1.55
                -2.61	-2.23	-1.90	-1.54
                -2.53	-2.18	-1.88	-1.53
                -2.57	-2.21	-1.90	-1.53];
   %det=3 (Intercept, Seasonal Dummy, No Trend)             
   %rows  48  100  136  200
         %columns  1%   2.5%    5%        10%    (size of the test)
   zt(:,:,3) = [-4.31	-3.92	-3.61	-3.24
                -4.06	-3.72	-3.44	-3.14
                -4.06	-3.72	-3.44	-3.11
                -4.00	-3.67	-3.38	-3.07];
   %det=4 (Intercept, No Seasonal Dummy, Trend)             
   %rows  48  100  136  200
         %columns  1%   2.5%    5%        10%    (size of the test)
   zt(:,:,4) = [-2.68	-2.27	-1.92	-1.52
                -2.56	-2.19	-1.89	-1.54
                -2.56	-2.20	-1.90	-1.52
                -2.58	-2.21	-1.92	-1.56];
   %det=5 (Intercept, Seasonal Dummy, Trend)             
   %rows  48  100  136  200
         %columns  1%   2.5%    5%        10%    (size of the test)
   zt(:,:,5) = [-4.46	-4.02	-3.66	-3.28
                -4.12	-3.76	-3.48	-3.14
                -4.05	-3.72	-3.44	-3.12
                -4.04	-3.69	-3.41	-3.10];
            
  case {'pi4'}
      pflag=4;
   
   %det=1 (No Intercept, No Seasonal Dummy, No Trend)
   %rows  48  100  136  200
         %columns  1%   2.5%      5%     10%      90%   95%     97.5%   99% (size of the test)
   zt(:,:,1) = [ -2.51	-2.11	-1.76	-1.35	1.33	1.72	2.05	2.49
                 -2.43	-2.01	-1.68	-1.32	1.31	1.67	2.00	2.40
                 -2.44	-1.99	-1.68	-1.31	1.30	1.66	1.99	2.38
                 -2.43	-1.98	-1.65	-1.30	1.29	1.67	1.97	2.36];
   %det=2 (Intercept, No Seasonal Dummy, No Trend)             
   %rows  48  100  136  200
         %columns  1%   2.5%      5%     10%      90%   95%     97.5%   99% (size of the test)
   zt(:,:,2) = [-2.44	-2.06	-1.72	-1.33	1.30	1.68	2.04	2.41
                -2.38	-1.99	-1.68	-1.30	1.28	1.65	1.97	2.32
                -2.36	-1.98	-1.68	-1.31	1.27	1.65	1.97	2.31
                -2.36	-1.98	-1.66	-1.29	1.28	1.65	1.96	2.30];
   %det=3 (Intercept, Seasonal Dummy, No Trend)             
   %rows  48  100  136  200
         %columns  1%   2.5%      5%     10%      90%   95%     97.5%   99% (size of the test)
   zt(:,:,3) = [-2.86	-2.37	-1.98	-1.53	1.54	1.96	2.35	2.81
                -2.78	-2.32	-1.96	-1.53	1.52	1.93	2.29	2.73
                -2.72	-2.31	-1.96	-1.52	1.51	1.92	2.28	2.71        
                -3.74	-2.33	-1.96	-1.54	1.53	1.95	2.32	2.78];
   %det=4 (Intercept, No Seasonal Dummy, Trend)             
   %rows  48  100  136  200
         %columns  1%   2.5%      5%     10%      90%   95%     97.5%   99% (size of the test)
   zt(:,:,4) = [-2.41	-2.05	-1.70	-1.33	1.26	1.64	1.96	2.37
                -3.38	-1.97	-1.65	-1.28	1.28	1.65	1.98	2.32
                -2.36	-1.97	-1.64	-1.29	1.26	1.62	1.92	2.31
                -2.35	-1.97	-1.66	-1.29	1.26	1.64	1.96	2.30];
   %det=5 (Intercept, Seasonal Dummy, Trend)             
   %rows  48  100  136  200
         %columns  1%   2.5%    5%        10%   90%   95%  97.5%   99% (size of the test)
   zt(:,:,5) = [-2.75	-2.26	-1.91	-1.48	1.51	1.97	2.34	2.78
                -2.76	-2.32	-1.94	-1.51	1.51	1.92	2.28	2.69
                -2.71	-2.78	-1.94	-1.51	1.53	1.96	2.31	2.78
                -2.65	-2.27	-1.92	-1.48	1.55	1.97	2.31	2.71];
  
  case {'theta14'}
      pflag=5;
   
   %det=1 (No Intercept, No Seasonal Dummy, No Trend)
   %rows  48  100  160  200  400
      %columns  10%        5%    (size of the test)
   zt(:,:,1) = [2.11	2.62
                2.11	2.55
                2.06	2.53
                2.12	2.56
                2.07	2.53];
   %det=2 (Intercept, No Seasonal Dummy, No Trend)             
   %rows  48  100  160  200  400
      %columns  10%        5%    (size of the test)
   zt(:,:,2) = [2.86	3.47
                2.86	3.37
                2.81	3.38
                2.81	3.32
                2.79	3.29];
   %det=3 (Intercept, Seasonal Dummy, No Trend)             
   %rows  48  100  160  200  400
      %columns  10%        5%    (size of the test)
   zt(:,:,3) = [5.02	5.96
                4.95	5.74
                4.90	5.69
                4.87	5.58
                4.86	5.55];
   %det=4 (Intercept, No Seasonal Dummy, Trend)             
   %rows  48  100  160  200  400
      %columns  10%        5%    (size of the test)
   zt(:,:,4) = [3.60	4.28
                3.58	4.26
                3.55	4.13
                3.55	4.12
                3.54	4.09];
   %det=5 (Intercept, Seasonal Dummy, Trend)             
   %rows  48  100  160  200  400
      %columns  10%        5%    (size of the test)
   zt(:,:,5) = [5.71	6.53
                5.68	6.47
                5.56	6.33
                5.61	6.38
                5.55	6.27];
            
case {'theta24'}
    pflag=6;
   
   %det=1 (No Intercept, No Seasonal Dummy, No Trend)
   %rows  48  100  160  200  400
      %columns  10%        5%    (size of the test)
   zt(:,:,1) = [2.21	2.80	
                2.21	2.76	
                2.18	2.72	
                2.22	2.73	
                2.19	2.76];
   %det=2 (Intercept, No Seasonal Dummy, No Trend)             
   %rows  48  100  160  200  400
      %columns  10%        5%    (size of the test)
   zt(:,:,2) = [2.17	2.63	
                2.18	2.74	
                2.21	2.74	
                2.19	2.75	
                2.15	2.69];
   %det=3 (Intercept, Seasonal Dummy, No Trend)             
   %rows  48  100  160  200  400
      %columns  10%        5%    (size of the test)
   zt(:,:,3) = [5.16	6.60
                5.18	6.05
                5.16	6.04
                5.14	5.99
                5.15	5.95];
   %det=4 (Intercept, No Seasonal Dummy, Trend)             
   %rows  48  100  160  200  400
      %columns  10%        5%    (size of the test)
   zt(:,:,4) = [2.14	2.67
                2.15	2.76
                2.15	2.75
                2.17	2.72
                2.16	2.70];
   %det=5 (Intercept, Seasonal Dummy, Trend)             
   %rows  48  100  160  200  400
      %columns  10%        5%    (size of the test)
   zt(:,:,5) = [5.13	6.09
                5.13	5.99
                5.14	5.91
                5.10	5.89
                5.1	    5.9];
  
case {'theta34'}
    pflag=7;
   
   %det=1 (No Intercept, No Seasonal Dummy, No Trend)
   %rows  48  100  136  200
      %columns  10%      5%      2.5%    1% (size of the test)
   zt(:,:,1) = [2.45	3.26	4.04	5.02	
                2.39	3.12	3.89	4.89	
                2.41	3.14	3.86	4.81	
                2.42	3.16	3.92	4.81];
   %det=2 (Intercept, No Seasonal Dummy, No Trend)             
   %rows  48  100  136  200
      %columns  10%      5%      2.5%    1% (size of the test)
   zt(:,:,2) = [2.32	3.04	3.78	4.78	
                2.35	3.08	3.81	4.77	
                2.36	3.00	3.70	4.73	
                2.37	3.12	3.86	4.76];
   %det=3 (Intercept, Seasonal Dummy, No Trend)             
   %rows  48  100  136  200
      %columns  10%      5%      2.5%    1% (size of the test)
   zt(:,:,3) = [5.50	6.60	7.68	9.22	
                5.56	6.57	7.72	8.74	
                5.56	6.63	7.66	8.92	
                5.56	6.61	7.53	8.93];
   %det=4 (Intercept, No Seasonal Dummy, Trend)             
   %rows  48  100  136  200
      %columns  10%      5%      2.5%    1% (size of the test)
   zt(:,:,4) = [2.23	2.95	3.70	4.64	
                2.31	2.98	3.71	4.70	
                2.33	3.04	3.69	4.57	
                2.34	3.07	3.76	4.66];
   %det=5 (Intercept, Seasonal Dummy, Trend)             
   %rows  48  100  136  200
      %columns  10%      5%      2.5%    1% (size of the test)
   zt(:,:,5) = [5.37	6.55	7.70	9.27	
                5.52	6.60	7.52	8.79	
                5.55	6.62	7.59	8.77	
                5.56	6.57	7.56	8.96];
            
        
        otherwise
        error('Wrong type');
        
    end
     
    
    %No. of Observations
    
    %recode strings
    %pflag String
    %1      pi1
    %2      pi2
    %3      pi3
    %4      pi4
    %5      theta14
    %6      theta24
    %7      theta34
    
    if pflag==1|pflag==2|pflag==3|pflag==4|pflag==7
    %rows  48  100  136  200
    if (nobs>=0 & nobs<=48); 
           r=1;
           c=asgc(sig,pflag);
    elseif (nobs>49 & nobs<=100); 
           r=2;
           c=asgc(sig,pflag);
    elseif (nobs>101 & nobs<=136); 
           r=3;
           c=asgc(sig,pflag);
    elseif (nobs>137 & nobs<=200); 
           r=4;
           c=asgc(sig,pflag);
    else
           error('Wrong nobs');
       end;
        
    else if pflag==5|pflag==6
   %rows  48  100  160  200  400
    if (nobs>=0 & nobs<=48); 
           r=1;
           c=asgc(sig,pflag);
    elseif (nobs>49 & nobs<=100); 
           r=2;
           c=asgc(sig,pflag);
    elseif (nobs>101 & nobs<=160); 
           r=3;
           c=asgc(sig,pflag);
    elseif (nobs>161 & nobs<=200); 
           r=4;
           c=asgc(sig,pflag);
    elseif (nobs>201 & nobs<=400);
           r=5;
           c=asgc(sig,pflag);
    else
           error('Wrong nobs');
       end;
    
   else
       error('Wrong type');
       
   end; 
  
             
    end
    
    %critical value
        crit = zt(r,c,det);
    
    
    function c = asgc(sig,pflag)
    % define column
    if pflag==1|pflag==2|pflag==3
    %columns  1%   2.5%    5%        10%    (size of the test)
     switch sig 
          case 0.01
              c=1;
          case 0.025
              c=2;
          case 0.05
              c=3;
          case 0.1
              c=4;
          otherwise
            error('Wrong sig');
        end;
            
    elseif  pflag==4

     %columns  1%   2.5%      5%     10%      90%   95%     97.5%   99% (size of the test)
     switch sig 
          case 0.01
              c=1;
          case 0.025
              c=2;
          case 0.05
              c=3;
          case 0.1
              c=4;
          case 0.9
              c=5;
          case 0.95
             c=6;
          case 0.975
             c=7;
          case 0.99
             c=8;
         otherwise
            error('Wrong sig');
        end;
        
       
    elseif pflag==7

     %columns  10%      5%      2.5%    1% (size of the test)
         switch sig 
          case 0.1
              c=1;
          case 0.05
             c=2;
          case 0.025
             c=3;
          case 0.01
             c=4;
         otherwise
            error('Wrong sig');
        end;
        
    elseif pflag==5|pflag==6
        %columns  10%        5%    (size of the test)
    switch sig 
          case 0.1
              c=1;    
          case 0.05
              c=2;
          otherwise
            error('Wrong sig');
        end;
         
    else
        error('Wrong type');
    end
    
                   
           
           
           
           
           