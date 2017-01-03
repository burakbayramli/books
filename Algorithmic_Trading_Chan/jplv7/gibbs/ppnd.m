function y = ppnd(p)
% PURPOSE: determines quantiles of the cumulative standard normal
% ------------------------------------------------------------
% USAGE: called by nmin.m and raftery.m
% where: s = probability associated with r
% ------------------------------------------------------------
% NOTES: based on algorithm AS 241 Applied Statistics (1988)
%        Volume 37, no. 3, pp. 477-484
% ------------------------------------------------------------
% REFERENCES: Geweke (1992), `Evaluating the accuracy of sampling-based
% approaches to the calculation of posterior moments', in J.O. Berger,
% J.M. Bernardo, A.P. Dawid, and A.F.M. Smith (eds.) Proceedings of
% the Fourth Valencia International Meeting on Bayesian Statistics,
% pp. 169-194, Oxford University Press
% Also: `Using simulation methods for Bayesian econometric models: 
% Inference, development and communication', at: www.econ.umn.edu/~bacc
% -----------------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu
 
% NOTE: this code draws heavily on MATLAB programs written by
% Siddartha Chib available at: www.econ.umn.edu/~bacc
% I have repackaged it to make it easier to use.

   
 split1 = 0.425;  split2 = 5.0;
 const1 = 0.180625; const2 = 1.6;
 a0=3.3871327179e+00; a1=5.0434271938e+01; a2=1.5929113202e+02;
 a3=5.9109374720e+01; b1=1.7895169469e+01; b2=7.8757757664e+01;
 b3=6.7187563600e+01; c0=1.4234372777e+00; c1=2.7568153900e+00;
 c2=1.3067284816e+00; c3=1.7023821103e-01; d1=7.3700164250e-01;
 d2=1.2021132975e-01; e0=6.6579051150e+00; e1=3.0812263860e+00;
 e2=4.2868294337e-01; e3=1.7337203997e-02; f1=2.4197894225e-01;
 f2=1.2258202635e-02; 
 q = p - 0.5;
  
    if (abs(q) <= split1),
     r = const1 - q * q;
  y = q * (((a3*r+a2)*r+a1)*r+a0)/(((b3*r+b2)*r+b1)*r+1.0);
  return;
 elseif  (q < 0.0),
  r = p; 
 else,
  r = 1 - p;
 end;     % end of if
 if (r <= 0.0);
   y = 0.0;
   return;
 end;     % end of if
 
 r = sqrt(-log(r));
 
 if (r <= split2);
  r = r - const2;
  y = (((c3*r+c2)*r+c1)*r+c0)/((d2*r+d1)*r+1.0);
 else,
  r = r - split2;
  y = (((e3*r+e2)*r+e1)*r+e0)/((f2*r+f1)*r+1.0);
 end;      % end of if; 
 if (q < 0.0);
  y = -y;
  return; 
    end;      % end of if

