function [g2, bic] = mctest2(d,n)
% PURPOSE: function called by raftery.m
% ------------------------------------------------
% SEE ALSO: coda(), prt()
% ------------------------------------------------
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

 m1 = zeros(2,2); m2 = zeros(2,2); g2 = 0.0; bic = 0.0;
 for i=3:n;      % count states
 i1 = d(i-2,1)+1; i2 = d(i-1,1)+1; i3 = d(i,1)+1;
  if (i3 == 1); m1(i1,i2) = m1(i1,i2) + 1; end;
  if (i3 == 2); m2(i1,i2) = m2(i1,i2) + 1; end;
 end;       % end of for i
 for i1 = 1:2;
  for i2 = 1:2;
   for i3 = 1:2;
   if (i3 == 1);
    if (m1(i1,i2) ~= 0);
    t1 = m1(i1,i2)+m2(i1,i2); t2 = m1(1,i2)+m1(2,i2);
    t3 = m1(1,i2)+m2(1,i2); t4 = m1(2,i2)+m2(2,i2);
    fitted = (t1*t2)/(t3+t4); focus = m1(i1,i2); 
    g2 = g2 + log(focus/fitted)*focus; 
    end;      % end of if m1   
   end;       % end of if i3
   if (i3 == 2);
    if (m2(i1,i2) ~= 0);
    t1 = m1(i1,i2) + m2(i1,i2); t2 = m2(1,i2) + m2(2,i2);
    t3 = m1(1,i2) + m2(1,i2); t4 = m1(2,i2) + m2(2,i2);
    fitted = (t1*t2)/(t3+t4); focus = m2(i1,i2); 
    g2 = g2 + log(focus/fitted)*focus;
    end;      % end of if m2  
   end;        % end of if i3
   end;       % end of for i3 
  end;        % end of for i2 
 end;         % end of for i1
 g2 = g2*2.0; bic = g2 - log(n - 2.0)*2.0;