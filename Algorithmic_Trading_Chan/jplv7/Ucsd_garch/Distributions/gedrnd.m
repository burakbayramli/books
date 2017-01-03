function random = gedrnd(n,nu) 
% PURPOSE:
%     Generates Deviates from the Generalized Error Distribution 
%     This is the same as the Exponential Power Distn with the single exception
% 
% 
% USAGE:
%     random = gedrnd(t,nu) 
% 
% 
% INPUTS:
%     t is the number of deviates wanted 
%     nu is the shape parameter
% 
% 
% OUTPUTS:
%     random - t by 1 vector of deviates
% 
% COMMENTS:
%     NOTE nu must be greater than 1
%     All deviates are from distribition with unit variance
% 
% 
%     Uses an acceptance rejection method
% 
%     The Generalized Error dist'n pdf is given by:
%     f(x)=Kd * exp (-|x|^nu)/((gamma(3/nu)/gamma(1/nu))^0.5)
%     KD = inv(2 * gamma (1+(1/nu) ) )
% 
%     Taken from Tadikamalla 1980
% 
%     Included in the ucsd_garch toolbox and the JPL library
%     Requires the JPL toolbox
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

stddev=(gamma(3/nu)/gamma(1/nu))^(0.5);
invstd=inv(stddev);

A = 1/nu;
B = A^A;
dummy =1;
random=zeros(n,1);


if nu>1 & nu<2
   for i=1:n
      while dummy == 1
         u = rand;
         if u > .5
            x=B*(-log(2*(1-u)));
         else
            x=B*log(2*u);
         end
         if log(rand)<=(-abs(x)^nu + abs(x)/B - 1 + A)
            random(i)=invstd*x;
            break
         end
      end
   end
else if nu>=2
      for i=1:n
         while dummy == 1
            x = B*randn;
            if log(rand)<=(-abs(x)^nu + x^2/(2*(B^2)) - 0.5 + A)
               random(i)=invstd*x;
               break
            end
         end
      end   
   end
end
