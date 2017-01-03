function random = exppowrnd(x,nu) 
% PURPOSE:
%     Random numbers from an exponential power distribution
% USAGE:  
%     random = exppowrnd(t,nu) 
% 
% INPUTS:
%     t   Number of random numbers to return
%     nu  A scalar shape parameer
% 
% OUTPUTS:
%     random:  T by 1 vectos fo random numbers
% 
% COMMENTS:
%     NOTE nu must be greater than 1
% 
%     Uses an acceptance rejection method
% 
%     The exponential power dist'n pdf is given by:
% 
%     f(x)=Kd * exp (-|x|^nu)
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



if (length(x)>1 | length(nu)>1 | nu<1)
    error('Error in arguements')
end



A = 1/nu;
B = A^A;
dummy =1;

random=zeros(x,1);

for i=1:x
   while dummy == 1
      u = rand;
      if u > .5
         x=B*(-log(2*(1-u)));
      else
         x=B*log(2*u);
      end
      if log(rand)<=(-abs(x)^alpha + abs(x)/B - 1 + A)
         random(i)=x;
         break
      end
   end
end

   
      