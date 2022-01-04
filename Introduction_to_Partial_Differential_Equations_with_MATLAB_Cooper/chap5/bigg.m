




function y = bigg(x)

%  data choice 1)
%       y = (x > 0).*(1 - exp(-x)) + (x < 0).*(exp(x) -1);

%  data choice 2)
%      y = -(x < -2) +( (x < 2 ) - (x < -2)).*(x/2) + (x > 1.9999);

%  data choice 3)

      y = (x-5).*(x-6).*exp(-(x-5).^2 )/.7635 ;



