



function y = bigf(x)

%   data choice 1)
      y = .5*(x > 0).*(exp(-x) -1) + .5*(x<= 0).*(exp(x) -1);

%   data choice 2)
%      y = (x < -2) + ((x < 2) - (x < -2)).*(-x./2) - (x > 1.999);



