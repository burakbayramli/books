

function y = f(x)

%     y = exp( -.5.*x.^2);
 
%      y = cos(x.^2)./(1+x.^4);

%      y = exp(-abs(x));


%       y = 1./(1+x.^2);

%         y = (x < 1) - (x < -1);

      y = 4*(sin(x)./x.^3 - cos(x)./x.^2 )/(2*pi);
