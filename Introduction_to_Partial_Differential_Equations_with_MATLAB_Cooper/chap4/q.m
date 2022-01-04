

function y = q(x)

%     y = exp(-(x-5).^2);

%      y = 0;

      y = (x < 5).*exp(-2*(x-2).^2) - 2*(x >=5).*exp(-8*(x-7).^2); 
