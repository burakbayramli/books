


function y = g(x)

    y1 = - (pi*ones(size(x)) + x)/pi;
    y2 = (pi*ones(size(x)) -x)/pi ;




    y = (x< 0).*y1 + ( (x < 2*pi) -( x< 0) ).*y2;
 
%     y = cos(4*x);

%      y = .5*(exp(2*i*x) +exp(-2*i*x));

%      y = 5+exp(i*x) -2*exp(2*i*x) + 3*exp(3*i*x) -4*exp(4*i*x);
