clear all;

xi=[-sqrt(3/5) 0 sqrt(3/5)]; % 3 point Gauss quadrature
wi=[5/9 8/9 5/9];            % weights
HL=1/2;                      % half of the interval

% next block: compute the Gauss quadrature
I=0;
for i=1:3
  x=0.5 + xi(i)*0.5;
  for j=1:3
    y=0.5 + xi(j)*0.5;
    wxy=wi(i)*wi(j)*0.5^2;
    I=I+exp(x*y)*wxy;
  end
end

I

Ie=1+1/4+1/18+1/96+1/600+ 1/(6*720)  % exact result