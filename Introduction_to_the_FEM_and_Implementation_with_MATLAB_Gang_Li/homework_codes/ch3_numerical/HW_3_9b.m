clear all;

xi=[-sqrt(3/5) 0 sqrt(3/5)]; % 3 point Gauss quadrature
wi=[5/9 8/9 5/9];            % weights
HT=pi/4;                     % half of the angle interval
HR=0.5;                      % half of the radius interval
r0=(0+1)/2;                  % radius center point
theta0=(0+pi/2)/2;           % theta center point

% next block: compute the Gauss quadrature
I=0;
for i=1:3                 % radius direction
  r= r0 + xi(i)*HR;
  for j=1:3
    theta=theta0 + xi(j)*HT; % theta direction
    wxy=wi(i)*wi(j)*HR*HT;
    I=I+exp(r^2*sin(theta)*cos(theta))*r*wxy;
  end
end

I

Ie=0.9287  % exact result