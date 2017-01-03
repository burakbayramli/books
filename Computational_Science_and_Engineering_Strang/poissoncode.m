%3.4  poissoncode.m

function u=poisson(x,y)   % choose evaluation points for u(x,y)
                          % x = and y = choose 1 point or more 
N=39; u=zeros(size(x));   % size=1,1 for evaluation at 1 point
if nargin==1              % Only x coordinates so 1D problem
  for k=1:2:N             % Add N terms in the 1D sine series
    u = u + 2^2/pi^3/k^3*sin(k*pi*x);
  end
% xx=0:.01:1;yy=poisson(xx);plot(xx,yy) to plot u in 1D (2D is below)

elseif nargin==2          % x and y coordinates so 2D problem
  for i=1:2:N             % - u_xx - u_yy = 1 in unit square
    for j=1:2:N           % Add N^2 terms in the 2D sine series
      u = u + 2^4/pi^4/(i*j)/(i^2+j^2)*sin(i*pi*x).*sin(j*pi*y);
    end;                
  end;                    % 3D would have (i*j*k)/(i^2+j^2+k^2)
end
% [xx,yy]=meshgrid(0:.1:1,0:.1:1);zz=poisson(xx,yy);contourf(xx,yy,zz)
