mm=3;   % input parameter
nn=3;   % input parameter

% next block: set up the problem
nx=30;
ny=40;
a=3;
b=4;
q=-100;
E=70e9;
nu=0.3;
t=0.005;
dd=E*t^3/12/(1-nu^2);
D=[dd nu*dd 0
   nu*dd dd 0
    0 0 (1-nu)/2*dd]

% next block: meshgrid with zero deflection
[x y]=meshgrid(0:a/nx:a, 0:b/ny:b);
w=x*0;

% next block: calculate the deflection using the formula
for i=1:ny+1
  for j=1:nx+1
    xp=x(i,j);
    yp=y(i,j);
    for m=1:2:mm
      for n=1:2:nn 
        Qmn=16*q/pi^2/m/n;
        dmn=pi^4*(D(1,1)*m^4/a^4 ...
                 +2*(D(1,2)+2*D(3,3))*m^2*n^2/a^2/b^2 ...
                 + D(2,2)*n^4/b^4);
        Wmn=Qmn/dmn;
        alpha=m*pi/a;
        beta=n*pi/b;
        w(i,j)=w(i,j)+Wmn*sin(alpha*xp)*sin(beta*yp);
      end
    end
  end
end

% next block: plotting
figure(1)
clf;
plot([0:a/nx:a],w(ny/2+1,:)','b-');
hold on;
plot([0:b/ny:b],w(:,nx/2+1)','r-');
format long;
w(ny/2+1,nx/2+1)
format short;

%0.142*q*a^4/E/t^3/(2.21*(a/b)^3+1)  %alternative formular
%surf(x,y,w);