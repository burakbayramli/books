% Kriging example 
% 2D spatial scenario

% Place observation points
N=25; %number of points
R=6; %distance parameter
x=R*rand(N,1); y=R*rand(N,1);

% Compute C matrix
sd=1; %standard deviation
ksd=(sd*sd)/2;
for i=1:N,
   for j=i:N,
      C(i,j)=exp(-ksd*((x(i)-x(j))^2+(y(i)-y(j))^2)); %upper triangle
      C(j,i)=C(i,j); %symmetric lower triangle
   end;
end;

ivC=inv(C); %the inverse of C

% Assign values to the observation points
% according with a 2D function
f=100; A=randn(1,f)*sqrt(2/f);
beta=2*pi*rand(f,1); k=sd*randn(f,2);

for nn=1:N,
   z(nn,1)=A*cos(k*[x(nn);y(nn)]+ beta);
end;

% Set a grid of points where values will be predicted
gs=[40 40]; gr=[0 R 0 R]; %size and range of the grid
ivl=(gr(2)-gr(1))/(gs(1)-1); ivh=(gr(4)-gr(3))/(gs(2)-1);
[xp,yp]=ndgrid(gr(1):ivl:gr(2), gr(3):ivh:gr(4));

% kriging computations
for i=1:gs(1),
   for j=1:gs(2),
      zf(i,j)=A*cos(k*[xp(i,j); yp(i,j)]+beta); %values of 2D function
      for nn=1:N,
         Co(1,nn)=exp(-ksd*((x(nn)-xp(i,j))^2+(y(nn)-yp(i,j))^2));
      end;
      zp(i,j)=Co*ivC*z; % predicted values at (xp,yp) 
   end;
end;

% display
figure(1)
plot3(x,y,z,'r*','MarkerSize',10); hold on;
mesh(xp,yp,zp);
grid;
view(-20,30);
title('Kriging: observed values and interpolated surface')



