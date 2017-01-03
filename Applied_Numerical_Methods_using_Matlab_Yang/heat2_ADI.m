function [u,x,y,t]=heat2_ADI(a,D,T,ixy0,bxyt,Mx,My,N)
%solve u_t = c(u_xx +u_yy) for D(1)<=x<=D(2), D(3)<=y<=D(4), 0<=t<=T
% Initial Condition: u(x,y,0) = ixy0(x,y)
% Boundary Condition: u(x,y,t) = bxyt(x,y,t) for (x,y)cB
% Mx/My = # of subintervals along x/y-axis
% N = # of subintervals along t-axis
dx= (D(2)-D(1))/Mx; x= D(1)+[0:Mx]*dx; 
dy= (D(4)-D(3))/My; y= D(3)+[0:My]'*dy; 
dt= T/N;  t= [0:N]*dt;
%Initialization
for j=1:Mx+1
  for i=1:My+1
    u(i,j)= ixy0(x(j),y(i));
  end
end
rx= a*dt/(dx*dx); rx1=1+2*rx; rx2=1-2*rx;
ry= a*dt/(dy*dy); ry1=1+2*ry; ry2=1-2*ry;
for j=1:Mx-1 %Eq.(9.2-22a)
  Ay(j,j)= ry1; 
  if j>1, Ay(j-1,j)= -ry; Ay(j,j-1)= -ry; end
end
for i=1:My-1 %Eq.(9.2-22b)
  Ax(i,i)= rx1; 
  if i>1, Ax(i-1,i)= -rx; Ax(i,i-1)= -rx; end
end
for k=1:N
  u_1=u;   t=k*dt;
  for i=1:My+1 %Boundary condition
    u(i,1)= feval(bxyt,x(1),y(i),t);
    u(i,Mx+1)= feval(bxyt,x(Mx+1),y(i),t);
  end
  for j=1:Mx+1
    u(1,j)= feval(bxyt,x(j),y(1),t);
    u(My+1,j)= feval(bxyt,x(j),y(My+1),t);
  end
  if mod(k,2)==0
    for i=2:My
      jj=2:Mx;
      bx= [ry*u(i,1) zeros(1,Mx-3) ry*u(i,My+1)] ...
               +rx*(u_1(i-1,jj)+u_1(i+1,jj)) +rx2*u_1(i,jj);
      u(i,jj)= trid(Ay,bx')'; %Eq.(9.2-22a)
    end
   else
    for j=2:Mx
      ii=2:My;
      by= [rx*u(1,j); zeros(My-3,1); rx*u(Mx+1,j)] ...
               +ry*(u_1(ii,j-1)+u_1(ii,j+1)) +ry2*u_1(ii,j);
      u(ii,j)= trid(Ax,by); %Eq.(9.2-22b)
    end
  end
end
