function [u,x,y]=poisson_Neuman(f,g,bx0,bxf,by0,byf,x0,xf,y0,yf,...)
.. .. .. .. .. .. .. ..
Neum=zeros(1,4); %Not Neumann, but Dirichlet condition by default
if length(x0)>1, Neum(1)=x0(2); x0=x0(1); end
if length(xf)>1, Neum(2)=xf(2); xf=xf(1); end
if length(y0)>1, Neum(3)=y0(2); y0=y0(1); end
if length(yf)>1, Neum(4)=yf(2); yf=yf(1); end
.. .. .. .. .. .. .. ..
dx_2=dx*dx; dy_2=dy*dy;  dxy2=2*(dx_2+dy_2);  
rx=dx_2/dxy2; ry=dy_2/dxy2; rxy=rx*dy_2; rx=rx; 
dx2=dx*2; dy2=dy*2; rydx=ry*dx2; rxdy=rx*dy2;
u(1:My1,1:Mx1)=zeros(My1,Mx1);

sum_of_bv= 0; num= 0;
if Neum(1)==0 %Dirichlet boundary condition
   for m=1:My1, u(m,1)= bx0(y(m)); end %side a
 else %Neumann boundary condition
  for m=1:My1, duxa(m)= bx0(y(m)); end %du/dx(x0,y)
end
if Neum(2)==0 %Dirichlet boundary condition
  .. .. .. .. .. .. .. .. .. .. .. 
end
if Neum(3)==0 %Dirichlet boundary condition
  n1=1; nM1=Mx1;
  if Neum(1)==0, u(1,1)=(u(1,1)+by0(x(1)))/2; n1=2; end
  if Neum(2)==0, u(1,Mx1)=(u(1,Mx1)+by0(x(Mx1)))/2; nM1=Mx; end
  for n=n1:nM1, u(1,n)= by0(x(n)); end %side c
 else %Neumann boundary condition
  for n=1:Mx1, duyc(n)= by0(x(n)); end %du/dy(x,y0)
end
if Neum(4)==0 %Dirichlet boundary condition
  .. .. .. .. .. .. .. .. .. .. ..
end
for itr=1:imax
  if Neum(1) %Neumann boundary condition
    for i=2:My
       u(i,1)= 2*ry*u(i,2) +rx*(u(i+1,1)+u(i-1,1)) ...
                  +rxy*(G(i,1)*u(i,1)-F(i,1))-rydx*duxa(i); %(9.1-9)
    end
    if Neum(3), u(1,1)= 2*(ry*u(1,2) +rx*u(2,1)) ...
     +rxy*(G(1,1)*u(1,1)-F(1,1))-rydx*duxa(1)-rxdy*duyc(1);%(9.1-11)
    end
    if Neum(4), u(My1,1)= 2*(ry*u(My1,2) +rx*u(My,1)) ...
     +rxy*(G(My1,1)*u(My1,1)-F(My1,1))+rxdy*duyd(1)-rydx*duxa(My1);
    end
  end
  if Neum(2) %Neumann boundary condition
.. .. .. .. .. .. .. .. .. .. .. .. .. 
end
  if Neum(3) %Neumann boundary condition
    for j=2:Mx
       u(1,j)= 2*rx*u(2,j)+ry*(u(1,j+1)+u(1,j-1)) ...
                 +rxy*(G(1,j)*u(1,j)-F(1,j))-rxdy*duyc(j); %(9.1-10)
    end
  end
  if Neum(4) %Neumann boundary condition
 .. .. .. .. .. .. .. .. .. .. .. .. .. 
end
.. .. .. .. .. .. .. .. .. .. .. .. .. 
end
