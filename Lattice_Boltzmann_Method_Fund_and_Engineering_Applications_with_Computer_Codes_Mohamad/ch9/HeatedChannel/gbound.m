function [g]=gbound(nx,ny,w,g)
%Boundary condition:

twall=1.0;

%left hand all temperature

%left boundary, inlet temp. zero

g(1,:,1)=-g(1,:,3);
g(1,:,5)=-g(1,:,7);
g(1,:,8)=-g(1,:,6);

%right hand boundary, adiabatic

g(nx,:,3)=g(nx,:,1);
g(nx,:,7)=g(nx,:,5);
g(nx,:,6)=g(nx,:,8);

%bottom boundary, temp. is twall

g(:,1,2)=(w(2)+w(4))*twall-g(:,1,4);
g(:,1,5)=(w(5)+w(7))*twall-g(:,1,7);
g(:,1,6)=(w(6)+w(8))*twall-g(:,1,8);

%Top boundary, temp. is twall

g(:,ny,4)=(w(4)+w(2))*twall-g(:,ny,2);
g(:,ny,8)=(w(8)+w(6))*twall-g(:,ny,6);
g(:,ny,7)=(w(7)+w(5))*twall-g(:,ny,5);

end
