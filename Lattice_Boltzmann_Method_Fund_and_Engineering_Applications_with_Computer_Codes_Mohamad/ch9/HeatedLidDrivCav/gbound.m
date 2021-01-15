function [g]=gbound(nx,ny,w,g)
%Boundary condition:

twall=1.0;

%left hand all temperature

%left boundary, bounc back

g(1,:,1)=-g(1,:,3);
g(1,:,5)=-g(1,:,7);
g(1,:,8)=-g(1,:,6);

%right hand boundary

g(nx,:,3)=-g(nx,:,1);
g(nx,:,7)=-g(nx,:,5);
g(nx,:,6)=-g(nx,:,8);

%bottom boundary, bounce back

g(:,1,2)=g(:,1,4);
g(:,1,5)=g(:,1,7);


g(:,1,6)=g(:,1,8);

g(:,1,1)=g(:,2,1);
g(:,1,3)=g(:,2,3);
g(:,1,4)=g(:,2,4);

g(:,1,8)=g(:,2,8);

%Top boundary,moving lid with uo

g(:,ny,4)=(w(4)+w(2))*twall-g(:,ny,2);
g(:,ny,8)=(w(8)+w(6))*twall-g(:,ny,6);
g(:,ny,7)=(w(7)+w(5))*twall-g(:,ny,5);

end
