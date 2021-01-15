function [g]=gbound(nx,ny,w,g)
%Boundary condition:

twall=1.0;

%left hand all temperature

%left boundary, T=1.0

g(1,:,1)=twall*(w(1)+w(3))-g(1,:,3);
g(1,:,5)=twall*(w(5)+w(7))-g(1,:,7);
g(1,:,8)=twall*(w(8)+w(6))-g(1,:,6);

%right hand boundary T=0.0

g(nx,:,3)=-g(nx,:,1);
g(nx,:,7)=-g(nx,:,5);
g(nx,:,6)=-g(nx,:,8);

%bottom boundary,Insulated

g(:,1,2)=g(:,2,4);
g(:,1,5)=g(:,2,7);
g(:,1,6)=g(:,2,8);

g(:,1,4)=g(:,2,4);
g(:,1,7)=g(:,2,7);
g(:,1,8)=g(:,2,8);


g(:,1,9)=g(:,2,9);
%Top boundary,Insulated

g(:,ny,4)=g(:,ny-1,2);
g(:,ny,8)=g(:,ny-1,6);
g(:,ny,7)=g(:,ny-1,5);

g(:,ny,1)=g(:,ny-1,1);
g(:,ny,2)=g(:,ny-1,2);
g(:,ny,3)=g(:,ny-1,3);
g(:,ny,5)=g(:,ny-1,5);
g(:,ny,9)=g(:,ny-1,9);


end
