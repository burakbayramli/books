function [f]=boundary(nx,ny,f,uo)
%Boundary condition:

%left boundary, bounc back

f(1,:,1)=f(1,:,3);
f(1,:,5)=f(1,:,7);
f(1,:,8)=f(1,:,6);

%right hand boundary

f(nx,:,3)=f(nx,:,1);
f(nx,:,7)=f(nx,:,5);
f(nx,:,6)=f(nx,:,8);

%bottom boundary, bounce back

f(:,1,2)=f(:,1,4);
f(:,1,5)=f(:,1,7);
f(:,1,6)=f(:,1,8);

%Top boundary,moving lid with uo

for i=2:nx-1
rhon=f(i,ny,9)+f(i,ny,1)+f(i,ny,3)+2.*(f(i,ny,2)+f(i,ny,6)+f(i,ny,5));
f(i,ny,4)=f(i,ny,2);
f(i,ny,8)=f(i,ny,6)+rhon*uo/6.0;
f(i,ny,7)=f(i,ny,5)-rhon*uo/6.0;
end

end
