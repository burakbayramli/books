function [f]=boundary(nx,ny,f)
%Boundary condition:

%left boundary, bounc back

f(1,:,1)=f(nx,:,1);
f(1,:,5)=f(nx,:,5);
f(1,:,8)=f(nx,:,8);

%right hand boundary

f(nx,:,3)=f(1,:,3);
f(nx,:,7)=f(1,:,7);
f(nx,:,6)=f(1,:,6);

%bottom boundary, bounce back

f(:,1,2)=f(:,ny,2);
f(:,1,5)=f(:,ny,5);
f(:,1,6)=f(:,ny,6);

%Top boundary

for i=2:nx-1
f(i,ny,4)=f(i,1,4);
f(i,ny,8)=f(i,1,8);
f(i,ny,7)=f(i,1,7);
end

end
