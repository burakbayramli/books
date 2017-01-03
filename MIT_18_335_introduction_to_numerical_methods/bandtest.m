% Initialize banded matrix and RHS for Poisson equation on unit square
p=20; m=p^2;
Ab=zeros(m,p+1); Ab(:,1)=4; Ab(:,2)=-1; Ab(p:p:m,2)=0; Ab(:,p+1)=-1;
f=ones(m,1)/(p+1)^2;

% Solve using banded solver
Rb=bandchol(Ab);
y=bandforwardsub(Rb,f);
x=bandbacksub(Rb,y);

% Solve using MATLAB
A=spdiags(Ab(:,[1,2,p+1]),-[0,1,p],m,m); A=A+tril(A,-1)';
x0=A\f;

% Plot and compare solutions
surf(reshape(x,p,p)); view(2); axis equal;
shading interp; colorbar; set(gcf,'rend','z');
error=norm(x-x0)
