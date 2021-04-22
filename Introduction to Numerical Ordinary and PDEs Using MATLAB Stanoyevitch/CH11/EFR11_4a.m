%EFR11_4a
%Script for solving the Poisson Problem of EFR 11_4a
N=4; M=4; h = 1/(N+1);
x=linspace(0,1,N+2); y=x; 
A=4*eye(N^2);
%form sub/super diagonals
a1rep=[0 -1 -1 -1];
a1=[-1 -1 -1];
for i=1:3, a1=[a1 a1rep]; end
a4=-1*ones(1,12);
%put these diagonal entries on A
A=A+diag(a1,-1)+ diag(a1,1)+diag(a4,-4)+diag(a4,4);
% key in vectors for boundary values:
L = zeros(size(y)); R = L;
B = sin(pi*x); T = B/exp(2);

% Now (for the most complicated part, we construct the vector C
% First we construct the column vector -h^2*Q arising from the source term:
% We do this by creating an inline function for the inhomogeneity and then 
% collecting the needed entries in the required reading order (using an 
% appropriately designed loop).
q=inline('3*exp(-2*y).*sin(pi*x)', 'x', 'y');
row = 1;
for j=5:-1:2
    count=(row-1)*4+1;
    Q(:,count:count+3)=q(x(2:5),y(j)); row = row+1;
end
%By combining with the appropriate boundary values, we now construct C:
C= -h^2*Q + [L(5)+T(2) T(3) T(4) R(5)+T(5) L(4) 0 0 R(4) L(3) 0 0 R(3) L(2)+B(2)... 
  B(3) B(4) R(2)+B(5)];  C=C';
%Now we are ready to solve the system, form the mesh, and plot
U=A\C;
Z=zeros(4);
Z(:)=U;
Z=Z';
Z=[T(2:5); Z; B(2:5)];
for i=1:6, Lrev(i)=L(7-i); end
Z=[Lrev; Z'; R]';
for i=1:6, yrev(i)=y(7-i); end
surf(x, yrev ,Z), xlabel('x-values'), ylabel('y-values'), zlabel('u-values')

%Errors
[X, Y] = meshgrid(x,yrev);
Zexact=exp(-2*Y).*sin(pi*X);
Max_Error = max(max(abs(Z(2:N+1,2:N+1)-Zexact(2:N+1,2:N+1))))
Max_Relative_Error = max(max(abs(Z(2:N+1,2:N+1)-Zexact(2:N+1,2:N+1))./abs(Zexact(2:N+1,2:N+1))))






