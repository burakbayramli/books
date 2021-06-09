%EFR11_4b
%Script for solving the Poisson Problem of EFR 11_4a
%Same BVP but half the step size:
%This program is written more generally.
N=8;, M=8;, h = 1/(N+1);
x=linspace(0,1,N+2);, y=x; 
A=4*eye(N^2);
%form sub/super diagonals
a1rep=[0 -1 -1 -1 -1 -1 -1 -1];
a1=[-1 -1 -1 -1 -1 -1 -1];
for i=1:7, a1=[a1 a1rep];, end
aN=-1*ones(1,N^2-N);
%put these diagonal entries on A
A=A+diag(a1,-1)+ diag(a1,1)+diag(aN,-N)+diag(aN,N);
% key in vectors for boundary values:
L = zeros(size(y)); R = L;
B = sin(pi*x);, T = B/exp(2);

% Now (for the most complicated part, we construct the vector C
% First we construct the column vector -h^2*Q arising from the source term:
% We do this by creating an inline function for the inhomogeneity and then 
% collecting the needed entries in the required reading order (using an 
% appropriately designed loop).
q=inline('(4-pi^2)*exp(-2*y).*sin(pi*x)', 'x', 'y');
row = 1;
for j=N+1:-1:2
    count=(row-1)*N+1;
    Q(:,count:count+N-1)=q(x(2:N+1),y(j));, row = row+1;
end
%By combining with the appropriate boundary values, we now construct C:
zer = zeros(1, N-2); %useful vector for constructing C
C= -h^2*Q + [L(9)+T(2) T(3) T(4) T(5) T(6) T(7) T(8) R(9)+T(9) ...
  L(8) zer R(8) L(7) zer R(7) L(6) zer R(6) L(5) zer R(5) L(4) zer R(4)...
  L(3) zer R(3) L(2)+B(2) B(3) B(4) B(5) B(6) B(7) B(8) R(2)+B(9)];
C=C';
%Now we are ready to solve the system, form the mesh, and plot
U=A\C;
Z=zeros(N);
Z(:)=U;
Z=Z';
Z=[T(2:N+1); Z; B(2:N+1)];
for i=1:N+2, Lrev(i)=L(N+3-i);, end
Z=[Lrev; Z'; R]';
for i=1:N+2, yrev(i)=y(N+3-i);, end
surf(x, yrev ,Z), xlabel('x-values'), ylabel('y-values'), zlabel('u-values')