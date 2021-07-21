function y = dual(t, psi)
%
% Computes the right hand side of the dual problem
% -phi ' = J^t phi
%
% where J = f'(U) is the jacobian.
%
% Input: t - the time at which the rhs is evaluated
%        psi - the dual function
%
% Output: y - the rhs, J^t (T-t) * psi

global Up fp Tp tp N 
% Up - the solution U
% fp - the function specifying the problem, e.g. 'akzonobelf' 
% Tp - the final time T
% tp - all the timevalues
% N - size of the system, u_1, ... u_N

J = zeros(N,N);
I = eye(N);
h = 1e-8;

[dummy,index] = min(abs(tp-ones(size(tp))*(Tp-t)));
u = Up(:,index);


% Compute right-hand side J*psi = f'(U(T-t))*psi for the dual problem
for j=1:N
 
 J(:,j) = ( feval(fp, Tp-t, u+h*I(:,j))' - ...
            feval(fp, Tp-t, u-h*I(:,j))' ) / (2*h);
end


y = J'*psi;
y=y';
