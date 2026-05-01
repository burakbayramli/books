function [f,D] = fnewt(x,a)
%  Function used by the N-variable Newton's method
%  Inputs
%    x     State vector [x y z]
%    a     Parameters [r sigma b]
%  Outputs
%    f     Lorenz model r.h.s. [dx/dt dy/dt dz/dt]
%    D     Jacobian matrix, D(i,j) = df(j)/dx(i)

% Evaluate f(i)
f(1) = a(2)*(x(2)-x(1));
f(2) = a(1)*x(1)-x(2)-x(1)*x(3);
f(3) = x(1)*x(2)-a(3)*x(3);

% Evaluate D(i,j)
D(1,1) = -a(2);        % df(1)/dx(1)
D(1,2) = a(1)-x(3);    % df(2)/dx(1)
D(1,3) = x(2);         % df(3)/dx(1)
D(2,1) = a(2);         % df(1)/dx(2)
D(2,2) = -1;           % df(2)/dx(2)
D(2,3) = x(1);         % df(3)/dx(2)
D(3,1) = 0;            % df(1)/dx(3)
D(3,2) = -x(1);        % df(2)/dx(3)
D(3,3) = -a(3);        % df(3)/dx(3)
return;
