function p = lorentz(x,y)
% LORENTZ  Calculates the Lorentz inner product
%          of the two 4 by 1 vectors x and y

% Written by Kai Borre
% April 22, 1995

%  M = diag([1 1 1 -1]);
%  p = x'*M*y;

   p = x(1)*y(1) + x(2)*y(2) + x(3)*y(3) - x(4)*y(4);

%%%%%%%%%% end lorentz.m %%%%%%%%%%%%%%%
