function y = f(x)

% run class_mar17 to test this one out:
% run class_mar17_nonuniform to test this one out
y = cos(3*pi*x);

% run class_mar17a to test this one out:
y = x.^4;
% leads to exact solution -x^6/30 + 1/5 x

% run class_mar17a_nonuniform to test this one out:
y = nthroot((x-1/2),3);
% leads to exact solution 
% -(9/112)*(2*x-1).^2.*nthroot(x-1/2,3)+(3/16)*2^(2/3)*x-(9/224)*2^(2/3)

% run class_mar17b_nonuniform
y = x.*(1-x);
% leads to exact solution (1/12)*x^4-(1/6)*x^3+(1/6)*x
