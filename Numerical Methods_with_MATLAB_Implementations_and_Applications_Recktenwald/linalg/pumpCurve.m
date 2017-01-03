function c = pumpCurve(h,q)
% pumpCurve  Coefficients of quadratic pump curve given head and flow rate
%
% Synopsis:   c = pumpCurve(h,q)
%
% Input:      h = 3 element row or column vector of head values
%             q = 3 element row or colunn vector of flow rate values
%
% Output:     c = column vector of coefficients such that
%                  h = c(1)*q.^2 + c(2)*q + c(3)

A = [q(1)^2  q(1)  1;         %  Compute A
     q(2)^2  q(2)  1;
     q(3)^2  q(3)  1];
b = [h(1); h(2); h(3)];       %  Assign values to b
c = A\b;                      %  Solve the system
