function [errmat] = betarcalc(x,m)
% Purpose: Compute matrix to allow evaluation of smoothness indicator in
%          WENO based on stencil [x] of length m+1 .
%          Returns sum of operators for l=1.m-1

% Evaluate Lagrange polynomials
[cw] = lagrangeweights(x);

% Compute error matrix for l=1..m-1
errmat = zeros(m,m);
for l=2:m
   % Evaluate coefficients for derivative of Lagrange polynomial
   dw = zeros(m,m-l+1);
   for k=0:(m-l)
     for q=0:m-1
       dw(q+1,k+1) = sum(cw((q+2):m+1,k+l+1));
     end
   end  
   
   % Evaluate entries in matrix for order 'l'
   Qmat = zeros(m,m);
   for p=0:m-1
     for q=0:m-1
        D = dw(q+1,:)'*dw(p+1,:);
        Qmat(p+1,q+1) = Qcalc(D,m,l);
     end
   end 
   errmat = errmat + Qmat;
end
return
