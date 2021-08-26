function [Ef Hf ep mu] = CavityExact(x, epl, epr, mul, mur, time);
% function [Ef Hf ep mu] = CavityExact(x, epl, epr, mul, mur, time);
% Purpose: Set up exact solution to EM cavity problem
xL = length(x); ii = sqrt(-1.0); n1 = sqrt(epl*mul); n2 = sqrt(epr*mur);
Ef = zeros(xL,1); Hf = zeros(xL,1); ep = zeros(xL,1);  mu = zeros(xL,1);

% Compute omega to match coefficients - set initial guess to obtain 
% different solutions
omega = fzero(@(x) (n1*tan(n2*x) + n2*tan(n1*x)),5);

% Set up exact solution
A1 = complex(n2*cos(omega*n2)/(n1*cos(n1*omega))); 
A2 = exp(-ii*omega*(n1+n2));
B1 = A1*exp(-ii*2*n1*omega); B2 = A2*exp(ii*2*n2*omega);
for j=1:xL
    if (x(j)<= 0)
        A = A1; B = B1; n = n1; ep(j)=epl; mu(j)=mul;
    else
        A = A2; B = B2; n = n2; ep(j)=epr; mu(j)=mur;
    end
    Eh = (A*exp(ii*n*omega*x(j)) - B*exp(-ii*n*omega*x(j)))...
            *exp(-ii*omega*time);
    Hh = n*(A*exp(ii*n*omega*x(j)) + B*exp(-ii*n*omega*x(j)))...
            *exp(-ii*omega*time);
    Ef(j) = real(Eh); Hf(j) = real(Hh);
end;
return