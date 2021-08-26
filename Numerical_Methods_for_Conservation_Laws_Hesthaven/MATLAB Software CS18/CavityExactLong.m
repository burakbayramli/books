function [Ef Hf ep mu] = CavityExactLong(x, epl, epr, mul, mur, time);
% function [Ef Hf ep mu] = CavityExact(x, epl, epr, mul, mur, time);
% Purpose: Set up exact solution to EM cavity problem [-2,2]
% NOTE: n1=1 is assumed;

xL = length(x); ii = sqrt(-1.0);
n1 = 1.0; n2 = sqrt(epr*mur);
Ef = zeros(xL,1); Hf = zeros(xL,1); ep = zeros(xL,1);  mu = zeros(xL,1);

% Compute omega to match coefficients - set initial guess to obtain different solutions
omega = fzero(@(x) (tan(n2*x) + n2*tan(x)),5);

% Set up exact solution
A1 = complex(n2*cos(omega*n2)/(cos(omega))); A2 = exp(-ii*omega*2); A3 = exp(-4*ii*omega)*A1;
B1 = A3; B2= A2; B3 = A1;
for j=1:xL
    if (x(j)<=-1)
        A = A1; B = B1; n = 1.0; ep(j)=epl; mu(j)=mul;
    else
        if (x(j)>=1)
           A = A3; B = B3; n = 1.0; ep(j)=epl; mu(j)=mul;
        else
           A = A2; B = B2; n = n2; ep(j)=epr; mu(j)=mur;
        end
    end
    Eh = (A*exp(ii*n*omega*x(j)) - B*exp(-ii*n*omega*x(j)))*exp(ii*omega*time);
    Hh = n*(A*exp(ii*n*omega*x(j)) + B*exp(-ii*n*omega*x(j)))*exp(ii*omega*time);
    Ef(j) = real(Eh); Hf(j) = real(Hh);
end;
return