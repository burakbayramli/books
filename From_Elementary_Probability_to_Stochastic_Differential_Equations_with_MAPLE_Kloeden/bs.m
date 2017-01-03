% BS  Black-Scholes European put price.
%

%%%%%%%%% Problem parameters %%%%%%%%%%%%%%%
S = 5; E = 10; T = 1; r = 0.06; sigma = 0.3;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

d1 = (log(S/E) + (r + 0.5*sigma^2)*T)/(sigma*sqrt(T));
d2 = d1 - sigma*sqrt(T);
N1 = 0.5*(1+erf(-d1/sqrt(2)));
N2 = 0.5*(1+erf(-d2/sqrt(2)));

value = E*exp(-r*T)*N2 - S*N1;

disp('Option value is'), disp(value)
