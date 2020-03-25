function  out=fixed_point(t,N,I,a2)
% this implements the function t-zeta*gamma^[l](t)
l=7;
f=1/2*pi^(2*l)*sum(I.^l.*a2.*exp(-I*pi^2*t));
for s=l-1:-1:2
   K0=prod([1:2:2*s-1])/sqrt(pi/2);  K1=(1+(1/2)^(s+1/2))/3;
   time=(K1*K0/N/f)^(2/(3+2*s));
   f=1/2*pi^(2*s)*sum(I.^s.*a2.*exp(-I*pi^2*time));
end
out=t-(2*N*sqrt(pi)*f)^(-2/5);