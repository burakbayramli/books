%nm5p11
a=-1; b=1; % the lower-/upper-bounds of the integration interval
N=1000 % the number of segments for the Simpson method
tol=1e-6 % the error tolerance
M=20 % the number of grid points for Gauss-Legendre integration 
IT=pi; h=1e-3 % true integral and step-size for numerical derivative
flength=inline('sqrt(1+dfp511(x,h).^2)','x','h');%integrand P5.11-1)
Is= smpsns(flength,a,b,N,h);
[Ias,points,err]=adapt_smpsn(flength,a,b,tol,h);
Iq=quad(flength,a,b,tol,[],h);
Iql=quadl(flength,a,b,tol,[],h);
IGL=Gauss_Legendre(flength,a,b,M,h);
