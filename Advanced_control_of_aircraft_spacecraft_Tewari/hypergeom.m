function F=hypergeom(z)
% Hypergeometric function evaluation
% by continued fraction
% (c) 2010 Ashish Tewari
d=1;u=1;F=1;
eps=1e-9;
i=1;
while abs(u)>eps
if rem(i,2)==0
gamma=i*(i-3)/((2*i+1)*(2*i+3));
else
gamma=(i+2)*(i+5)/((2*i+1)*(2*i+3));
end
d=1/(1-gamma*z*d);
u=u*(d-1);
F=F+u;
i=i+1;
end
function G=hypergeomd(z)
% Hypergeometric function, G(z)
% evaluated by continued fraction
d=1;u=1;G=1;
eps=1e-9;
i=1;
while abs(u)>eps
n=i+1;
if rem(n,2)==0
gamma=n*(n-3)/((2*n+1)*(2*n+3));
else
gamma=(n+2)*(n+5)/((2*n+1)*(2*n+3));
end
d=1/(1-gamma*z*d);
u=u*(d-1);
G=G+u;
i=i+1;
end
