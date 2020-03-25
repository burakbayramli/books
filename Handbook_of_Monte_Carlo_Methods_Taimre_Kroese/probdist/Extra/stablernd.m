function x=stablernd(alpha,beta)
% Stable(alpha,beta) generator (Algorithms 4.55+4.56)
% arrays 'alpha' and 'beta' have to be of the same size

V = pi*(rand(size(alpha)) - 0.5);
W = -log(rand(size(alpha)));
B = atan(beta.*tan(pi*alpha/2))./alpha;
x = (sin(alpha.*(V + B))./(cos(alpha.*B).*cos(V)).^(1./alpha)).*...
    (cos(V - alpha.*(V + B))./W).^((1-alpha)./alpha);
% use special formula for the case alpha=1
I=find(alpha==1);
V(I)= pi*(rand(size(alpha(I))) - 0.5);
W(I) = -log(rand(size(alpha(I))));
x(I)=2/pi*((pi/2+beta(I).*V(I)).*tan(V(I))-...
    beta(I).*log(W(I).*cos(V(I))./(1+2.*beta(I).*V(I)/pi)));