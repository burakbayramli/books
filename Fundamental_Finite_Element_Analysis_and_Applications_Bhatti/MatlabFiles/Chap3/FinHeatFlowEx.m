% Heat flow through a fin
kx=237;w=3;t=0.3/100;A=w*t;h=30;L=20/100.;P=2*(w+t);Tinf=25;
alpha=-h*A/(kx*A); beta=h*A*Tinf/(kx*A);
k=kx*A; p=-h*P; q=h*P*Tinf;
nodes = [0:L/8:L];n=length(nodes);
K=zeros(n); R = zeros(n,1);
% Generate equations for each element and assemble them.
for i=1:4
    lm=[2*(i-1)+1,2*(i-1)+2,2*(i-1)+3];
    [ke, re] = BVP1DQuadElement(k,p,q, nodes(lm));
    K(lm, lm) = K(lm, lm) + ke;
    R(lm) = R(lm) + re;
end
% Adjust for NBC
K(n,n)=K(n,n)-alpha*k
R(n)=R(n)+beta*k
% Nodal solution and reactions
d = NodalSoln(K, R, [1], [100])
plot(nodes,d),title('Temperature distribution'), xlabel('x'),ylabel('T')