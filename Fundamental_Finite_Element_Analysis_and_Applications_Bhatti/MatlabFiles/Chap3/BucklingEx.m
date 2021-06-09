% Solution of Euler buckling using quadratic elements
L = 12*10.; EI = 10^6;
nodes = [0:L/8:L];n=length(nodes);
Ke=zeros(n); Kp=zeros(n);
% Generate equations for each element and assemble them.
for i=1:4
    lm=[2*(i-1)+1,2*(i-1)+2,2*(i-1)+3];
    [ke, kp] = BucklingQuadElement(EI, nodes(lm));
    Ke(lm, lm) = Ke(lm, lm) + ke;
    Kp(lm, lm) = Kp(lm, lm) + kp;
end
% Adjust for EBC
debc=[1,n];
df = setdiff(1:n, debc);
Kef = Ke(df, df)
Kep = Kp(df, df)
[v,e] = eig(Kef, Kep);
fprintf('Buckling load = %10.6g',e(1,1))
d = zeros(n,1);
d(df) = v(:,1)
plot(nodes,d),title('First buckling mode'), xlabel('x'),ylabel('v')