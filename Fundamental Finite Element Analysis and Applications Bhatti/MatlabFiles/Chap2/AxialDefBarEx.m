% Tapered bar example
e = 70*10^3; P = 20*1000;
nodes = [0:150:600];
A = [2175, 1725, 1275, 825];
lmm = [1,2; 2,3; 3,4; 4,5];  
K=zeros(5);
% Generate stiffness matrix for each element and assemble it.
for i=1:4
    lm=lmm(i,:);
    k=AxialDefElement(e, A(i), nodes(lm));
    K(lm, lm) = K(lm, lm) + k;
end
K
% Define the load vector
R = zeros(5,1); R(3)=P

% Nodal solution and reactions
[d, reactions] = NodalSoln(K, R, [1,5], zeros(2,1))
results=[];
for i=1:4
    results = [results; AxialDefResults(e, A(i), ...
            nodes(lmm(i,:)), d(lmm(i,:)))];
end
format short g
results
plot(nodes,d),title('Axial displacement'), xlabel('x'),ylabel('u')