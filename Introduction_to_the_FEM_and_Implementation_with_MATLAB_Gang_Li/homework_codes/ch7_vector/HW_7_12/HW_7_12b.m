clear all;

% next block: create the model - nodes, elements, loads, BCs, material properties 
nodes=[1 0 5; 2 0 0; 3 10 5; 4 10 0];
elements=[1 1 4 3; 2 1 2 4];
bcsforce=[4 0 -500];
bcsdisp=[1 1 0; 2 1 0; 2 2 0];
bcstraction=[1 3 3 1 0.0 -200 0.0 -200];
materials=[30e6 0.3 1.0]';

% next block: bookkeeping
n_nodes=size(nodes,1);
n_elements=size(elements,1);
n_bcstraction=size(bcstraction,1);
n_bcsforce=size(bcsforce,1);
n_bcsdisp=size(bcsdisp,1);

K=CompK(nodes, elements, materials);   % compute global K matrix and
F=CompF(nodes, elements, materials, bcstraction, bcsforce); % F vector

% next block: apply displacement boundary condition
coeff=abs(max(K(bcsdisp(1,1)*2-1,:)))*1e8;  %penalty number
for i=1:n_bcsdisp
  node_id=bcsdisp(i,1);
  direction=bcsdisp(i,2);
  K(2*node_id-2 + direction, 2*node_id-2 + direction)=coeff;
  F(2*node_id-2 + direction, 1) = bcsdisp(i,3)*coeff;
end

u=K\F;  % solve the global linear system

Sxy=CompStress(nodes, elements, materials, u);

% next 6 lines: save the displacement results in file
U=zeros(n_nodes,5);
for n=1:n_nodes
    U(n,:)=[n  nodes(n,2:3)  u(2*n-1:2*n,1)'];
end
save -ascii -double feU.dat U;
save -ascii -double Sxy.dat Sxy;
save -ascii -double elements.dat elements;

plotdeformedmesh