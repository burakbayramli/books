clear all;
% next block: set up nodes and elements
[nodes,elements]=UniformMeshTri3(0,0,10,5,160,80);
n_nodes=size(nodes,1);
n_elements=size(elements,1);
materials=[30e6 0.3 1.0]';

% next block: set displacement BC and force
k=1; 
for i=1:n_nodes
  if nodes(i,2)==10 && nodes(i,3)==0
    bcsforce=[i 0 -500];
  end
  if nodes(i,2)==0 
    bcsdisp(k,1:3)=[i 1 0];
    k=k+1;
    if nodes(i,3)==0 
      bcsdisp(k,1:3)=[i 2 0];
      k=k+1;
    end
  end
end

% next block: set up the edges that have distributed load
k=1;
for e=1:n_elements
  for j=1:3
    if nodes(elements(e,j+1),3)== 5 && nodes(elements(e,rem(j,3)+2),3)== 5
      bcstraction(k,1:8)=[e j elements(e,j+1) elements(e,rem(j,3)+2) 0 -200 0 -200];
      k=k+1;
    end
  end
end

% next block: bookkeeping
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
Sxy=CompStress(nodes, elements, materials, u);  % compute stresses

% next block: save the displacement results in file
U=zeros(n_nodes,5);
for n=1:n_nodes
    U(n,:)=[n  nodes(n,2:3)  u(2*n-1:2*n,1)'];
end

U(n_nodes,5)*100

save -ascii -double feU.dat U;
save -ascii -double Sxy.dat Sxy;
save -ascii -double elements.dat elements;

%plotdeformedmesh