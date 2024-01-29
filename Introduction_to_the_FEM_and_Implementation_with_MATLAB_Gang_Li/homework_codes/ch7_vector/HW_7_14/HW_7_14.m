clear all;
HW_6_2b;    % create mesh

% next block: load files
load nodes.dat;         % nodes
load elements.dat;      % elements
load bcstraction.dat;   % surface traction
load materials.dat;     % material properties
n_nodes=size(nodes,1); 
n_elements=size(elements,1);
n_bcstraction=size(bcstraction,1);

% next block: set displacement BC and force
k=1; 
p=1;
for i=1:n_nodes
  if nodes(i,2)==0 &&  nodes(i,3)>0
    bcsdisp(k,1:3)=[i 1 0];
    k=k+1;
    bcsdisp(k,1:3)=[i 2 0];
    k=k+1;
  end
  if nodes(i,2)==0 &&  nodes(i,3)==-0.2
    bcsforce(p,1:3)=[i 0 -1000];
    p=p+1;
  end
end

% next block: bookkeeping
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
Sxy=CompStress(nodes, elements, materials, u); % compute the stresses

% next block: save the displacement results in file
U=zeros(n_nodes,5);
for n=1:n_nodes
    U(n,:)=[n  nodes(n,2:3)  u(2*n-1:2*n,1)'];
end
[x y maxU]=CompMaxDisp(U)   % maximum displacement

save -ascii -double feU.dat U;
save -ascii -double Sxy.dat Sxy;

plotdeformedmesh;
PlotStress;