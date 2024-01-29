clear all;
% next 3 lines: read the input files
filenames = {'nodes.dat','elements.dat','materials.dat', ...
             'bcsforce.dat', 'bcsdisp.dat'};  
for i = 1:numel(filenames); load(filenames{i}); end;

nodes(:,2:4)=nodes(:,2:4)*0.01;
% next 4 lines: bookkeeping 
n_nodes=size(nodes,1);
n_elements=size(elements,1);
n_bcsforce=size(bcsforce,1);
n_bcsdisp=size(bcsdisp,1);

K=CompK(nodes, elements, materials);   % compute global K matrix
F=CompF(nodes, elements, bcsforce);    % compute global F vector

% next block: apply displacement boundary condition
coeff=abs(max(max(K)))*1e8;  % penalty number
for i=1:n_bcsdisp
  node_id=bcsdisp(i,1);
  direction=bcsdisp(i,2);
  K(3*node_id-3 + direction, 3*node_id-3 + direction)=coeff;
  F(3*node_id-3 + direction, 1) = bcsdisp(i,3)*coeff;
end

u=K\F  % solve the global linear system

% next block: save the displacement results in file
U=zeros(n_nodes,7);
for n=1:n_nodes
    U(n,1:7)=[n nodes(n,2:4) u(3*n-2:3*n,1)'];
end

save -ascii -double feU.dat U
format long;
Ep=CompStrain(nodes, elements, materials, u)  % compute strains
