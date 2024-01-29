clear all;
% next 3 lines: read the input files
filenames = {'nodes.dat','elements.dat','materials.dat', ...
             'bcstraction.dat','bcsforce.dat', 'bcsdisp.dat'};  
for i = 1:numel(filenames); load(filenames{i}); end;

% next 5 lines: bookkeeping
n_nodes=size(nodes,1);
n_elements=size(elements,1);
n_bcstraction=size(bcstraction,1);
n_bcsforce=size(bcsforce,1);
n_bcsdisp=size(bcsdisp,1);

K=CompK(nodes, elements, materials);   % compute global K matrix and
F=CompF(nodes, elements, materials, bcstraction, bcsforce); % F vector

% next 7 lines: apply displacement boundary condition
coeff=abs(max(K(bcsdisp(1,1)*2-1,:)))*1e8;  %penalty number
for i=1:n_bcsdisp
  node_id=bcsdisp(i,1);
  direction=bcsdisp(i,2);
  K(2*node_id-2 + direction, 2*node_id-2 + direction)=coeff;
  F(2*node_id-2 + direction, 1) = bcsdisp(i,3)*coeff;
end

u=K\F;  % solve the global linear system

% next 6 lines: save the displacement results in file
U=zeros(n_nodes,5);
for n=1:n_nodes
    U(n,:)=[n  nodes(n,2:3)  u(2*n-1:2*n,1)'];
end
save -ascii -double feU.dat U;