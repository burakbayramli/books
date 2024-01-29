clear all;
UniformMeshQuad8n4(0,0,1,1,10,10); % create uniform mesh
% next 3 lines: read the input files
filenames = {'nodes.dat','elements.dat','materials.dat', ...
             'pnodes.dat','pelements.dat'};  
for i = 1:numel(filenames); load(filenames{i}); end;
SetBCs(nodes);  % set up boundary conditions
load bcsvp.dat;
n_nodes=size(nodes,1);
n_bcsvp=size(bcsvp,1);
n_pnodes=max(pnodes(:,2));

% next two lines: compute global K & F 
K=CompK(nodes, pnodes, elements, pelements, materials); 
F=zeros(n_nodes*2+n_pnodes,1);

% next 14 lines: apply v & P boundary conditions
coeff=abs(max(K(bcsvp(1,1)*2-1,:)))*1e7;  % penalty factor
prow=2*n_nodes;
for i=1:n_bcsvp
  node_id=bcsvp(i,1);
  component=bcsvp(i,2);
  if component<=2
    K(2*node_id-2 + component, 2*node_id-2 + component)=coeff;
    F(2*node_id-2 + component, 1) = bcsvp(i,3)*coeff;
  else
    pnode_id=pnodes(node_id,2);
    K(prow + pnode_id, prow + pnode_id)=coeff;
    F(prow + pnode_id, 1) = bcsvp(i,3)*coeff;    
  end
end

u=K\F;  % solve the global linear system

% next 15 lines: save the displacement results in file
U=zeros(n_nodes,6);
k=1;
for i=1:n_nodes
    U(i,1:5)=[nodes(i,1:3) u(2*i-1:2*i,1)'];
    pnode_id=pnodes(i,2);
    U(i,6)=u(prow + pnode_id,1);
    if nodes(i,2)==0.5
     vxline(k,1:2)=U(i,3:4);
     k=k+1;
   end
end
vxline
save -ascii -double sol.dat U
save -ascii -double vx.dat vxline
disp('Velocity and pressure results stored in sol.dat');
PlotQuiver;  % plot the velocity results