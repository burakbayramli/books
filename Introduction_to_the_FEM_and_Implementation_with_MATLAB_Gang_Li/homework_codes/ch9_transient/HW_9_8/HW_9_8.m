clear all;

[nodes elements]=FShapeUniformMeshQuad4(0,0,.16,.40,32,80, ...
                                        .04,0,.17,.16, .04,.20,.17,.32);
n_nodes=size(nodes,1);
n_elements=size(elements,1);

% next block: apply displacement BC on the bottom edge
for i=1:n_nodes
  if nodes(i,3)==0
    bcsdisp(2*i-1:2*i,1:3)=[i 1 0; i 2 0];
  end
end
n_bcsdisp=size(bcsdisp,1);

% next block: set up global material properties and constants
materials=[1.2e10 .25 2330 0 0]';
E=materials(1,1);
nu=materials(2,1);
rho=materials(3,1);
dimension=2;

K=CompK(nodes, elements, materials);   % compute global K matrix
M=CompM(nodes, elements, rho);         % compute global M matrix

U=ones(n_nodes*dimension,1);
drows=zeros(size(bcsdisp,1),1);
for j=1:size(bcsdisp,1);    
  nid=bcsdisp(j,1);
  k=bcsdisp(j,2);
  row=dimension*(nid-1)+k;
  drows(j)=row;
  U(row,1)=0;
end

K(drows,:)=[];
K(:,drows)=[];
M(drows,:)=[];
M(:,drows)=[];

n_modes=5;       % input number of modes to be computed
current_mode=1;  % input the mode to be plotted

[V D]=eigs(K,M,n_modes,'smallestabs');
D=sqrt(D)

row=1;
for j=1:size(V,1)
  while U(row)==0
    row=row+1;
  end
  U(row)=V(j,current_mode);
  row=row+1;
end

feU=zeros(n_nodes,5);
for i=1:n_nodes
  feu(i,1)=i;
  feU(i,2)=nodes(i,2);
  feU(i,3)=nodes(i,3);
  feU(i,4)=U(dimension*i-1);
  feU(i,5)=U(dimension*i);
end

save -ascii -double feU.dat feU

plotdeformedmesh;