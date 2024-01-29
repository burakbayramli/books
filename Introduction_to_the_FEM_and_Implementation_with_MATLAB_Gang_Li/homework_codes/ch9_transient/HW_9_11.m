clear all;

% next block: generate nodes, elements and set nodal displacements
nodes=[1 1.0 0 0; 2 1.0 0 1.0; 3 1.0 1.0 0; 4 0 1.0 0];
elements=[1 1 3; 2 2 3; 3 3 4];
bcsdisp=[1 1 0;  1 2 0; 1 3 0; 2 1 0; 2 2 0; 2 3 0; ...
         4 1 0; 4 2 0; 4 3 0];

% next block: set up constants and empty matrices 
nNodes=size(nodes,1);
nElements=size(elements,1);
E=200e9;
A=1e-4;
K=zeros(3*nNodes,3*nNodes);
M=zeros(3*nNodes,3*nNodes);
length=zeros(nElements,1);
direction_cos=zeros(nElements,3);

% for-loop: compute the global stiffness matrix
for e=1:nElements
  nids=elements(e,2:3);
  dv=nodes(nids(2),2:4)-nodes(nids(1),2:4);
  length(e)=norm(dv);
  direction_cos(e,:)=dv/length(e);
  cx=direction_cos(e,1);
  cy=direction_cos(e,2);
  cz=direction_cos(e,3);
  ke=[cx^2   cx*cy  cx*cz -cx^2  -cx*cy -cx*cz  
     cx*cy  cy^2   cy*cz -cx*cy -cy^2  -cy*cz
     cx*cz  cy*cz  cz^2  -cx*cz -cy*cz -cz^2 
    -cx^2  -cx*cy -cx*cz  cx^2   cx*cy  cx*cz  
    -cx*cy -cy^2  -cy*cz  cx*cy  cy^2   cy*cz
    -cx*cz -cy*cz -cz^2   cx*cz  cy*cz  cz^2];
  ke=ke*A*E/length(e);
  me=[2*cx^2   2*cx*cy  2*cx*cz  cx^2  cx*cy cx*cz  
      2*cx*cy  2*cy^2   2*cy*cz  cx*cy cy^2  cy*cz
      2*cx*cz  2*cy*cz  2*cz^2   cx*cz cy*cz cz^2 
      cx^2  cx*cy cx*cz  2*cx^2   2*cx*cy  2*cx*cz  
      cx*cy cy^2  cy*cz  2*cx*cy  2*cy^2   2*cy*cz
      cx*cz cy*cz cz^2   2*cx*cz  2*cy*cz  2*cz^2];
  me=me*A*length(e)/6;
  
  % for-loop: assemble the element matrices into the global K
  for j=1:2         % loop over the row blocks
    for k=1:2       % loop over the column blocks
      K(3*nids(j)-2:3*nids(j), 3*nids(k)-2:3*nids(k)) ...
        =K(3*nids(j)-2:3*nids(j), 3*nids(k)-2:3*nids(k)) ...
         + ke(3*j-2:3*j,3*k-2:3*k);
      M(3*nids(j)-2:3*nids(j), 3*nids(k)-2:3*nids(k)) ...
        =M(3*nids(j)-2:3*nids(j), 3*nids(k)-2:3*nids(k)) ...
         + me(3*j-2:3*j,3*k-2:3*k);
    end            
  end
end

% next block: record rows for zero displacements
U=ones(nNodes*3,1);
drows=zeros(size(bcsdisp,1),1);
for j=1:size(bcsdisp,1);    
  nid=bcsdisp(j,1);
  k=bcsdisp(j,2);
  row=3*(nid-1)+k;
  drows(j)=row;
  U(row,1)=0;
end

K(drows,:)=[];
K(:,drows)=[];
M(drows,:)=[];
M(:,drows)=[];

n_modes=3;       % input number of modes to be computed
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

% next block: plotting
figure(1);
U=U*1e-3;  % scale the displacement for plotting
clf;
hold on;
% for-loop: plot the deformed and undeformed structure
for e=1:nElements
  nd1=elements(e,2);
  nd2=elements(e,3);
  x1=nodes(nd1,2); y1=nodes(nd1,4); z1=nodes(nd1,3);
  x2=nodes(nd2,2); y2=nodes(nd2,4); z2=nodes(nd2,3);
  plot3([x1 x2], [y1 y2], [z1 z2],'k--');       
  plot3([x1+U(3*nd1-2) x2+U(3*nd2-2)], [y1+U(3*nd1) y2+U(3*nd2)],...
        [z1+U(3*nd1-1) z2+U(3*nd2-1)], 'k-','LineWidth',3);
end
axis([0 1.2 -0.3 1 -0 1.2]);
set(gca,'Ydir','reverse');    % reverse the y-axis for plotting
xlabel('X'); ylabel('Z'); zlabel('Y');
view(55,25);
hold off;