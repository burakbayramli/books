clear all;
% next 3 lines: read the input files
filenames = {'nodes.dat','elements.dat','materials.dat', ...
             'options.dat','forces.dat', 'bcsdisp.dat'};  
for i = 1:numel(filenames); load(filenames{i}); end;

% next 8 lines: set up constants and empty matrices 
nNodes=size(nodes,1);
nElements=size(elements,1);
E=materials(1,1);
A=options(2,1);
K=zeros(3*nNodes,3*nNodes);
F=zeros(3*nNodes,1);
length=zeros(nElements,1);
direction_cos=zeros(nElements,3);

% for-loopL compute the global stiffness matrix
for e=1:nElements
  nids=elements(e,2:3);
  dv=nodes(nids(2),2:4)-nodes(nids(1),2:4)
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
  % for-loop: assemble the element matrices into the global K
  for j=1:2         % loop over the row blocks
    for k=1:2       % loop over the column blocks
      K(3*nids(j)-2:3*nids(j), 3*nids(k)-2:3*nids(k)) ...
        =K(3*nids(j)-2:3*nids(j), 3*nids(k)-2:3*nids(k)) ...
         + ke(3*j-2:3*j,3*k-2:3*k);
    end            
  end
end

% for-loop: set up the global force vector
for i=1:size(forces,1)
  node = forces(i,1);
  F(3*node-2:3*node)= forces(i,2:4);
end

% next 8 lines: apply the displacement BC using the penalty method
Kold=K
penalty=abs(max(max(K)))*1e7;
for i=1:size(bcsdisp,1)
  node = bcsdisp(i,1);
  direction = bcsdisp(i,2);
  K(3*node + direction - 3, 3*node + direction - 3)= penalty;
  F(3*node + direction - 3)= penalty*bcsdisp(i,3);
end

d=K\F  % solve global linear system

% next 32 lines: postprocessing
reaction_force=Kold*d;  % compute support reactions
% next 10 lines: compute element stresses
stress=zeros(nElements,1);
for e=1:nElements
  nids=elements(e,2:3);
  cx=direction_cos(e,1);
  cy=direction_cos(e,2);
  cz=direction_cos(e,3);
  d_vector= [d(nids(1)*3 -2) d(nids(1)*3-1) d(nids(1)*3) ...
              d(nids(2)*3 -2)  d(nids(2)*3-1) d(nids(2)*3)]';
  stress(e)=E/length(e)*[-cx -cy -cz cx cy cz]*d_vector;
end
% next 19 lines: plotting
figure(1);
d=d*10000;  % scale the displacement for plotting
clf;
hold on;
% for-loop: plot the deformed and undeformed truss system
for e=1:nElements
  nd1=elements(e,2);
  nd2=elements(e,3);
  x1=nodes(nd1,2); y1=nodes(nd1,4); z1=nodes(nd1,3);
  x2=nodes(nd2,2); y2=nodes(nd2,4); z2=nodes(nd2,3);
  plot3([x1 x2], [y1 y2], [z1 z2],'k--');       
  plot3([x1+d(3*nd1-2) x2+d(3*nd2-2)], [y1+d(3*nd1) y2+d(3*nd2)],...
        [z1+d(3*nd1-1) z2+d(3*nd2-1)], 'k-','LineWidth',3);
end
axis([0 1.2 -0.3 1 -0 1.2]);
set(gca,'Ydir','reverse');    % reverse the y-axis for plotting
xlabel('X'); ylabel('Z'); zlabel('Y');
view(55,25);
hold off;