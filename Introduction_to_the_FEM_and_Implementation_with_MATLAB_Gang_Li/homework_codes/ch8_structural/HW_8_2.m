clear;

% next block: preprocessing
nodes=[1 0 0; 2 3 4; 3 6 0; 4 6 4];
elements=[1 1 3; 2 3 2; 3 2 1; 4 2 4; 5 3 4];
bcsdisp=[4 1 0; 4 2 0; 1 2 0];
forces=[3 1 353.5534; 3 2 353.5534; 2 1 400; 2 2 -300];
nNodes=size(nodes,1);
nElements=size(elements,1);

E=2e9;
A=1e-5;

% next block: initialization
K=zeros(2*nNodes,2*nNodes);
F=zeros(2*nNodes,1);
length=zeros(nElements,1);
phi=zeros(nElements,1);

% for-loop: compute the global stiffness matrix
for e=1:nElements
  node1=elements(e,2);
  node2=elements(e,3);
  dy=(nodes(node2,3)-nodes(node1,3));
  dx=(nodes(node2,2)-nodes(node1,2));
  phi(e)=atan(dy/dx);
  length(e)=sqrt(dy^2 + dx^2);
  c=cos(phi(e));
  s=sin(phi(e));
  
  k=[c^2   c*s   -c^2   -c*s  
     c*s  s^2   -c*s   -s^2   
     -c^2   -c*s   c^2   c*s 
     -c*s   -s^2   c*s  s^2] * A*E/length(e);
  
  K(2*node1-1:2*node1, 2*node1-1:2*node1) ...
            =K(2*node1-1:2*node1, 2*node1-1:2*node1) +k(1:2,1:2);
  K(2*node1-1:2*node1, 2*node2-1:2*node2) ...
            =K(2*node1-1:2*node1, 2*node2-1:2*node2) +k(1:2,3:4);
  K(2*node2-1:2*node2, 2*node1-1:2*node1) ...
            =K(2*node2-1:2*node2, 2*node1-1:2*node1) +k(3:4,1:2);
  K(2*node2-1:2*node2, 2*node2-1:2*node2) ...
            =K(2*node2-1:2*node2, 2*node2-1:2*node2) +k(3:4,3:4);            
end


% for-loop: set up the global force vector
for i=1:size(forces,1)
  node = forces(i,1);
  direction =  forces(i,2);
  F(2*node + direction - 2)= forces(i,3);
end
F
% next block: apply the displacement boundary condition using the penalty method
Kold=K;
scale=max(max(K))
for i=1:size(bcsdisp,1)
  node = bcsdisp(i,1);
  direction = bcsdisp(i,2);
  K(2*node + direction - 2, 2*node + direction - 2)= scale*1e7;
  F(2*node + direction - 2)= scale*1e7*bcsdisp(i,3);
end

%solve
d=K\F

% next block: postprocessing
rection_force=Kold*d  % support reactions
stress=zeros(nElements,1); % stresses: initialization 
for e=1:nElements
  node1=elements(e,2);
  node2=elements(e,3);
  c=cos(phi(e));
  s=sin(phi(e));
  d_vector= [d(node1*2 -1) d(node1*2) d(node2*2 -1)  d(node2*2)]';
  stress(e)=E/length(e)*[-c -s c s]*d_vector;
end

stress

% next block: plotting
figure(1);
scale=5;
d=d*scale;
clf;
hold on;
for e=1:nElements
  nid1=elements(e,2);
  nid2=elements(e,3);
  plot([nodes(nid1,2)+d(2*nid1-1) nodes(nid2,2)+d(2*nid2-1)], ...
       [nodes(nid1,3)+d(2*nid1)   nodes(nid2,3)+d(2*nid2)],'-');
end
for e=1:nElements
  nid1=elements(e,2);
  nid2=elements(e,3);
  plot([nodes(nid1,2) nodes(nid2,2)], ...
       [nodes(nid1,3) nodes(nid2,3)],'k--');
end
