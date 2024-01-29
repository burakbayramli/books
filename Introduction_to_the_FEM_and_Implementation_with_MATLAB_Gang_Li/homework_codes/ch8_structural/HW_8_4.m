clear all;
% next block: preprocessing
nodes=[1 -4.5 0; 2 -3 0.4; 3 -3 0.9; 4 -1.5 0.8; ... 
       5 -1.5 1.8; 6 0 1.2; 7 0 2.7; 8 1.5 0.8; ...
       9 1.5 1.8; 10 3 0.4; 11 3 0.9; 12 4.5 0];
elements=[1 1 2; 2 3 1; 3 2 3; 4 2 4; 5 3 4;...
          6 3 5; 7 4 5; 8 4 6; 9 5 6; 10 5 7; 11 6 7;...
          12 6 8; 13 6 9; 14 7 9; 15 8 9; 16 8 10;...
          17 8 11; 18 9 11; 19 10 11; 20 10 12; 21 11 12];
bcsdisp=[12 1 0; 12 2 0; 1 2 0];
forces=[1 2 -1000; 3 2 -2000; 5 2 -2000; 7 2 -3000;...
        9 2 -2000; 11 2 -2000; 12 2 -1000];
nNodes=size(nodes,1);
nElements=size(elements,1);
E=2e9;
A=1e-3;
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
% next block: apply the displacement boundary condition using 
% the penalty method
Kold=K;
scale=max(max(K))
for i=1:size(bcsdisp,1)
  node = bcsdisp(i,1);
  direction = bcsdisp(i,2);
  K(2*node + direction - 2, 2*node + direction - 2)= scale*1e7;
  F(2*node + direction - 2)= scale*1e7*bcsdisp(i,3);
end

% solve
d=K\F
max(abs(d))

% next block: compute stress
rection_force=Kold*d   % support reactions
stress=zeros(nElements,1);  % stress: initialization
for e=1:nElements
  node1=elements(e,2);
  node2=elements(e,3);
  c=cos(phi(e));
  s=sin(phi(e));
  d_vector= [d(node1*2 -1) d(node1*2) d(node2*2 -1)  d(node2*2)]';
  stress(e)=E/length(e)*[-c -s c s]*d_vector;
end
stress      % print stress

% next block: plotting
figure(1);
scale=1;
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