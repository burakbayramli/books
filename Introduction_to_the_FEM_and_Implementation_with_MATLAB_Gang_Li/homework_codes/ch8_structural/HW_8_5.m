clear all;

n_division=5;  % number of divisions for each of the straight segment

% next block: set up model
nodes=zeros(3*n_division+1,4);
nodes(1:n_division+1,2:3)=[zeros(1,n_division+1);0:3/n_division:3]';
nodes(n_division+1:2*n_division+1,2:3)=[0:2/n_division:2;3:-1.5/n_division:1.5]';
nodes(2*n_division+1:3*n_division+1,2:3)=[ones(1,n_division+1)*2;...
                                          1.5:-1.5/n_division:0]';
n_nodes=size(nodes,1);
nodes(1:n_nodes,1)=[1:n_nodes]';
n_elements=3*n_division;
elements=zeros(n_elements,3);
elements(:,1)=[1:n_elements]';
elements(1:2*n_division,2:3)=[1:2*n_division; 2:2*n_division+1]';
elements(2*n_division+1:3*n_division,2:3)=...
    [2*n_division+2:3*n_division+1;2*n_division+1:3*n_division]';

materials=[1e9 3.8462e8]';
geometry=zeros(n_elements,6);
geometry(:,1)=[1:n_elements]';
geometry(:,3)=1e-2;
geometry(:,4)=8.3333e-06;
geometry(:,5)=8.3333e-06;
geometry(:,6)=1.6667e-05;
forces(1,:)=[n_division+1 1000 0 0 0 0 0];
forces(2,:)=[2*n_division+1 0 0 0 0 0 -5000];
bcsdisp=zeros(2*6,3);
bcsdisp(1:6,1:2)=[ones(1,6);1:6]';
bcsdisp(7:12,1:2)=[ones(1,6)*n_nodes;1:6]';

% next block: set up constants and empty matrices 
nNodes=size(nodes,1);
nElements=size(elements,1);
E=materials(1,1);
G=materials(2,1);
alpha=geometry(:,2)/180*pi;
A=geometry(:,3);
Iy=geometry(:,4);
Iz=geometry(:,5);
J=geometry(:,6);
K=zeros(6*nNodes,6*nNodes);
F=zeros(6*nNodes,1);
length=zeros(nElements,1);
direction_cos=zeros(nElements,3);

% for-loop: compute the global stiffness matrix
for e=1:nElements
  node1=elements(e,2);
  node2=elements(e,3);
  dv=nodes(node2,2:4)-nodes(node1,2:4)
  length(e)=norm(dv);
  direction_cos(e,:)=dv/length(e);
  % set up intermediate quantities
  EAL=E*A(e)/length(e);
  GJL=G*J(e)/length(e);
  EIzL=E*Iz(e)/(length(e));
  EIyL=E*Iy(e)/(length(e));
  EIzL2=EIzL/length(e);
  EIyL2=EIyL/length(e);
  EIzL3=EIzL2/length(e);
  EIyL3=EIyL2/length(e);
  % next 9 lines: element k matrix in local coordinate system 
  k=zeros(12,12);
  k(1,1)=EAL;  k(2,2)=12*EIzL3;  k(3,3)=12*EIyL3; k(4,4)=GJL;
  k(5,3)=-6*EIyL2; k(5,5)=4*EIyL; k(6,2)=6*EIzL2; k(6,6)=4*EIzL;
  k(7,1)=-EAL;  k(7,7)=EAL;  k(8,2)=-12*EIzL3;  k(8,6)=-6*EIzL2;
  k(8,8)=12*EIzL3;  k(9,3)=-12*EIyL3;  k(9,5)=6*EIyL2;
  k(9,9)=12*EIyL3;  k(10,4)=-GJL;  k(10,10)=GJL;  k(11,3)=-6*EIyL2;
  k(11,5)=2*EIyL;  k(11,9)=6*EIyL2;  k(11,11)=4*EIyL;  k(12,2)=6*EIzL2;
  k(12,6)=2*EIzL;  k(12,8)=-6*EIzL2;  k(12,12)=4*EIzL;
  k=k+k'-eye(12).*diag(k);
  % next 16 lines: calculate lambda
  d=sqrt(dv(1)^2 + dv(3)^2)
  if abs(d)>1e-10*length(e)
    lambda1=zeros(3,3);
    lambda1(1,:)=direction_cos(e,:);
    lambda1(2,:)=1/(d*length(e))*...
          [-dv(1)*dv(2) dv(1)^2+dv(3)^2 -dv(2)*dv(3)];
    lambda1(3,:)=1/d*[-dv(3) 0 dv(1)];
    lambda2=[1  0    0
            0 cos(alpha(e)) sin(alpha(e))
            0 -sin(alpha(e)) cos(alpha(e))];
    lambda=lambda1*lambda2;
  else
    lambda=[0 1 0
          -cos(alpha(e)) 0 sin(alpha(e))
           sin(alpha(e)) 0 cos(alpha(e))];
  end
  % for-loop: create the big lambda matrix
  for i=0:3
    Lambda(i*3+1:i*3+3,i*3+1:i*3+3)=lambda;
  end
  k=inv(Lambda)*k*Lambda; % transform to global coordinate system
  % assembly block
  K(6*node1-5:6*node1, 6*node1-5:6*node1) ...
            =K(6*node1-5:6*node1, 6*node1-5:6*node1) +k(1:6,1:6);
  K(6*node1-5:6*node1, 6*node2-5:6*node2) ...
            =K(6*node1-5:6*node1, 6*node2-5:6*node2) +k(1:6,7:12);
  K(6*node2-5:6*node2, 6*node1-5:6*node1) ...
            =K(6*node2-5:6*node2, 6*node1-5:6*node1) +k(7:12,1:6);
  K(6*node2-5:6*node2, 6*node2-5:6*node2) ...
            =K(6*node2-5:6*node2, 6*node2-5:6*node2) +k(7:12,7:12);            
end

% set up the global force vector
for i=1:size(forces,1)
  node = forces(i,1);
  F(6*node-5:6*node)= forces(i,2:7);
end

% next 7 lines: apply the displacement BC using the penalty method
penalty=abs(max(max(K)))*1e7;
for i=1:size(bcsdisp,1)
  node = bcsdisp(i,1);
  disp_type = bcsdisp(i,2);
  K(6*node + disp_type - 6, 6*node + disp_type - 6)= penalty;
  F(6*node + disp_type - 6)= penalty*bcsdisp(i,3);
end

d=K\F  % solve global linear system

d(2*n_division*6+1)

% next block: plotting
figure(1);
d=d*1;
clf;
hold on;
% for-loop: plot the deformed and undeformed structure
for e=1:nElements
  nd1=elements(e,2);
  nd2=elements(e,3);
  x1=nodes(nd1,2); y1=nodes(nd1,4); z1=nodes(nd1,3);
  x2=nodes(nd2,2); y2=nodes(nd2,4); z2=nodes(nd2,3);
  plot3([x1 x2], [y1 y2], [z1 z2],'k--');       
  plot3([x1+d(6*nd1-5) x2+d(6*nd2-5)],[y1+d(6*nd1-3) ...
         y2+d(6*nd2-3)], [z1+d(6*nd1-4) z2+d(6*nd2-4)],...
         'k-','LineWidth',3);
end
set(gca,'Ydir','reverse');    % reverse the y-axis for plotting
xlabel('X'); ylabel('Z'); zlabel('Y');
view(0,0);
hold off;