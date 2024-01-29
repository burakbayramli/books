clear all;

n_div=5;  % number of divisions for each of the straight segment

% next block: geometric model
keypts=[1 -4.5 0; 2 -3 0.4; 3 -3 0.9; 4 -1.5 0.8; ... 
       5 -1.5 1.8; 6 0 1.2; 7 0 2.7; 8 1.5 0.8; ...
       9 1.5 1.8; 10 3 0.4; 11 3 0.9; 12 4.5 0];
segments=[1 1 2; 2 3 1; 3 2 3; 4 2 4; 5 3 4;...
          6 3 5; 7 4 5; 8 4 6; 9 5 6; 10 5 7; 11 6 7;...
          12 6 8; 13 6 9; 14 7 9; 15 8 9; 16 8 10;...
          17 8 11; 18 9 11; 19 10 11; 20 10 12; 21 11 12];

% next blocks: discretization, generate nodes and elements
n_keypts=size(keypts,1);
n_segments=size(segments,1);
n_nodes=n_segments*(n_div-1)+ n_keypts;
n_elements=n_segments*n_div;

nodes=zeros(n_nodes,4);
elements=zeros(n_elements,3);
nodes(1:n_keypts,1:3)=keypts;

k=n_keypts+1;
e=1;
for s=1:size(segments,1)
  sx=keypts(segments(s,2),2); 
  sy=keypts(segments(s,2),3);
  ex=keypts(segments(s,3),2); 
  ey=keypts(segments(s,3),3);
  if n_div>1
    x=linspace(sx,ex,n_div+1);
    y=linspace(sy,ey,n_div+1);
    elements(e,1:3)=[e segments(s,2) k];
    e=e+1;
    for i=2:n_div-1
      nodes(k,1:3)=[k x(i) y(i)];
      elements(e, 1:3)=[e k k+1];
      e=e+1;
      k=k+1;
    end
    nodes(k,1:3)=[k x(n_div) y(n_div)];
    elements(e,1:3)=[e k segments(s,3)];
    e=e+1;
    k=k+1;
  elseif n_div==1
    elements(e,1:3)=[e segments(s,2) segments(s,3)];
    e=e+1;
  else
    return;   % if wrong n_div
  end
end

nodes(1:n_nodes,1)=[1:n_nodes]';

materials=[2e9 8e8 2330]';   % material properties

% next block: cross-sectional properties
geometry=zeros(n_elements,6);
geometry(:,1)=[1:n_elements]';
geometry(:,3)=1e-5;
geometry(:,4)=8.3333e-12;
geometry(:,5)=8.3333e-12;
geometry(:,6)=1.6667e-11;

% next block: applied forces
forces(1,:)=[1 0 -1000 0 0 0 0];
forces(2,:)=[3 0 -2000 0 0 0 0];
forces(3,:)=[5 0 -2000 0 0 0 0];
forces(4,:)=[7 0 -3000 0 0 0 0];
forces(5,:)=[9 0 -2000 0 0 0 0];
forces(6,:)=[11 0 -2000 0 0 0 0];
forces(7,:)=[12 0 -1000 0 0 0 0];

% next block: displacement boundary conditions
bcsdisp=zeros(9,3);
bcsdisp(1:5,1:2)=[ones(1,5);[1 2 3 4 5]]';
bcsdisp(6:9,1:2)=[ones(1,4)*12;[2 3 4 5]]';


% next block: set up constants and empty matrices 
nNodes=size(nodes,1);
nElements=size(elements,1);
E=materials(1,1);
G=materials(2,1);
rho=materials(3,1);
alpha=geometry(:,2)/180*pi;
A=geometry(:,3);
Iy=geometry(:,4);
Iz=geometry(:,5);
J=geometry(:,6);
K=zeros(6*nNodes,6*nNodes);
M=zeros(6*nNodes,6*nNodes);
length=zeros(nElements,1);
direction_cos=zeros(nElements,3);

% for-loop: compute the global stiffness matrix
for e=1:nElements
  node1=elements(e,2);
  node2=elements(e,3);
  dv=nodes(node2,2:4)-nodes(node1,2:4);
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
  L=length(e);
  JA=J(e)/A(e);
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

  me=zeros(12,12);
  me(1,1)=140; me(2,2)=156; me(3,3)=156; me(4,4)=140*JA; 
  me(5,3)=-22*L; me(5,5)=4*L^2; me(6,2)=22*L; me(6,6)=4*L^2;
  me(7,1)=70;  me(7,7)=140; me(8,2)=54; me(8,6)=13*L;
  me(8,8)=156; me(9,3)=54; me(9,5)=-13*L; 
  me(9,9)=156; me(10,4)=70*JA; me(10,10)=140*JA; me(11,3)=13*L;
  me(11,5)=-3*L^2; me(11,9)=22*L; me(11,11)=4*L^2; me(12,2)=-13*L;
  me(12,6)=-3*L^2; me(12,8)=-22*L; me(12,12)=4*L^2;
  me=me+me'-eye(12).*diag(me);
  me=me*(rho*A(e)*L/420);

  % next 16 lines: calculate lambda
  d=sqrt(dv(1)^2 + dv(3)^2);
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
  me=inv(Lambda)*me*Lambda;
  % assembly 
  K(6*node1-5:6*node1, 6*node1-5:6*node1) ...
            =K(6*node1-5:6*node1, 6*node1-5:6*node1) +k(1:6,1:6);
  K(6*node1-5:6*node1, 6*node2-5:6*node2) ...
            =K(6*node1-5:6*node1, 6*node2-5:6*node2) +k(1:6,7:12);
  K(6*node2-5:6*node2, 6*node1-5:6*node1) ...
            =K(6*node2-5:6*node2, 6*node1-5:6*node1) +k(7:12,1:6);
  K(6*node2-5:6*node2, 6*node2-5:6*node2) ...
            =K(6*node2-5:6*node2, 6*node2-5:6*node2) +k(7:12,7:12);

  M(6*node1-5:6*node1, 6*node1-5:6*node1) ...
            =M(6*node1-5:6*node1, 6*node1-5:6*node1) +me(1:6,1:6);
  M(6*node1-5:6*node1, 6*node2-5:6*node2) ...
            =M(6*node1-5:6*node1, 6*node2-5:6*node2) +me(1:6,7:12);
  M(6*node2-5:6*node2, 6*node1-5:6*node1) ...
            =M(6*node2-5:6*node2, 6*node1-5:6*node1) +me(7:12,1:6);
  M(6*node2-5:6*node2, 6*node2-5:6*node2) ...
            =M(6*node2-5:6*node2, 6*node2-5:6*node2) +me(7:12,7:12);     
end


U=ones(n_nodes*6,1);
drows=zeros(size(bcsdisp,1),1);
for j=1:size(bcsdisp,1);    
  nid=bcsdisp(j,1);
  k=bcsdisp(j,2);
  row=6*(nid-1)+k;
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


% next block: plotting
figure(1);
U=U*1;
clf;
hold on;
% for-loop: plot the deformed and undeformed structure
for e=1:nElements
  nd1=elements(e,2);
  nd2=elements(e,3);
  x1=nodes(nd1,2); y1=nodes(nd1,4); z1=nodes(nd1,3);
  x2=nodes(nd2,2); y2=nodes(nd2,4); z2=nodes(nd2,3);
  plot3([x1 x2], [y1 y2], [z1 z2],'k--');       
  plot3([x1+U(6*nd1-5) x2+U(6*nd2-5)],[y1+U(6*nd1-3) ...
         y2+U(6*nd2-3)], [z1+U(6*nd1-4) z2+U(6*nd2-4)],...
         'k-','LineWidth',3);
end
set(gca,'Ydir','reverse');    % reverse the y-axis for plotting
xlabel('X'); ylabel('Z'); zlabel('Y');
view(0,0);
hold off;
