clear all;
nx=20; ny=4; nz=4;  % input parameters

% next block: initialization
dx=2/nx;
dy=0.2/ny;
dz=0.2/nz;
nid=ones(ny+1,nz+1,nx+1)*-1;
nodes=zeros((2*(ny+1)+nz-1)*(nx+1),4);
elements=zeros((2*ny+nz)*nx*2,4);

% for-loop: create nodes for the top and bottom plates
p=1;
for i=1:nx+1
  for j=1:ny+1
    for k=1:nz+1
       if k==1 || k==nz+1 
          nid(i,j,k)=p;
          nodes(p,1:4)=[p 2-(i-1)*dx (j-1)*dy (k-1)*dz];
          p=p+1;
       end
     end
   end
 end
% for-loop: create nodes for the vertical plate
for i=1:nx+1
    for k=2:nz
        nid(i,ny/2+1,k)=p;
        nodes(p,1:4)=[p 2-(i-1)*dx (ny/2)*dy (k-1)*dz];
        p=p+1;
     end
end

% for-loop: create elements for the top and bottom plates
p=1;
for i=1:nx
  for j=1:ny
    elements(p,1:4)=[p nid(i,j,1) nid(i,j+1,1) nid(i+1,j+1,1)];
    p=p+1;
    elements(p,1:4)=[p nid(i,j,1) nid(i+1,j+1,1) nid(i+1,j,1)];
    p=p+1;
    elements(p,1:4)=[p nid(i,j,nz+1) nid(i,j+1,nz+1) nid(i+1,j+1,nz+1)];
    p=p+1;
    elements(p,1:4)=[p nid(i,j,nz+1) nid(i+1,j+1,nz+1) nid(i+1,j,nz+1)];
    p=p+1;
  end
end
% for-loop: create elements for the vertical plate
for i=1:nx
  for k=1:nz
    elements(p,1:4)=[p nid(i,ny/2+1,k) nid(i+1,ny/2+1,k+1) nid(i,ny/2+1,k+1) ];
    p=p+1;
    elements(p,1:4)=[p nid(i,ny/2+1,k) nid(i+1,ny/2+1,k) nid(i+1,ny/2+1,k+1) ];
    p=p+1;
  end
end
    
nNodes=size(nodes,1);
nElements=size(elements,1);
hNodes=2*(ny+1)*(nx+1);       % number of nodes on the top and bottom plates
vNodes=nNodes-hNodes;         % number of nodes on the vertical plate
hElements=4*ny*nx;            % number of elements on the top and bottom plates
vElements=nElements-hElements;% number of elements on the vertical plate

% next block: set up force and displacement boundary conditions
forces(1,1:4)=[ny+2 100 0 0];
k=1;
for i=1:nNodes
  x=nodes(i,2);
  if x==0 && i<=hNodes
    bcsdisp(k:k+2,1:3)=[i 1 0; i 2 0; i 3 0];
    k=k+3;
  elseif x==0
    bcsdisp(k,1:3)=[i 1 0];
    k=k+1;
  end
end

% next block: material properties and initializations
E=200e9;
nu=0.3;
thk=0.005;
K=zeros(3*hNodes+vNodes,3*hNodes+vNodes);
F=zeros(3*hNodes+vNodes,1);
r=zeros(9,9);
[r(1,1),r(2,2),r(3,3),r(4,1),r(5,2),r(6,3)]=deal(1);
[r(7,1),r(8,2),r(9,3)]=deal(1);
Lambda=zeros(9,9);

% for-loop: compute the (partial) global stiffness matrix for 
% the top and bottom plates
for e=1:hElements
  elnodes=elements(e,2:4);
  X=nodes(elnodes,2);  Y=nodes(elnodes,3);  Z=nodes(elnodes,4);
  v12=[X(2)-X(1) Y(2)-Y(1) Z(2)-Z(1)]';
  length_12=norm(v12);
  oy=v12/length_12;
  v13=[X(3)-X(1) Y(3)-Y(1) Z(3)-Z(1)]';
  proj_13=v13'*oy;
  ox=v13-proj_13*oy;
  length_ox=norm(ox);
  ox=ox/length_ox;
  OX=[1 0 0]'; OY=[0 1 0]';
  lox=ox'*OX; mox=ox'*OY;
  loy=oy'*OX; moy=oy'*OY;
  x=[0 0 length_ox]';
  y=[0 length_12 proj_13]';
  % Z matrix 
  r(4,3)=y(2); r(4,6)=y(2)^2; r(4,9)=y(2)^3; 
  r(5,5)=y(2); r(5,8)=y(2)^2;
  r(6,6)=2*y(2); r(6,9)=3*y(2)^2;
  r(7,2)=x(3); r(7,3)=y(3); r(7,4)=x(3)^2;
  r(7,5)=x(3)*y(3); r(7,6)=y(3)^2; r(7,7)=x(3)^3;
  r(7,8)=x(3)^2*y(3)+ x(3)*y(3)^2; r(7,9)=y(3)^3;
  r(8,4)=2*x(3); r(8,5)=y(3); 
  r(8,7)=3*x(3)^2; r(8,8)=y(3)^2 + 2*x(3)*y(3);
  r(9,5)=x(3); r(9,6)=2*y(3); 
  r(9,8)=2*x(3)*y(3) + x(3)^2; r(9,9)=3*y(3)^2;
  % integrals
  I=1/2*x(3)*y(2);
  Ix=1/6*x(3)^2*y(2);
  Iy=1/6*x(3)*y(2)*(y(2)+y(3));
  Ix2=1/12*x(3)^3*y(2);
  Ixy=1/24*x(3)^2*y(2)*(y(2)+2*y(3));
  Iy2=1/12*x(3)*y(2)*(y(2)^2+y(2)*y(3)+y(3)^2);
  % set up element k matrix
  k=zeros(9,9);
  k(4,4)=4*I; k(5,5)=2*(1-nu)*I; k(6,4)=4*nu*I;
  k(6,6)=4*I; k(7,4)=12*Ix; k(7,6)=12*nu*Ix;
  k(7,7)=36*Ix2; k(8,4)=4*(nu*Ix + Iy);
  k(8,5)=4*(1-nu)*(Ix + Iy); k(8,6)=4*(Ix+nu*Iy);
  k(8,7)=12*nu*Ix2 + 12*Ixy; 
  k(8,8)=(12-8*nu)*(Ix2 + 2*Ixy + Iy2) - 8*(1-nu)*Ixy;
  k(9,4)=12*nu*Iy; k(9,6)=12*Iy; k(9,7)=36*nu*Ixy;
  k(9,8)=12*Ixy+ 12*nu*Iy2; k(9,9)=36*Iy2;
  k=k+k'-eye(9).*diag(k);
  k=k*(E*thk^3/(12*(1-nu^2)));
  % set up coordinate transformation matrix
  lambda=[-1 0 0; 0 lox mox;  0 loy moy];
  Lambda(1:3,1:3)=lambda;
  Lambda(4:6,4:6)=lambda;
  Lambda(7:9,7:9)=lambda; 
  ir=inv(r);
  k=Lambda'*(ir'*k*ir)*Lambda;
  % assemble global K matrix
  for i=1:3
    ni=elnodes(i);
    for j=1:3
      nj=elnodes(j);
      K(3*ni-2:3*ni, 3*nj-2:3*nj)=K(3*ni-2:3*ni, 3*nj-2:3*nj)... 
                                  +k(i*3-2:i*3, j*3-2:j*3);
    end
  end
end

% for-loop: compute the (partial) global stiffness matrix 
% for the vertical plate
for e=hElements+1:hElements+vElements
  elnodes=elements(e,2:4);
  X=nodes(elnodes,2);  Y=nodes(elnodes,3);  Z=nodes(elnodes,4);
  v12=[X(2)-X(1) Y(2)-Y(1) Z(2)-Z(1)]';
  length_12=norm(v12);
  oy=v12/length_12;
  v13=[X(3)-X(1) Y(3)-Y(1) Z(3)-Z(1)]';
  proj_13=v13'*oy;
  ox=v13-proj_13*oy;
  length_ox=norm(ox);
  ox=ox/length_ox;
  OX=[1 0 0]'; OY=[0 1 0]'; OZ=[0 0 1]';
  lox=ox'*OX; mox=ox'*OY; nox=ox'*OZ; 
  loy=oy'*OX; moy=oy'*OY; noy=oy'*OZ;
  x=[0 0 length_ox]';
  y=[0 length_12 proj_13]';

  % set up element k matrix
  x21=x(2)-x(1); x31=x(3)-x(1); x32=x(3)-x(2);
  y21=y(2)-y(1); y31=y(3)-y(1); y32=y(3)-y(2);
  Area=det([1 x(1) y(1); 1 x(2) y(2); 1 x(3) y(3)])/2; 

  k1=[y32^2   0 0 0 0 0
     -nu*y32*x32   x32^2 0 0 0 0
      -y32*y31    nu*x32*y31   y31^2 0 0 0
      nu*y32*x31   -x32*x31   -nu*y31*x31  x31^2 0 0
      y32*y21     -nu*x32*y21  -y31*y21   nu*x31*y21  y21^2 0
      -nu*y32*x21   x32*x21    nu*y31*x21  -x31*x21 -nu*y21*x21 x21^2];
  k1=k1+k1'- eye(size(k1,1)).*diag(k1);
  k1=k1*(E*thk/4/Area/(1-nu^2));

  k2=[x32^2   0 0 0 0 0
     -x32*y32     y32^2 0 0 0 0
      -x32*x31    y32*x31      x31^2 0 0 0
      x32*y31    -y32*y31     -x31*y31    y31^2 0 0
      x32*x21     -y32*x21     -x31*x21   y31*x21    x21^2 0
      -x32*y21   y32*y21      x31*y21    -y31*y21  -x21*y21  y21^2];  
  k2=k2+k2'- eye(size(k2,1)).*diag(k2);
  k2=k2*(E*thk/8/Area/(1+nu));
  
  k=k1+k2;  
  % set up coordinate transformation matrix
  lambda=[lox mox nox; loy moy noy];
  Lambda=zeros(6,9);
  Lambda(1:2,1:3)=lambda;
  Lambda(3:4,4:6)=lambda;
  Lambda(5:6,7:9)=lambda; 
  k=Lambda'*k*Lambda;
  k([1 2 4 5 7 8],:)=[];
  k(:,[1 2 4 5 7 8])=[];
  % assemble global K matrix
  for i=1:3
    if elnodes(i) <= hNodes; ni=3*elnodes(i)-2;
    else ni=3*hNodes + elnodes(i)-hNodes;
    end
    for j=1:3
      if elnodes(j) <= hNodes; nj=3*elnodes(j)-2;
      else nj=3*hNodes + elnodes(j)-hNodes;
      end
      K(ni,nj)=K(ni,nj)+k(i,j);
    end
  end
end

% set up the global force vector
for i=1:size(forces,1)
  node = forces(i,1);
  F(3*node-2:3*node)= forces(i,2:4);
end

% apply the displacement boundary condition using the penalty method
penalty=abs(max(max(K)))*1e7;
for i=1:size(bcsdisp,1)
  node = bcsdisp(i,1);
  if node<= hNodes
    disp_type = bcsdisp(i,2);
    K(3*node + disp_type - 3, 3*node + disp_type - 3)= penalty;
    F(3*node + disp_type - 3)= penalty*bcsdisp(i,3);
  else
    K(2*hNodes + node, 2*hNodes + node)= penalty;
    F(2*hNodes,1)= penalty*bcsdisp(i,3);
  end
end

d=K\F; %solve
d=d*20000;   % scale the displacements for visualization

% next block: setup deformed nodes
dnodes=nodes;
for i=1:nNodes
  if i<= hNodes
    dnodes(i,4)=nodes(i,4)+ d(i*3-2);
  else
    dnodes(i,4)=nodes(i,4)+ d(2*hNodes + i);
  end
end

% for-loop: plot the undeformed plate
figure(1)
clf;
hold on
for e=1:nElements
  for j=2:3
    n1=nodes(elements(e,j),2:4);
    n2=nodes(elements(e,j+1),2:4);
    plot3([n1(1) n2(1)],[n1(2) n2(2)],[n1(3) n2(3)],'k--');
  end 
  n1=nodes(elements(e,2),2:4);
  plot3([n1(1) n2(1)],[n1(2) n2(2)],[n1(3) n2(3)],'k--');
end
% next block: plot the deformed plate
p=patch('Vertices',dnodes(:,2:4),'Faces',elements(:,2:4));
set(p,'facecolor',[0.7 0.7 0.7],'edgecolor','black');
axis([0 2 -0.2 .4 -0.2 .4]);
xlabel('X'); ylabel('Y'); zlabel('Z');
view(30,20);