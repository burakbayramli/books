clear all;
% next 3 lines: read the input files
filenames = {'nodes.dat','elements.dat','materials.dat', ...
             'options.dat','forces.dat', 'bcsdisp.dat'};  
for i = 1:numel(filenames); load(filenames{i}); end;
% next 11 lines: set up constants and empty matrices 
nNodes=size(nodes,1);
nElements=size(elements,1);
E=materials(1,1);
nu=materials(2,1);
thk=options(1,1);
K=zeros(3*nNodes,3*nNodes);
F=zeros(3*nNodes,1);
r=zeros(9,9);
[r(1,1),r(2,2),r(3,3),r(4,1),r(5,2),r(6,3)]=deal(1);
[r(7,1),r(8,2),r(9,3)]=deal(1);
Lambda=zeros(9,9);

% for-loop: compute the global stiffness matrix
for e=1:nElements
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
% set up the global force vector
for i=1:size(forces,1)
  node = forces(i,1);
  F(3*node-2:3*node)= forces(i,2:4);
end

% apply the displacement boundary condition using the penalty method
penalty=abs(max(max(K)))*1e7;
for i=1:size(bcsdisp,1)
  node = bcsdisp(i,1);
  disp_type = bcsdisp(i,2);
  K(3*node + disp_type - 3, 3*node + disp_type - 3)= penalty;
  F(3*node + disp_type - 3)= penalty*bcsdisp(i,3);
end

d=K\F %solve

%--- ploting the plate deformation
figure(1);
clf;  hold on;
% setup deformed nodes
dnodes=nodes;
for i=1:nNodes
  dnodes(i,4)=nodes(i,4)+ d(i*3-2);
end

% for-loop: plot the undeformed plate
for e=1:nElements
  for j=2:3
    n1=nodes(elements(e,j),2:4);
    n2=nodes(elements(e,j+1),2:4);
    plot3([n1(1) n2(1)],[n1(2) n2(2)],[n1(3) n2(3)],'k--');
  end 
  n1=nodes(elements(e,2),2:4);
  plot3([n1(1) n2(1)],[n1(2) n2(2)],[n1(3) n2(3)],'k--');
end
%plot the deformed plate
p=patch('Vertices',dnodes(:,2:4),'Faces',elements(:,2:4));
set(p,'facecolor',[0.7 0.7 0.7],'edgecolor','black');
%axis([-15 15 0 20 -1 0.2]);
xlabel('X'); ylabel('Y'); zlabel('Z');
view(110,20);