%--------------------------------------------------
% read the input files
%--------------------------------------------------
load nodes.dat;        
load elements.dat;
load materials.dat;
load bcsforce.dat;
load bcsdisp.dat;
load options.dat

%--------------------------------------------------
% Global material properties and constants
%--------------------------------------------------
E=materials(1,1);
nu=materials(2,1);
rho=materials(3,1);
dampK=materials(4,1);
dampM=materials(5,1);
dimension=options(1,1);
thickness=options(2,1);
timePeriod=options(4,1);
dt=options(5,1);
probeNode=options(6,1);

%--------------------------------------------------
% bookkeeping 
%--------------------------------------------------
n_nodes=size(nodes,1);
n_elements=size(elements,1);
n_bcsforce=size(bcsforce,1);
n_bcsdisp=size(bcsdisp,1);

n_timeSteps=timePeriod/dt+1;
U=zeros(n_nodes*dimension,n_timeSteps);
V=zeros(n_nodes*dimension,n_timeSteps);
A=zeros(n_nodes*dimension,n_timeSteps);

%--------------------------------------------------
% compute global K matrix
%--------------------------------------------------
K=CompK(nodes, elements, materials);
%--------------------------------------------------
% compute global K matrix
%--------------------------------------------------
M=CompM(nodes, elements, rho);

%--------------------------------------------------
% compute global C matrix
%--------------------------------------------------
%C=CompC(K, M, dampK, dampM);
%--------------------------------------------------
% compute global F vector
%--------------------------------------------------
F=CompF(nodes, elements, thickness, bcsforce);

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
F(drows,:)=[];

%u=K\F;

%[ev,e]=eig(K);

%u=ev(:,246);

e=eig(K)

return;

row=1;
for j=1:size(u,1)
  while U(row)==0
    row=row+1;
  end
  U(row)=u(j);
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


%plot(Uout, 'LineWidth',2);
%axis([0 2100 -1.2e-3 0]);
%xlabel('Time step');
%ylabel('Displacement (m)');
%set(gca,'fontsize',16);
