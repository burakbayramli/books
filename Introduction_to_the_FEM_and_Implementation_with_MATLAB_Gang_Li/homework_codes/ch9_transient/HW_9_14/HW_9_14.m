clear all;

nx=200;    % number of elements in x-direction
ny=2;      % number of elements in y-direction

% next block: create mesh
[nodes,elements]=UniformMeshQuad4(0,0,20,0.2,nx,ny);
n_nodes=size(nodes,1);
n_elements=size(elements,1);

materials=[200e9 0.3 7800 0 0]';  % material properties

% next block: apply displacement boundary conditions
k=1;
for i=1:n_nodes
  if nodes(i,2)==0 && nodes(i,3)==0
    bcsdisp(k:k+1,1:3)=[i 1 0; i 2 0];
    k=k+2;
  end
  if nodes(i,2)==20 && nodes(i,3)==0
    bcsdisp(k,1:3)=[i 2 0];
    k=k+1;
  end
end

% next block: set up global material properties and constants
E=materials(1,1);
nu=materials(2,1);
rho=materials(3,1);
dampK=materials(4,1);
dampM=materials(5,1);
dimension=2;
thickness=4;
timePeriod=2;
dt=1e-3;
probeNode=n_nodes-nx/2;
n_bcsdisp=size(bcsdisp,1);
n_timeSteps=timePeriod/dt+1;

% next 3 lines: set up empty matrices 
U=zeros(n_nodes*dimension,n_timeSteps);
V=zeros(n_nodes*dimension,n_timeSteps);
A=zeros(n_nodes*dimension,n_timeSteps);

K=CompK(nodes, elements, materials);   % compute global K matrix
M=CompM(nodes, elements, rho);         % compute global M matrix
C = dampM*M + dampK*K;                 % compute global C matrix
F=CompF(nodes, elements, thickness, nx, ny, 0); % compute global F vector

% next block: time marching using Newmark Scheme
beta = 0.25;
gamma = 0.5;
LHS = M*(1.0/(beta*dt*dt))+ C*(gamma/(beta*dt)) + K; 
penalty=max(max(abs(LHS)))*1e+6;
A(:,1)=M\F;  % initilization of accelecration
k=1;
for t=2:n_timeSteps
  F=CompF(nodes, elements, thickness, nx, ny, (t-1)*dt); 
  [U,V,A]=Newmark(t,dt,beta, gamma, dimension,... 
          K,M,C,F,bcsdisp,penalty,U,V,A);
  if rem(t*100,(n_timeSteps-1)*5)==0
    fprintf('%d %%\n',floor(t*100/(n_timeSteps-1)));
    Sxy=CompStress(nodes, elements, materials, U(:,t));
    k=k+1;
    % if-block: plot stress if (k-1)*5% of the time has reached
    if k==3
      t*dt
      figure(2)
      scatter3(nodes(:,2),nodes(:,3),Sxy(:,2),500,Sxy(:,2),'s','filled'); 
      axis([0  20 -1 1 -2e6 2e6]); 
      view(0,90);
    end
  end
end

% next block:  plot transient displacement of the probe node
Uout=U(probeNode*2,:);
disp('Vertical displacement results stored in Uout.dat');
Uout(200:205)*1000
figure(1);
clf;
plot(Uout, 'LineWidth',2);
xlabel('Time step');
ylabel('Displacement (m)');
set(gca,'fontsize',16);