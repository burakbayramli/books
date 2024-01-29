clear all;
% next 3 lines: read the input files
filenames = {'nodes.dat','elements.dat','materials.dat', ...
             'options.dat','bcsforce.dat', 'bcsdisp.dat'};  
for i = 1:numel(filenames); load(filenames{i}); end;

% next block: set up global material properties and constants
E=materials(1,1);
nu=materials(2,1);
rho=materials(3,1);
dampK=materials(4,1);
dampM=materials(5,1);
dimension=options(1,1);
thickness=options(2,1);
timePeriod=options(4,1);
dt=1e-3;
probeNode=options(6,1);

% next block: bookkeeping 
n_nodes=size(nodes,1);
n_elements=size(elements,1);
n_bcsforce=size(bcsforce,1);
n_bcsdisp=size(bcsdisp,1);
n_timeSteps=round(timePeriod/dt)+1;

% next 3 lines: set up empty matrices 
U=zeros(n_nodes*dimension,n_timeSteps);
V=zeros(n_nodes*dimension,n_timeSteps);
A=zeros(n_nodes*dimension,n_timeSteps);

K=CompK(nodes, elements, materials);   % compute global K matrix
M=CompM(nodes, elements, rho);         % compute global M matrix
M=diag(sum(M,2));                      % compute lumped M
C = dampM*M + dampK*K;                 % compute global C matrix
F=CompF(nodes, elements, thickness, bcsforce); % compute global F vector

% next block: time marching using Newmark Scheme
beta = 0.25;
gamma = 0.5;
LHS = M*(1.0/(beta*dt*dt))+ C*(gamma/(beta*dt)) + K; 
penalty=max(max(abs(LHS)))*1e+6;
A(:,1)=M\F;  % initilization of accelecration
for t=2:n_timeSteps
  [U,V,A]=Newmark(t,dt,beta, gamma, dimension,... 
          K,M,C,F,bcsdisp,penalty,U,V,A);
  if rem(t*100,(n_timeSteps-1)*5)==0
    fprintf('%d %%\n',floor(t*100/(n_timeSteps-1)));
  end
end

% next block: save the transient results in file, plot
figure(1);
Uout=U(probeNode*2,:);
save -ascii -double Uout2.dat Uout
disp('Vertical displacement results stored in Uout.dat');
Uout(200:205)*1000
plot(Uout, 'g-','LineWidth',2);
axis([0 2100 -1.2e-3 0]);
xlabel('Time step');
ylabel('Displacement (m)');
set(gca,'fontsize',16);
hold on;