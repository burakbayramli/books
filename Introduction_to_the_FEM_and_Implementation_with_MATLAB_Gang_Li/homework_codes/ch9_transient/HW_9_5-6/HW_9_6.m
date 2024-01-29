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
dt=options(5,1);
probeNode=options(6,1);

% next block: bookkeeping 
n_nodes=size(nodes,1);
n_elements=size(elements,1);
n_bcsforce=size(bcsforce,1);
n_bcsdisp=size(bcsdisp,1);
n_timeSteps=round(timePeriod/dt)+1;

% next block: set up empty matrices 
U=zeros(n_nodes*dimension,2);
V=zeros(n_nodes*dimension,2);
A=zeros(n_nodes*dimension,2);

K=CompK(nodes, elements, materials);   % compute global K matrix
M=CompM(nodes, elements, rho);         % compute global M matrix
vM=sum(M,2);

C = dampM*M;                           % global C matrix (ignore K)
vC= diag(C);                           
F=CompF(nodes, elements, thickness, bcsforce); % compute global F vector

% next block: prepare for time marching using the explicit scheme
beta = 0.25;
gamma = 0.5;
LHS = M*(1.0/(beta*dt*dt))+ C*(gamma/(beta*dt)) + K; 
penalty=max(max(abs(LHS)))*1e+6;
A(:,1)=F./vM;  % initilization of accelecration
LHS=vM+0.5*dt*vC;
Uout=zeros(n_timeSteps,1);

% for-loop: time marching using the explicit scheme
for t=2:n_timeSteps
  c1=2-mod(t,2);  % U, V, A column for current time step
  c2=3-c1;        % U, V, A column for previous time step

  u1= U(:,c2); 
  vel1 = V(:,c2); 
  accel1 = A(:,c2);

  rhsvec1 = u1 + vel1*dt + accel1*(.5*dt*dt);
  rhsvec2 = vel1 + 0.5*dt*accel1;
  RHS=F- vC.*rhsvec2 - K*rhsvec1;
  A(:,c1)=RHS./LHS;
  V(:,c1)=V(:,c2) + A(:,c2)*.5*dt + A(:,c1)*dt*.5;
  U(:,c1)=U(:,c2) + dt*V(:,c2) + 0.5*dt*dt*A(:,c2);

  for j=1:size(bcsdisp,1);    
    nid=bcsdisp(j,1);
    k=bcsdisp(j,2);
    U(dimension*(nid-1)+k,c1) =bcsdisp(j,3);
  end

  if rem(t*100,(n_timeSteps-1)*5)==0
    fprintf('%d %%\n',floor(t*100/(n_timeSteps-1)));
  end
  Uout(t)=U(probeNode*2,c1);
end

% next block: save the transient results in file, plot
figure(1);
%save -ascii -double Uout2.dat Uout  % very large file
%disp('Vertical displacement results stored in Uout.dat');
plot(Uout, 'k-','LineWidth',2);
axis([0  n_timeSteps -1.3e-3 0]);
xlabel('Time step');
ylabel('Displacement (m)');
set(gca,'fontsize',16);