clear all;
% next 1 line: user defined parameter
n_elements=100;  
% next block: define the geometry, elements, and nodes 
length=1;                      %the length of the domain
left_end=0;
length_element=length/n_elements;
n_nodes=n_elements+1;
elements(:,1)=[1:n_elements];
elements(:,2)=[1:n_nodes-1];
elements(:,3)=[2:n_nodes];
nodes(:,1)=[1:n_nodes];
nodes(:,2)=[left_end:length_element:length];

% next block: set up boundary conditions and applied force
bcsdisp=[1 1 0];
bcsforce=[n_nodes, 100];

E=70e9;
rho=5000;
Area=0.01;
K=zeros(n_nodes,n_nodes);     % set up empty K matrix
M=zeros(n_nodes,n_nodes);     % set up empty M matrix
C=zeros(n_nodes,n_nodes);     % set up empty C matrix

n_gauss_points=2;             % number of Gauss points
[gauss_xi, gauss_w] = Get1DGauss(n_gauss_points); % Get Gauss points
N=zeros(2,n_gauss_points);    % set up shape function matrix    
Nxi=zeros(2,n_gauss_points);  % set up dN/dxi matrix 
[N,Nxi]=CompShapeLinear1D(N, Nxi, gauss_xi); % compute N, dN/dxi
                                              % at Gauss points   
% for-loop: compute element matrices and vectors and assemble the global system              
for i=1:n_elements  % loop over elements                                
  start_x= nodes(elements(i,2),2);                      
  end_x=  nodes(elements(i,3),2); 
  [element_nodes, node_id_map]= SetElementNodes(i, nodes, elements);
  jacobian=CompJacobian1D([start_x end_x]', Nxi);                                  
  ke=zeros(2,2);   % element matrix    
  me=zeros(2,2);                                  
  for m=1:2                                       
    for n=1:2                                     
      for g=1:n_gauss_points                       
        ke(m,n)=ke(m,n) + E*Area*Nxi(m,g)*Nxi(n,g)*gauss_w(g)/jacobian(g);
        me(m,n)=me(m,n)+rho*Area*N(m,g)*N(n,g)*gauss_w(g)*jacobian(g);
     end
    end
  end
  [K]= AssembleGlobalMatrix(K,ke,node_id_map,1);
  [M]= AssembleGlobalMatrix(M,me,node_id_map,1);
end

F=CompF(nodes, elements, bcsforce); % compute global F vector

% next block: time marching using Newmark Scheme
timePeriod=.003;
dt=1e-5;
n_timeSteps=round(timePeriod/dt)+1;
U=zeros(n_nodes,n_timeSteps);
V=zeros(n_nodes,n_timeSteps);
A=zeros(n_nodes,n_timeSteps);
beta = .25;
gamma = 0.5;
LHS = M*(1.0/(beta*dt*dt))+ C*(gamma/(beta*dt)) + K; 
penalty=max(max(abs(LHS)))*1e+6;
A(:,1)=M\F;  % initilization of accelecration
for t=2:n_timeSteps
  [U,V,A]=Newmark(t,dt,beta, gamma, 1,K,M,C,F,bcsdisp,penalty,U,V,A);
  if rem(t*100,(n_timeSteps-1)*5)==0
    fprintf('%d %%\n',floor(t*100/(n_timeSteps-1)));
  end
end

probeNode=n_nodes;
Uout=U(probeNode,:);

figure(1)
clf;
plot(Uout,'b-','LineWidth',2);
hold on
t=[1:n_timeSteps]*dt;
u2=100/Area/E*(1.-cos(sqrt(3*E/rho)*t/length));
plot(u2, 'r--','LineWidth',2);
axis([0 n_timeSteps*1.1 0 3.5e-7]);
xlabel('Time step');
ylabel('Displacement (m)');
legend('Numerical solution','Analytical solution');
set(gca,'fontsize',16);