clear all;
% next 1 line: user defined parameter
n_elements=100;  
% next block: define the geometry, elements, and nodes 
length=0.02;                      %the length of the domain
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

% next block: material properties and simulation parameters
kappa=150;
cv=1000;
Beta=300;
rho=2700;
extTemp=0;
initTemp=100;
timePeriod=1000;
timeStep=1e-1;
probeNode=n_nodes;  % select a node to track temperature change

nodalTemp=[];
n_nodalTemp=0;
n_timeSteps=timePeriod/timeStep+1;
prevTemp=ones(n_nodes,1)*initTemp;
currTemp=prevTemp;
tempHistory=zeros(n_nodes,n_timeSteps);
tempHistory(:,1)=currTemp;

K=zeros(n_nodes,n_nodes);     % set up empty K matrix
C=zeros(n_nodes,n_nodes);     % set up empty C matrix
F=zeros(n_nodes,1);           % set up empty F vector

n_gauss_points=2;             % number of Gauss points
[gauss_xi, gauss_w] = Get1DGauss(n_gauss_points); % Get Gauss points
N=zeros(2,n_gauss_points);    % set up shape function matrix    
Nxi=zeros(2,n_gauss_points);  % set up dN/dxi matrix 
[N,Nxi]=CompShapeLinear1D(N, Nxi, gauss_xi); % compute N, dN/dxi
                                              % at Gauss points   
% compute element matrices and vectors and assemble the global system              
for i=1:n_elements  % loop over elements                                
  start_x= nodes(elements(i,2),2);                      
  end_x=  nodes(elements(i,3),2); 
  [element_nodes, node_id_map]= SetElementNodes(i, nodes, elements);
  jacobian=CompJacobian1D([start_x end_x]', Nxi);                                  
  ke=zeros(2,2);   % element matrix    
  ce=zeros(2,2);                                  
  for m=1:2                                       
    for n=1:2                                     
      for g=1:n_gauss_points  
        xg=start_x*N(1,g) + end_x*N(2,g);                     
        ke(m,n)=ke(m,n) + kappa*xg*Nxi(m,g)*Nxi(n,g)*gauss_w(g)/jacobian(g);
        ce(m,n)=ce(m,n)+rho*cv*N(m,g)*N(n,g)*gauss_w(g)*jacobian(g);
     end
    end
  end
  [K]= AssembleGlobalMatrix(K,ke,node_id_map,1);
  [C]= AssembleGlobalMatrix(C,ce,node_id_map,1);
end

K(n_nodes, n_nodes)=K(n_nodes, n_nodes)+Beta;
F=zeros(n_nodes,1);

% next block: time integration
penalty=max(max(abs(K*0.5 + C*(1/timeStep))))*1e+6;
tempHistory(:,1)=currTemp;
for t=2:n_timeSteps
  currTemp=CrankNicolson(timeStep,K,C,F,nodalTemp,prevTemp,currTemp,penalty); 
  tempHistory(:,t)=currTemp; % record for this time step 
  prevTemp=currTemp;
  if rem(t*100,(n_timeSteps-1)*5)==0
    fprintf('%d %%\n',floor(t*100/(n_timeSteps-1)));
  end
end

% next block: save the transient temperature results in file
Tout=tempHistory(probeNode,:);
save -ascii -double Tout.dat Tout
disp('Temperature results stored in Tout.dat');


figure(1)
clf;
plot(Tout,'b-','LineWidth',2);
xlabel('Time step');
ylabel('Temperature (C)');
set(gca,'fontsize',16);

figure(2)
clf;
plot(tempHistory(:,11),'-','linewidth',2);
hold on;
plot(tempHistory(:,51),'-.','linewidth',2);
plot(tempHistory(:,201),'--','linewidth',2);
xlabel('Time step');
ylabel('Temperature (C)');
legend('t=1s','t=5s', 't=20s','location','southwest');
set(gca,'fontsize',16);

