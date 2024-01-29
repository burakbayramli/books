clear;

[nodes elements]=UniformMeshQuad4(0,0,4,8,16,32);

n_nodes=size(nodes,1);           % number of nodes
n_elements=size(elements,1);     % number of elements

% next block: set wire temperature 
for i=1:n_nodes
  if nodes(i,2)==2 && nodes(i,3)==6
    Q=[i 2000];
  end
end

% next block: set up the edges that have convection BC
k=1;
for e=1:n_elements
  for j=1:4
    if nodes(elements(e,j+1),3)== 8 && nodes(elements(e,rem(j,4)+2),3)== 8
      convEdges(k,1:2)=[e j];
      k=k+1;
    end
  end
end

% global material properties and constants
kappa=ones(n_elements,1)*10;
cv=1;
rho=5;
extTemp=-5;
initTemp=-5;
timePeriod=100;
timeStep=2e-2;
probeNode=553;

nodalTemp=[];
n_nodalTemp=size(nodalTemp,1);   % number of nodal temperature
n_timeSteps=timePeriod/timeStep+1;
prevTemp=ones(n_nodes,1)*initTemp;
currTemp=prevTemp;
tempHistory=zeros(n_nodes,n_timeSteps);
tempHistory(:,1)=currTemp;

% compute K and F
K=CompK(nodes, elements, kappa, 0);   % compute global K matrix
[K F]=CompEdgeConvectionF(nodes, elements, kappa, 5, -5, convEdges, K); 
F(Q(:,1),1)=F(Q(:,1),1)+Q(:,2);
C=CompC(nodes, elements, cv, rho);     % global heat capacity matrix

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
plot([0:timeStep:size(Tout,2)*timeStep-timeStep], Tout,'b-','LineWidth',2);
xlabel('Time (s)');
ylabel('Temperature (C)');