clear all;
% next 3 lines: read the input files
filenames = {'nodes.dat','elements.dat','materials.dat', ...
             'options.dat','edgeFlux.dat', 'nodalTemp.dat'};  
for i = 1:numel(filenames); load(filenames{i}); end;

% next 9 lines: set up global material properties and constants
kappa=materials(1,1);
cv=materials(2,1);
rho=materials(3,1);
thickness=options(2,1);
extTemp=options(4,1);
initTemp=options(5,1);
timePeriod=options(6,1);
timeStep=options(7,1);
probeNode=options(8,1);
% next 9 lines: bookkeeping 
n_nodes=size(nodes,1);
n_elements=size(elements,1);
n_edgeFlux=size(edgeFlux,1);
n_nodalTemp=size(nodalTemp,1);
n_timeSteps=timePeriod/timeStep+1;
prevTemp=ones(n_nodes,1)*initTemp;
currTemp=prevTemp;
tempHistory=zeros(n_nodes,n_timeSteps);
tempHistory(:,1)=currTemp;

K=CompK(nodes, elements, kappa);       % compute global K matrix
C=CompC(nodes, elements, cv, rho);     % global heat capacity matrix
F=CompF(nodes, elements, edgeFlux);    % compute global F vector

% next 10 lines: time integration
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

% next 3 lines: save the transient temperature results in file
Tout=tempHistory(probeNode,:);
save -ascii -double Tout.dat Tout
disp('Temperature results stored in Tout.dat');