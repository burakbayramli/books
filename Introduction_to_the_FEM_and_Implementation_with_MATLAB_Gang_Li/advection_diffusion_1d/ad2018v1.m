clear;
load elements.dat;      % load elements
load nodes.dat;         % load nodes
load globals.dat;       % load global properties
load bcs.dat;           % load boundary conditions
load bfs.dat;           % load volume source
v=globals(1);           % flow velocity
diffusivity=globals(2); % diffusivity
n_elements=size(elements,1);  % number of elements
n_nodes=size(nodes,1);        % number of nodes
K=zeros(n_nodes,n_nodes);     % set up empty K matrix
F=zeros(n_nodes,1);           % set up empty RHS F vector
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
  jacobian=CompJacobian1D([start_x end_x]', Nxi); 
  for g=1:n_gauss_points                                   
    sv(g)=bfs(i,2)*N(1,g)+ bfs(i+1,2)*N(2,g);  % body source
  end                                                  
  f=zeros(2,1);  % RHS vector
  for m=1:2
  	for g=1:n_gauss_points    
  		f(m,1)=f(m,1)+sv(g)*N(m,g)*gauss_w(g)*jacobian(g); 
  	end
  end
  k=zeros(2,2);   % diffusivity matrix                                      
  ka=zeros(2,2);  % advection matrix
  for m=1:2                                       
    for n=1:2                                     
      for g=1:n_gauss_points                       
        k(m,n)=k(m,n) +diffusivity ...                 
             *Nxi(m,g)*Nxi(n,g)*gauss_w(g)/jacobian(g);
        ka(m,n)=ka(m,n)+ v*N(m,g)*Nxi(n,g)*gauss_w(g);
     end
    end
  end
  [K,F]=assembleGlobalLinear1D(i, elements, k+ka, f, K, F);
end
% apply loads, boundary conditions(
for i=1:n_nodes-1:n_nodes                               
  if bcs(i,2)==1     
    penalty= abs(K(i,i))*1e7;
    K(i,i)=penalty;                            
    F(i)=bcs(i,3)*penalty;
  end
  if bcs(i,2)==2                                    
    F(i)=F(i)+bcs(i,3);                            
  end
end
C_nodes=K\F      % solve the global system equations

% post processing (step 10)
figure(1);                                             
plot(nodes(:,1), C_nodes,'b-o','linewidth',2);     
set(gca,'fontsize',16);
xlabel('node number','fontsize',18);
ylabel('Concentration {(mol/m}^3)','fontsize',18);
k=1; % set up index of the points for computing solute flux 
for i=1:n_elements     % loop over the elements     
  node1=elements(i,2); % global node number of the local node 1
  node2=elements(i,3); % global node number of the local node 2                         
  for x=-1:1:1
    [N,Nxi]=CompShapeLinear1D(zeros(2,1),zeros(2,1),x);
    C=C_nodes(node1)*N(1)+C_nodes(node2)*N(2);  % concentration 
    J=CompJacobian1D([nodes(node1,2) nodes(node2,2)]',Nxi); %Jacobian
    Cx=C_nodes(node1)*Nxi(1)/J+C_nodes(node2)*Nxi(2)/J; % dC(X)/dx
    re(k,1)=nodes(node1,2)*N(1)+nodes(node2,2)*N(2);                                                      
    re(k,2)=v*C-diffusivity*Cx; 
    k=k+1;                                                
   end
end
figure(2);              
plot(re(:,1),re(:,2),'r-','linewidth',2);     
set(gca,'fontsize',16);
xlabel('x (cm)','fontsize',18);
ylabel('Solute flux {(mol/s\cdotm}^2)','fontsize',18);