% Input elements, nodes, BCs and material properties 
% (from steps 2, 3, 4 and 5)
load elements.dat;      % load elements
load nodes.dat;         % load nodes
load materials.dat;     % load material properties
load bcs.dat;           % load boundary conditions
load bfs.dat;           % load body forces
load ndprops.dat;       % load nodal properties: cross section area
E=materials(1);         % get Young's modulus
n_elements=size(elements,1);  % number of elements
n_nodes=size(nodes,1);        % number of nodes
% Create empty global matrices and vectors for later use
K=zeros(n_nodes,n_nodes);        % empty K matrix
F=zeros(n_nodes,1);              % empty RHS F vector
% Location and weights of the Gauss points in the master element 
n_gauss_points=2;                % number of Gauss points
[gauss_xi, gauss_w] = Get1DGauss(n_gauss_points);
% N, Nxi: set up empty matrices storing the value of the shape 
% functions N and their derivatives Nxi (dN/dxi) evaluated at the 
% Guass points in the master element
N=zeros(2,n_gauss_points);        
Nxi=zeros(2,n_gauss_points);     
% Compute the shape functions and their derivatives at each Gauss point  
[N,Nxi]=CompShapeLinear1D(N, Nxi, gauss_xi);         
sv=zeros(2,1);      % set up body force vector                           
area=zeros(2,1);    % set up cross section area vector
% The for loop: compute element matrices and vectors and aseemble them 
% into the global matrix and RHS vector (Steps 6,7)
for i=1:n_elements  % loop over the elements                                    
  start_x= nodes(elements(i,2),2);  % x-coordinate of the start node                    
  end_x=  nodes(elements(i,3),2);   % x-coordinate of the end node  
  jacobian=CompJacobian1D([start_x end_x]', Nxi);   
  for g=1:n_gauss_points                               
    sv(g)=bfs(i,2)*N(1,g)+ bfs(i+1,2)*N(2,g);
    area(g)=ndprops(i,2)*N(1,g)+ ndprops(i+1,2)*N(2,g);
  end                                                  
  % Compute element force vector
  f=zeros(2,1);
  for m=1:2
  	for g=1:n_gauss_points    
  		f(m,1)=f(m,1)+sv(g)*N(m,g)*gauss_w(g)*jacobian(g); 
  	end
  end
  % Compute element stiffness matrix
  k=zeros(2,2);                                       
  for m=1:2                                       
    for n=1:2                                     
      for g=1:n_gauss_points                       
        k(m,n)=k(m,n) +area(g)*E ...                 
              *Nxi(m,g)*Nxi(n,g)*gauss_w(g)/jacobian(g); 
     end
    end
  end
  [K,F]=AssembleGlobalLinear1D(K, F, i, elements, k, f);
end
% for loop: apply loads, boundary conditions (Step 8)
for i=1:n_nodes-1:n_nodes                               
      if bcs(i,2)==1     
         penalty= abs(K(i,i)+1)*1e7;  % penalty method
         K(i,i)=penalty;                            
         F(i)=bcs(i,3)*penalty;
      end
      if bcs(i,2)==2                                    
  	    F(i)=F(i)+bcs(i,3)*ndprops(i,2);                            
      end
end
% Solve the global system equations (step 9)
u_nodes=K\F                                            
% Post processing (step 10)
figure(1);                                          
plot(nodes(:,1), u_nodes,'b-o','linewidth',2); % plot displacements  
set(gca,'fontsize',16);
xlabel('node number','fontsize',18);
ylabel('Displacement (in)','fontsize',18);

k=1; % set up index of the points for computing the stress
for i=1:n_elements   % loop over the elements   
  node1=elements(i,2); % global node number of the local node 1
  node2=elements(i,3); % global node number of the local node 2   
  for x=-1:0.1:1  % for points with spacing=0.1 in the master element
    [N,Nxi]=CompShapeLinear1D(zeros(2,1),zeros(2,1),x);      
    u=u_nodes(node1)*N(1)+u_nodes(node2)*N(2); % displacement u(x)    
    J=CompJacobian1D([nodes(node1,2) nodes(node2,2)]',Nxi); %Jacobian
    ux=u_nodes(node1)*Nxi(1)/J+u_nodes(node2)*Nxi(2)/J; % derivative of u(x)
    re(k,1)=nodes(node1,2)*N(1)+nodes(node2,2)*N(2);              
    re(k,2)=u;                                         
    re(k,3)=E*ux;                        
    k=k+1;                                             %
  end
end
% plot stress
figure(2);                   
plot(re(:,1),re(:,3),'r-','linewidth',2);      
set(gca,'fontsize',16);
xlabel('x (in)','fontsize',18);
ylabel('Stress (psi)','fontsize',18);