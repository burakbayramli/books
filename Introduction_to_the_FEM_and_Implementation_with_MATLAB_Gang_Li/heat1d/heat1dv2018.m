clear;
% Input elements, nodes, BCs and material properties 
% (from steps 2, 3, 4 and 5)
load elements.dat;      % load elements
load nodes.dat;         % load nodes
load globals.dat;       % load global properties
load bcs.dat;           % load boundary conditions
load elprops.dat;       % load element properties: conductivity
hlA=2*globals(3)/globals(2);  % define 2h/r
hlAT_inf=hlA*globals(1);      % define 2hT_inf/r
n_elements=size(elements,1);  % number of elements
n_nodes=size(nodes,1);        % number of nodes
K=zeros(n_nodes,n_nodes);         % Set up empty K matrix
F=zeros(n_nodes,1);               % Set up empty RHS F vector
n_gauss_points=2;                 % number of Gauss points
[gauss_xi, gauss_w] = Get1DGauss(n_gauss_points); % Get Gauss points
% N, Nxi: set up empty matrices storing the value of the shape 
% functions N and their derivatives Nxi (dN/dxi) evaluated at the 
% Guass points in the master element
N=zeros(2,n_gauss_points);  
Nxi=zeros(2,n_gauss_points);     
% Compute the shape functions and their derivatives at Gauss points  
[N,Nxi]=CompShapeLinear1D(N, Nxi, gauss_xi);  
% The for loop: compute element matrices and vectors and aseemble them 
% into the global matrix and RHS vector (Steps 6,7)  
for i=1:n_elements  %loop over elements                                    
  start_x= nodes(elements(i,2),2);   % x-coordinate of the start node                    
  end_x=  nodes(elements(i,3),2);    % x-coordinate of the end node        
  jacobian=CompJacobian1D([start_x end_x]', Nxi);   
  f=hlAT_inf * N*(gauss_w.*jacobian);
  k=zeros(2,2); kt=zeros(2,2);       % Set up empty element matrices 
  % for loop: compute k and kt matrices                         
  for m=1:2                                       
    for n=1:2                                     
      for g=1:n_gauss_points                       
      k(m,n)=k(m,n) +elprops(i,2) ...                 
             *Nxi(m,g)*Nxi(n,g)*gauss_w(g)/jacobian(g); 
      kt(m,n)=kt(m,n)+ hlA*N(m,g)*N(n,g)*gauss_w(g)*jacobian(g);
     end
    end
  end
  [K,F]=AssembleGlobalLinear1D(K, F, i, elements, k+kt, f);
end
% Apply loads and boundary conditions(step 8)
for i=1:n_nodes-1:n_nodes                               
  if bcs(i,2)==1     
    penalty= abs(K(i,i)+1)*1e7;
    K(i,i)=penalty;                            
    F(i)=bcs(i,3)*penalty;
  end
  if bcs(i,2)==2                                    
    F(i)=F(i)+bcs(i,3);                            
  end
end
T_nodes=K\F   % solve the global system equations (step 9)  
% Post processing (step 10)
figure(1);                                             
plot(nodes(:,1), T_nodes,'b-o','linewidth',2); % plot temperature     
set(gca,'fontsize',16);
xlabel('node number','fontsize',18);
ylabel('Temperature (^oC)','fontsize',18);

k=1; % set up index of the points for computing heat flux 
for i=1:n_elements     % loop over the elements     
  node1=elements(i,2); % global node number of the local node 1
  node2=elements(i,3); % global node number of the local node 2                         
  for x=-1:0.1:1
    [N,Nxi]=CompShapeLinear1D(zeros(2,1),zeros(2,1),x);    
    J=CompJacobian1D([nodes(node1,2) nodes(node2,2)]',Nxi); %Jacobian
    Tx=T_nodes(node1)*Nxi(1)/J+T_nodes(node2)*Nxi(2)/J; % dT(x)/dx
    re(k,1)=nodes(node1,2)*N(1)+nodes(node2,2)*N(2);                                                      
    re(k,2)=-elprops(i,2)*Tx;                               
    k=k+1;                                                
   end
end
% plot heat flux
figure(2);                   
plot(re(:,1),re(:,2),'r-','linewidth',2);      
set(gca,'fontsize',16);
xlabel('x (cm)','fontsize',18);
ylabel('Heat flux{(cal/s\cdotcm}^2)','fontsize',18);