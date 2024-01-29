clear all;

n_elements=10;  % user defined parameter: number of elements to be used

% next block: define the geometry, elements, and nodes 
length=100;                     %the length of the domain
left_end=0;
length_element=length/n_elements;
n_nodes=2*n_elements+1;
elements(:,1)=[1:n_elements];
elements(:,2)=[1:2:n_nodes-2];
elements(:,3)=[2:2:n_nodes-1];
elements(:,4)=[3:2:n_nodes];
nodes(:,1)=[1:n_nodes];
nodes(:,2)=[left_end:length_element/2:length];

% next block: set up boundary conditions 
bcs=zeros(n_nodes,3);
bcs(:,1)=nodes(:,1);
bcs(1,2)=2;
bcs(1,3)=0.1;
bcs(n_nodes,2)=1;
bcs(n_nodes,3)=0;

% next block: thermal conductivity
kappa(:,1)=elements(:,1);
kappa(1:round(n_elements/2),2)=0.92;
kappa(round(n_elements/2)+1:n_elements,2)=0.12;
if mod(n_elements,2) == 1     % if an element is covering two materials
  kappa(round(n_elements/2),2)=(0.92+0.12)/2; % take the average 
end

radius=0.5;                   % define r
h=3.7500e-05;                 % define h
Tinf=20;                      % define T_inf
hlA=2*h/radius;               % define 2h/r
hlAT_inf=hlA*Tinf;            % define 2hT_inf/r
n_elements=size(elements,1);  % number of elements
n_nodes=size(nodes,1);        % number of nodes
K=zeros(n_nodes,n_nodes);         % Set up empty K matrix
F=zeros(n_nodes,1);               % Set up empty RHS F vector
n_gauss_points=3;                 % number of Gauss points
[gauss_xi, gauss_w] = Get1DGauss(n_gauss_points); % Get Gauss points

% next block: N, Nxi: set up empty matrices storing the value of the shape 
% functions N and their derivatives Nxi (dN/dxi) evaluated at the 
% Guass points in the master element
N=zeros(3,n_gauss_points);  
Nxi=zeros(3,n_gauss_points);     

% compute the shape functions and their derivatives at Gauss points  
[N,Nxi]=CompShapeQuadratic1D(N, Nxi, gauss_xi);  

% for-loop: compute element matrices and vectors and aseemble them 
% into the global matrix and RHS vector  
for i=1:n_elements  %loop over elements                                    
  start_x= nodes(elements(i,2),2);   % x-coordinate of the start node
  mid_x=  nodes(elements(i,3),2);    % x-coordinate of the middle node                
  end_x=  nodes(elements(i,4),2);    % x-coordinate of the end node        
  jacobian=CompJacobian1D([start_x mid_x end_x]', Nxi);   
  f=hlAT_inf * N*(gauss_w.*jacobian);
  k=zeros(3,3); kt=zeros(3,3);       % Set up empty element matrices 
  % for-loop: compute k and kt matrices                         
  for m=1:3                                       
    for n=1:3                                     
      for g=1:n_gauss_points                       
      k(m,n)=k(m,n) + kappa(i,2) ...                 
             *Nxi(m,g)*Nxi(n,g)*gauss_w(g)/jacobian(g); 
      kt(m,n)=kt(m,n)+ hlA*N(m,g)*N(n,g)*gauss_w(g)*jacobian(g);
     end
    end
  end
  [K,F]=AssembleGlobalQuadratic1D(K, F, i, elements, k+kt, f);
end

% next block: apply loads and boundary conditions(step 8)
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

% next: post processing 
figure(1);                                             
plot(nodes(:,1), T_nodes,'b-o','linewidth',2); % plot temperature     
set(gca,'fontsize',16);
xlabel('node number','fontsize',18);
ylabel('Temperature (^oC)','fontsize',18);

k=1; % set up index of the points for computing heat flux 
for i=1:n_elements     % loop over the elements     
  node1=elements(i,2); % global node number of the local node 1
  node2=elements(i,3); % global node number of the local node 2 
  node3=elements(i,4); % global node number of the local node 3                      
  for x=-1:0.1:1
    [N,Nxi]=CompShapeQuadratic1D(zeros(3,1),zeros(3,1),x);    
    J=CompJacobian1D([nodes(node1,2) nodes(node2,2) nodes(node3,2)]',Nxi); %Jacobian
    Tx=T_nodes(node1)*Nxi(1)/J+ T_nodes(node2)*Nxi(2)/J+ ...
       T_nodes(node3)*Nxi(3)/J; % dT(x)/dx
    re(k,1)=nodes(node1,2)*N(1)+nodes(node2,2)*N(2)+nodes(node3,2)*N(3);                                                      
    re(k,2)=-kappa(i,2)*Tx;                               
    k=k+1;                                                
   end
end

% next block: plot heat flux
figure(2);                   
plot(re(:,1),re(:,2),'r-','linewidth',2);      
set(gca,'fontsize',16);
xlabel('x (cm)','fontsize',18);
ylabel('Heat flux{(cal/s\cdotcm}^2)','fontsize',18);