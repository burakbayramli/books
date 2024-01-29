clear all;
% next line: user defined parameter
n_elements=20;  

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

% next block: set up boundary conditions 
bcsdisp=[1 0];

% next block: material properties
E=70e9;
rho=7800;

K=zeros(n_nodes,n_nodes);     % set up empty K matrix
M=zeros(n_nodes,n_nodes);     % set up empty M matrix
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
        ke(m,n)=ke(m,n) + E*Nxi(m,g)*Nxi(n,g)*gauss_w(g)/jacobian(g);
        me(m,n)=me(m,n)+rho*N(m,g)*N(n,g)*gauss_w(g)*jacobian(g);
     end
    end
  end
  [K]= AssembleGlobalMatrix(K,ke,node_id_map,1);
  [M]= AssembleGlobalMatrix(M,me,node_id_map,1);
end

% next block: delete the rows and columns of K and M that are 
% associated with zero displacements 
drows=1;
K(drows,:)=[];
K(:,drows)=[];
M(drows,:)=[];
M(:,drows)=[];

% next block: solve the eigenvalue problem 
[V D]=eigs(K,M,5,'smallestabs');
D=sqrt(D)
V=[zeros(1,5);V]
