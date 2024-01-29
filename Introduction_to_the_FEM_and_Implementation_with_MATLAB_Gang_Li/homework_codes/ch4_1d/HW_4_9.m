clear all;

n_elements=2;  % user defined parameter: number of elements to be used

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
bcs=zeros(n_nodes,3);
bcs(:,1)=nodes(:,1);
bcs(1,2)=2;
bcs(1,3)=1.0;
bcs(n_nodes,2)=1;
bcs(n_nodes,3)=0;

K=zeros(n_nodes,n_nodes);     % set up empty K matrix
F=zeros(n_nodes,1);           % set up empty RHS F vector
n_gauss_points=2;             % number of Gauss points
[gauss_xi, gauss_w] = Get1DGauss(n_gauss_points); % Get Gauss points
N=zeros(2,n_gauss_points);    % set up shape function matrix    
Nxi=zeros(2,n_gauss_points);  % set up dN/dxi matrix 
[N,Nxi]=CompShapeLinear1D(N, Nxi, gauss_xi); % compute N, dN/dxi
                                              % at Gauss points   
% next block: compute element matrices and vectors and assemble the global system              
for i=1:n_elements  % loop over elements                                
  start_x= nodes(elements(i,2),2);                      
  end_x=  nodes(elements(i,3),2); 
  jacobian=CompJacobian1D([start_x end_x]', Nxi); 
  for g=1:n_gauss_points                                   
    sv(g)=2*(start_x*N(1,g)+ end_x*N(2,g));  % the 2x term
  end                                                  
  f=zeros(2,1);  % RHS vector
  for m=1:2
  	for g=1:n_gauss_points    
  		f(m,1)=f(m,1)+sv(g)*N(m,g)*gauss_w(g)*jacobian(g); 
  	end
  end
  k=zeros(2,2);   % element matrix                                      
  for m=1:2                                       
    for n=1:2                                     
      for g=1:n_gauss_points                       
        k(m,n)=k(m,n) + Nxi(m,g)*Nxi(n,g)*gauss_w(g)/jacobian(g);
     end
    end
  end
  [K,F]=AssembleGlobalLinear1D(K, F, i, elements, k, f);
end

% next block: apply loads, boundary conditions
for i=1:n_nodes-1:n_nodes                               
  if bcs(i,2)==1     
    penalty= abs(K(i,i))*1e7;
    K(i,i)=penalty;                            
    F(i)=bcs(i,3)*penalty;
  end
  if bcs(i,2)==2                                    
    F(i)=F(i)-bcs(i,3);                            
  end
end
u=K\F      % solve the global system equations

% next blocks: post processing 
x1=[left_end:length/100:length];    % analytically obtain the exact  
exact_u=-1/3 * x1.^3 + x1 - 2/3;    % solutions for comparison
exact_ux=-x1.^2 + 1;

figure(1);                                             
plot(nodes(:,2), u,'b-o',x1,exact_u,'k-','linewidth',2);     
set(gca,'fontsize',16);
xlabel('x','fontsize',18);
ylabel('u','fontsize',18);
legend('FE solution', 'Exact solution','location','northwest');
k=1; % set up index of the points 
for i=1:n_elements     % loop over the elements     
  node1=elements(i,2); % global node number of the local node 1
  node2=elements(i,3); % global node number of the local node 2                         
  for x=-1:1:1
    [N,Nxi]=CompShapeLinear1D(zeros(2,1),zeros(2,1),x);
    J=CompJacobian1D([nodes(node1,2) nodes(node2,2)]',Nxi); %Jacobian
    re(k,2)=u(node1)*Nxi(1)/J+u(node2)*Nxi(2)/J;            % du(X)/dx
    re(k,1)=nodes(node1,2)*N(1)+nodes(node2,2)*N(2);  
    k=k+1;                                                
   end
end

figure(2);              
plot(re(:,1),re(:,2),'r-',x1,exact_ux,'-','linewidth',2);     
set(gca,'fontsize',16);
xlabel('x','fontsize',18);
ylabel('du/dx','fontsize',18);
legend('FE solution', 'Exact solution','location','southwest');