rand('seed',1); 
% state space dimension
n = 100; 

% incidence matrix
L = zeros(n,n); 
num_links = [zeros(20,1); randi([1,3],75,1); randi([10 15],5,1)];
for i = 1 : n
    links = randi([1,n],num_links(i),1); 
    L(links,i) = 1; L(i,i) = 0; 
end
L(:,randi(n)) = 0; 
L(:,randi(n)) = 0; 

% payment matrix
R = rand(n,n).*L; 

% probability that surfer follows a link from the current page
theta = 1-1e-2; 