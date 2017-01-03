% PURPOSE: An example of using sem_c() function
%          to compare various weight matrix specifications
%          (on a small data set)                  
%---------------------------------------------------
% USAGE: compare_weights
%---------------------------------------------------

clear all;

% W-matrix from Anselin's neigbhorhood crime data set
load anselin.dat; 
latt = anselin(:,4); % latitude, longitude coordinates 
long = anselin(:,5); % are all we use here

% create W-matrix based on nearest 3 neighbors
W3 = make_neighborsw(latt,long,3);


% generate an sem model based on 3 nearest neighbors
n = length(latt);
IN = eye(n); 
rho = 0.7;  % true value of rho
sige = 1;
k = 3;
x = randn(n,k);
beta(1,1) = -0.5;
beta(2,1) = 0.5;
beta(3,1) = 0.5;

vnames = strvcat('y','constant','x1','x2');
    
% sem model generated here
% based on nearest 3-neighbors W-matrix, (W3 from above)

y = x*beta + (IN-rho*W3)\(randn(n,1)*sqrt(sige)); 


% produce log-marginal likelihood for 5 models using W1 to W5 as weight
% matrices

W1 = make_neighborsw(latt,long,1); % create W-matrix based on nearest 1 neighbor
results1 = sem_c(y,x,W1);
W2 = make_neighborsw(latt,long,2); % create W-matrix based on nearest 2 neighbors
results2 = sem_c(y,x,W2);
W3 = make_neighborsw(latt,long,3); % create W-matrix based on nearest 3 neighbors
results3 = sem_c(y,x,W3);
W4 = make_neighborsw(latt,long,4); % create W-matrix based on nearest 4 neighbors
results4 = sem_c(y,x,W4);
W5 = make_neighborsw(latt,long,5); % create W-matrix based on nearest 5 neighbors
results5 = sem_c(y,x,W5);

% compare 5 homoscedastic models based on 5 different weight matrices
fprintf(1,'posterior probabilities for 5 models \n');
fprintf(1,'based on W-matrices for neighbors 1 to 5 \n');
fprintf(1,'true model based on 3 neighbors \n');

probs = model_probs(results1, results2, results3, results4, results5);

rnames = strvcat('Models');
for j=1:5
    rnames = strvcat(rnames,['neighbors ',num2str(j)]);
end;
in.rnames = rnames;
in.cnames = strvcat('Model Probabilities');
mprint(probs,in);

