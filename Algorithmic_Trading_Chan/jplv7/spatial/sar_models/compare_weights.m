% PURPOSE: An example of model comparison using sar_c() function to compare various weight matrix specifications
%          on a homoscedastic sar model   
% (see compare_weights2 for non-homoscedastic models)               
%---------------------------------------------------
% USAGE: compare_weights
%---------------------------------------------------

clear all;

% W-matrix from Anselin's neigbhorhood crime data set
load anselin.dat; 
latt = anselin(:,4); % latitude, longitude coordinates 
long = anselin(:,5); % are all we use here

% create W-matrix based on nearest 3 neighbors
W3 = make_nnw(latt,long,3);

% generate an sar model based on 4 nearest neighbors
n = length(latt);
IN = eye(n); 
rho = 0.7;  % true value of rho
sige = 0.2;
k = 3;
x = randn(n,k);
x(:,1) = ones(n,1);
beta(1,1) = -1.0;
beta(2,1) = 0.0;
beta(3,1) = 1.0;

vnames = strvcat('y','constant','x1','x2');
    
% sar model generated here
% based on nearest 3-neighbors W-matrix, (W3 from above)

y = (IN-rho*W3)\(x*beta) + (IN-rho*W3)\(randn(n,1)*sqrt(sige)); 


% compute log-marginal posteriors for 5
% homoscedastic models using W1 to W5 as weight matrices

% run 5 homoscedastic models
prior.g = 1/n;

W1 = make_nnw(latt,long,1); % create W-matrix based on nearest 1 neighbor
results1 = sar_c(y,x,W1,prior);
% prt(results1,vnames);  NOTE: we cannot use prt here since there are no estimates to print           
W2 = make_nnw(latt,long,2); % create W-matrix based on nearest 2 neighbors
results2 = sar_c(y,x,W2,prior);
% prt(results2,vnames);
W3 = make_nnw(latt,long,3); % create W-matrix based on nearest 3 neighbors
results3 = sar_c(y,x,W3,prior);
% prt(results3,vnames);
W4 = make_nnw(latt,long,4); % create W-matrix based on nearest 4 neighbors
results4 = sar_c(y,x,W4,prior);
% prt(results4,vnames);
W5 = make_nnw(latt,long,5); % create W-matrix based on nearest 5 neighbors
results5 = sar_c(y,x,W5,prior);
% prt(results5,vnames);

% compare 5 homoscedastic models based on 5 different weight matrices
fprintf(1,'posterior probabilities for 5 homooscedastic models \n');
fprintf(1,'based on W-matrices for neighbors 1 to 5 \n');
probs = model_probs(results1, results2, results3, results4, results5);

rnames = strvcat('Homoscedastic Models');
for j=1:5
    rnames = strvcat(rnames,['neighbors ',num2str(j)]);
end;
in.rnames = rnames;
in.cnames = strvcat('Model Probabilities');
fprintf(1,'True model is based on 3 neighbors \n');
mprint(probs,in);

