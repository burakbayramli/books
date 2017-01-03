% PURPOSE: An example of using sdm_g() Gibbs sampling
%          to compare various weight matrix specifications
%          (on a small data set)                  
%---------------------------------------------------
% USAGE: model_compare2
%---------------------------------------------------

clear all;

% W-matrix from Anselin's neigbhorhood crime data set
load anselin.dat; 
latt = anselin(:,4); % latitude, longitude coordinates 
long = anselin(:,5); % are all we use here

% create W-matrix based on nearest 3 neighbors
W3 = make_neighborsw(latt,long,3);


% generate an sdm model based on 4 nearest neighbors
n = length(latt);
IN = eye(n); 
rho = 0.7;  % true value of rho
sige = 0.5;
k = 3;
x = randn(n,k);
x(:,1) = ones(n,1);
beta(1,1) = -1.0;
beta(2,1) = 0.0;
beta(3,1) = 1.0;

% for sdm model
Wx = W3*x(:,2:end);
xsdm = [x Wx];
beta = [beta
        beta(2:end,1)];

vnames = strvcat('y','constant','x1','x2');
    
% sdm model generated here
% based on nearest 4-neighbors W-matrix, (W4 from above)

y = (IN-rho*W3)\(xsdm*beta) + (IN-rho*W3)\(randn(n,1)*sqrt(sige)); 


% estimate 5 models using W1 to W5 as weight matrices

% run 5 heteroscedastic models
prior.rval = 4;     % heteroscedastic prior
ndraw = 1200;
nomit = 200;

W1 = make_neighborsw(latt,long,1); % create W-matrix based on nearest 1 neighbor
results1 = sdm_g(y,x,W1,ndraw,nomit,prior);
% prt(results1,vnames);            
W2 = make_neighborsw(latt,long,2); % create W-matrix based on nearest 2 neighbors
results2 = sdm_g(y,x,W2,ndraw,nomit,prior);
% prt(results2,vnames);
W3 = make_neighborsw(latt,long,3); % create W-matrix based on nearest 3 neighbors
results3 = sdm_g(y,x,W3,ndraw,nomit,prior);
% prt(results3,vnames);
W4 = make_neighborsw(latt,long,4); % create W-matrix based on nearest 3 neighbors
results4 = sdm_g(y,x,W4,ndraw,nomit,prior);
% prt(results4,vnames);
W5 = make_neighborsw(latt,long,5); % create W-matrix based on nearest 5 neighbors
results5 = sdm_g(y,x,W5,ndraw,nomit,prior);
% prt(results5,vnames);

% compare 5 heteroscedastic models based on 5 different weight matrices
fprintf(1,'posterior probabilities for 5 heteroscedastic models \n');
fprintf(1,'based on W-matrices for neighbors 1 to 5 \n');
probs = model_probs(results1, results2, results3, results4, results5);

rnames = strvcat('Hetero Models');
for j=1:5
    rnames = strvcat(rnames,['neighbors ',num2str(j)]);
end;
in.rnames = rnames;
in.cnames = strvcat('Model Probabilities');
mprint(probs,in);


% Now run 5 homoscedastic models
prior2.rval = 100;      % homoscedastic prior 
W1 = make_neighborsw(latt,long,1); % create W-matrix based on nearest 1 neighbor
results6 = sdm_g(y,x,W1,ndraw,nomit,prior2);
% prt(results6,vnames);            
W2 = make_neighborsw(latt,long,2); % create W-matrix based on nearest 2 neighbors
results7 = sdm_g(y,x,W2,ndraw,nomit,prior2);
% prt(results7,vnames);
W3 = make_neighborsw(latt,long,3); % create W-matrix based on nearest 3 neighbors
results8 = sdm_g(y,x,W3,ndraw,nomit,prior2);
% prt(results8,vnames);
W4 = make_neighborsw(latt,long,4); % create W-matrix based on nearest 3 neighbors
results9 = sdm_g(y,x,W4,ndraw,nomit,prior2);
% prt(results9,vnames);
W5 = make_neighborsw(latt,long,5); % create W-matrix based on nearest 5 neighbors
results10 = sdm_g(y,x,W5,ndraw,nomit,prior2);
% prt(results10,vnames);

% compare 5 homoscedastic models based on 5 different weight matrices
fprintf(1,'posterior probabilities for 5 homoscedastic models \n');
fprintf(1,'based on W-matrices for neighbors 1 to 5 \n');
probs2 = model_probs(results6, results7, results8, results9, results10);

rnames = strvcat('Homo Models');
for j=1:5
    rnames = strvcat(rnames,['neighbors ',num2str(j)]);
end;
in.rnames = rnames;
in.cnames = strvcat('Model Probabilities');
mprint(probs2,in);


