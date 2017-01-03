% PURPOSE: An example of model comparison using far_g() function
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

% generate far model based on 3 nearest neighbors
n = length(latt);
IN = eye(n); 
rho = 0.7;  % true value of rho
sige = 0.1;
k = 3;

vnames = strvcat('y','Wy');
    
% far model generated here
% based on nearest 3-neighbors W-matrix, (W3 from above)

y = (IN-rho*W3)\(randn(n,1)*sqrt(sige)); 


% estimate 5 models using W1 to W5 as weight matrices

% run 5 homoscedastic models
prior.novi = 1;     % homoscedastic prior
ndraw = 1200;
nomit = 200;

W1 = make_neighborsw(latt,long,1); % create W-matrix based on nearest 1 neighbor
results1 = far_g(y,W1,ndraw,nomit,prior);
W2 = make_neighborsw(latt,long,2); % create W-matrix based on nearest 2 neighbors
results2 = far_g(y,W2,ndraw,nomit,prior);
W3 = make_neighborsw(latt,long,3); % create W-matrix based on nearest 3 neighbors
results3 = far_g(y,W3,ndraw,nomit,prior);
W4 = make_neighborsw(latt,long,4); % create W-matrix based on nearest 4 neighbors
results4 = far_g(y,W4,ndraw,nomit,prior);
W5 = make_neighborsw(latt,long,5); % create W-matrix based on nearest 5 neighbors
results5 = far_g(y,W5,ndraw,nomit,prior);

% compare 5 homoscedastic models based on 5 different weight matrices
fprintf(1,'posterior probabilities for 5 models \n');
fprintf(1,'based on W-matrices for neighbors 1 to 5 \n');
probs = model_probs(results1, results2, results3, results4, results5);

rnames = strvcat('Models');
for j=1:5
    rnames = strvcat(rnames,['neighbors ',num2str(j)]);
end;
in.rnames = rnames;
in.cnames = strvcat('Model Probabilities');
fprintf(1,'True model is based on 3 neighbors \n');
mprint(probs,in);

