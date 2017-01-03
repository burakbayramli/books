%this m-file tries out the nonparametric regression 
%using variable selection as in Smith and Kohn;

%begin by generating some data. 
clear;
clc;



randn('seed',sum(100*clock));
rand('seed',sum(100*clock));

nobs = 1500;
x = -2 + 4*rand(nobs,1);
mx = 2 + .1*x - .15*(x.^2) + sin(x);
sigsq = .15;
y = mx + sqrt(sigsq)*randn(nobs,1);


%---------------------------------------------------------
%try and approximate this function using a regression spline
%---------------------------------------------------------
K = 9;      %number of knot terms in truncated series expansion
J = K+3;    %total number of coefficients in cubic spline (intercept + linear + quadratic).
knots = linspace(-1.75,1.75,K); %space them out evenly over the interior support

%set up a grid to estimate the function over. 
gridlen = 100;
xgrid = linspace(-1.95,1.95,gridlen)';
zero_vec = zeros(gridlen,1);
tempp  = zeros(gridlen,K);

%construct the bigx grid (for fitting the function) containing the intercept, linear, quadratic and
%cubic spline terms
for k = 1:K;
    tempp(:,k) = (max([zero_vec ((xgrid-knots(k)).^3) ]')');
end;
big_xgrid = [ones(gridlen,1)  (xgrid) (xgrid.^2) tempp];

%---------------------------
%Prior Hyperparameters
%---------------------------
c = nobs;           %c is like g inverse in the g-prior. 
%improper prior for error variance parameter

%------------------------
%set initial conditions
%------------------------
iter = 100;
sig_eps = .1;
fit_function = zeros(iter,gridlen);
select_all = all_combination(J); %this invokes an m-file that 
                                %creates all possible combinations of zeros
                                %and ones to enumerate each possible
                                %combination of including / excluding each
                                %variable. 
%---------------------------
%construct the big x matrix.
%--------------------------
zero_vec = zeros(nobs,1);
tempp  = zeros(nobs,K);
for k = 1:K;
    tempp(:,k) = max([zero_vec ((x-knots(k)).^3)]')';
end;
big_x = [ones(nobs,1)  x (x.^2) tempp];

temp_grid = linspace(1,length(select_all)-1,length(select_all)-1); %for later use when drawing component label vector. 
%------------------------------------
%START THE GIBBS SAMPLER
%-----------------------------------
sigeps_final = zeros(iter,1);
for i = 2:iter;
%-----------------------------
%Compute posterior ordinates for "Acceptance" Probabilities
%-----------------------------
ord = zeros(length(select_all)-1,1);
for j = 1:length(select_all)-1; %do not include the last selector vector, which is all zeros.
    tempp = find(select_all(j,:)==1);
    x_use = zeros(nobs,length(tempp));
    x_use = big_x(:,tempp);
    [nn,q_alpha] = size(x_use);
    term1 = (-q_alpha/2)*log(1+c); 
    temp_term = y'*x_use;
    term2 = y'*y  - (c/(1+c))*temp_term*inv(x_use'*x_use)*temp_term';
    term2a = (-nobs/2)*log(term2); 
    ord(j,1) = term1 + term2a;    %   TYPICALLY, WILL HAVE TO ADD PRIOR HERE.
end;

%-----------------------------------
%exponentiate log ordinates, and for computational reasons,
%set very small probabilities equal to zero.
%-----------------------------------
norm_const = max(ord); 
ord2 = max([(-250*ones(length(ord),1)) (ord - norm_const)]')';
ord = exp(ord2)/sum(exp(ord2));

temp1 = discrete(temp_grid',ord); %discrete is an m-file that draws from a discrete distribution 
                                    %given the probabilites of each
                                    %discrete value
vector_taken = select_all(temp1,:);

%Now, calculate the design matrix at the given vector. 
points_temp = find(vector_taken==1);
x_current = big_x(:,points_temp);

%-----------------------------------
%sample from the sigeps conditional. 
%----------------------------------- 
temp_term = y'*x_current;
term2 = y'*y  - (c/(1+c))*temp_term*inv(x_current'*x_current)*temp_term';
sig_eps = invgamrnd( (nobs/2), 2*inv(term2),1,1)
%----------------------------------
%Sample from the conditional for the parameters of the nonparametric part
%---------------------------------
D_delta = (inv(x_current'*x_current))*(sig_eps)*(c/(1+c));
d_delta = x_current'*(y)/sig_eps;
H = chol(D_delta);
parms = D_delta*d_delta + H'*randn(size(x_current,2),1);

sigeps_final(i,1) = sig_eps;
fit_function(i,:) = (big_xgrid(:,points_temp)*parms)';
end;

end;
true_fun = 2 + .1*xgrid - .15*(xgrid.^2) + sin(xgrid);
%true_fun =  .3*exp(-4*( (xgrid+1).^2)) + .7*exp(-16*( (xgrid-1).^2));
%true_fun = 3 + 2*xgrid;
plot(xgrid,mean(fit_function));
hold on;
plot(xgrid,true_fun,'--');
hold off;

