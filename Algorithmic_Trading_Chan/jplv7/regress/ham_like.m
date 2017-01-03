function likv = ham_like(parm1,y,x,nlag,st_mat);
% PURPOSE: log likelihood function for Hamilton's model
% y(t) - mu_(st) = phi1*(y(t-1) - mu_(st-1) + ...
%                  + phi_nlag*(y(t-nlag) + mu_(st-nlag)) + e(t)
% mu_(st) = mu0*(1-st) + mu1*(st)
% e(t) = N(0,sigma^2)
% pr(st=1 | st-1 = 1) = p
% pr(st=0 | st-1 = 0) = q
% -----------------------------------------------------
% Usage: like = ham_like(parm,y,x,nlag,st_mat);
% where:   y = nx1 vector of dependent variable
%          x = nxk matrix of lagged dependent variables
%       nlag = nxk matrix of explanatory variables
%      st_mat = an input matrix computed once 
%               in hamilton() function to save time
% -----------------------------------------------------
 

n = length(y);
nstates = nlag+1;
dimen = 2.^nstates;

parm = ham_trans(parm1); % transform input parameters

ppr = parm(1,1); % pr(st=1 | st-1 = 1)
qpr = parm(2,1); % pr(st=0 | st-1 = 0)

phi = parm(3:3+nlag-1,1);
sig = parm(3+nlag,1)*parm(3+nlag,1); 

mu0 = parm(3+nlag+1,1); % recession vs. boom
mu1 = parm(3+nlag+2,1); 

mu_mat = st_mat*mu1 + (ones(dimen,nstates) - st_mat)*mu0;

pr_tr = [qpr (1-ppr) 
         (1-qpr) ppr];

      
a = [eye(2) - pr_tr
     ones(1,2)]; 
                                
en = [0
      0
      1];

probt = inv(a'*a)*a'*en;
% pr(st=0) | pr(st=1) a 2x1 steady state probabilties vector

pr_trf0 = vec(pr_tr);
pr_trf1 = [pr_trf0
           pr_trf0];                    
pr_trf2 = [pr_trf1
           pr_trf1];
pr_trf = [pr_trf2
          pr_trf2];


probt = vecr([probt probt]).*pr_trf0; % PR[S_{-2},S_{-3}|I_0)    4x1
probt = vecr([probt probt]).*pr_trf1; % PR[S_{-1},S_{-2},S_{-3}|I_0)  8x1
probt = vecr([probt probt]).*pr_trf2; % PR[S_{0},S_{-1},S_{-2},S_{-3}|I_0) 16x1
prob = vecr([probt probt]); %should be 2^5 x 1 vector 


likv = 0;
iter = 1;

while iter <= dimen
lik = 0;

size(x)
size(phi)
dimen
size(mu_mat)
n
size(y)
fcast = (y(iter,1) - x(iter,:)*phi')*ones(dimen,1) ...
         - (mu_mat(:,5) - mu_mat(:,4)*phi(1,1) ...
                        - mu_mat(:,3)*phi(2,1) ...
                        - mu_mat(:,2)*phi(3,1) ...
                        - mu_mat(:,1)*phi(4,1));
fcast2 = fcast.*fcast;

var_l = sig*ones(dimen,1);

prob_dd = pr_trf.*prob;
%  Pr[S_t,S_{t-1},S_{t-2},S_{t-3},S_{t-4} | I(t-1)]

term1 = ones(dimen,1)./sqrt(2*pi*var_l);
term2 = exp(-0.5*fcast2./var_l);

pr_vl = term1.*term2.*prob_dd;


%pr_vl = (ones(dimen,1)./sqrt(2*pi*var_l)).*exp(-0.5*(fcast.*fcast)./var_l).*prob_dd;
% should be 2^5 x 1 vector
% with joint density of y_t,S_t,..,S_{t-4} given past information



pr_val = sum(pr_vl); % f(y_t|I(t-1)), density of y_t given past
                     % information:  This is weighted average of
                     %  2^5 conditional densities  
if pr_val ~= 0                     
lik = -log(pr_val);
pro = pr_vl/pr_val;
else % we do a fix-up in this case 
pr_val = 0.1; 
pr_vl = pr_vl + 0.001;
% warning('problem in likelihood function');
lik = -log(pr_val);
pro = pr_vl/pr_val;
end;

% Pr[S_t,S_{t-1},S_{t-2},S_{t-3},S_{t-4} | I(t-1),y_t]
% Updating of prob. with new information, y_t  

probt = pro(1:dimen/2,1) + pro(dimen/2+1:dimen,1);
% Integrate out S_{t-4}: then you get
% Pr[S_t, S_{t-1},S_{t-2},S_{t-3}| Y_t]  
% 2^4x1 vector

prob = vecr([probt probt]);
% should be 2^5 x 1 vector

likv = likv + lik;

iter = iter + 1;

end;


