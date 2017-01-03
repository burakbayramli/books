clear all;

A=wk1read('cigardemo.wk1',1,0);
W1=wk1read('spat-sym-us.wk1');
% Dataset downloaded from www.wiley.co.uk/baltagi/
% Spatial weights matrix constructed by Elhorst
%
% written by: J.Paul Elhorst summer 2008
% University of Groningen
% Department of Economics
% 9700AV Groningen
% the Netherlands
% j.p.elhorst@rug.nl
%
% REFERENCE: 
% Elhorst JP (2009) Spatial Panel Data Models. In Fischer MM, Getis A (Eds.) 
% Handbook of Applied Spatial Analysis, Ch. C.2. Springer: Berlin Heidelberg New York.
%
% dimensions of the problem
T=6; % number of time periods
N=46; % number of regions
% row-normalize W
W=normw(W1); % function of LeSage
y=A(:,[3]); % column number in the data matrix that corresponds to the dependent variable
x=A(:,[4,5,6]); % column numbers in the data matrix that correspond to the independent variables
xconstant=ones(N*T,1);
[nobs K]=size(x);
% ----------------------------------------------------------------------------------------
% spatial fixed effects + spatially lagged dependent variable
info.lflag=0; % required for exact results
info.model=1;
info.fe=0; % no print intercept and spatial fixed effects
results=sar_panel_FE(y,x,W,T,info); 
vnames=strvcat('logcit','logp','logpn','logy');
prt(results,vnames,1);
blagfe=[results.beta;results.rho];
covblagfe=results.cov;
% LR-test for joint significance spatial fixed effects
logliklagfe=results.lik;
info.model=0;
results=sar_panel_FE(y,x,W,T,info); 
logliklag=results.lik;
LR=-2*(logliklag-logliklagfe);
dof=N;
probability=1-chis_prb(LR,dof);
% Note: probability > 0.05 implies rejection of spatial fixed effects
fprintf(1,'LR-test joint significance spatial fixed effects, degrees of freedom and probability = %9.4f,%6d,%9.4f \n',LR,dof,probability);
% ----------------------------------------------------------------------------------------
% spatial random effects + spatially lagged dependent variable
clear info.model;
info.model=1;
results=sar_panel_RE(y,[xconstant x],W,T,info); 
vnames=strvcat('logcit','intercept','logp','logpn','logy');
prt(results,vnames,1);
blagre=[results.beta(2:end);results.rho]; % exclude constant
covblagre=results.cov(2:end,2:end); % exclude constant
% LR-test for significance spatial random effects (note logliklag is already available)
logliklagre=results.lik;
LR=-2*(logliklag-logliklagre);
dof=1;
probability=1-chis_prb(LR,dof);
% Note: probability > 0.05 implies rejection of spatial fixed effects
fprintf(1,'LR-test significance spatial random effects, degrees of freedom and probability = %9.4f,%6d,%9.4f \n',LR,dof,probability);
% ----------------------------------------------------------------------------------------
% Hausman test FE versus RE of model + spatially lagged dependent variable
hausman=(blagfe-blagre)'*inv(covblagre-covblagfe)*(blagfe-blagre);
dof=length(blagfe);
probability=1-chis_prb(abs(hausman),dof); 
% Note: probability > 0.05 implies rejection of random effects model in favor of fixed effects model
fprintf(1,'Hausman test-statistic, degrees of freedom and probability = %9.4f,%6d,%9.4f \n',hausman,dof,probability);
% ----------------------------------------------------------------------------------------
% spatial fixed effects + spatial autocorrelation
clear info.model;
info.lflag=0; % required for exact results
info.fe=0; % no print intercept and spatial fixed effects
info.model=1;
results=sem_panel_FE(y,x,W,T,info); 
vnames=strvcat('logcit','logp','logpn','logy');
prt_spnew(results,vnames,1);
berrorfe=[results.beta;results.rho];
covberrorfe=results.cov;
% LR-test for joint significance spatial fixed effects
loglikerrorfe=results.lik;
info.model=0;
results=sar_panel_FE(y,x,W,T,info); 
loglikerror=results.lik;
LR=-2*(loglikerror-loglikerrorfe);
dof=N;
probability=1-chis_prb(LR,dof);
% Note: probability > 0.05 implies rejection of spatial fixed effects
fprintf(1,'LR-test joint significance spatial fixed effects, degrees of freedom and probability = %9.4f,%6d,%9.4f \n',LR,dof,probability);
% ----------------------------------------------------------------------------------------
% spatial random effects + spatial autocorrelation
clear info.model;
results=sem_panel_RE(y,[xconstant x],W,T); 
vnames=strvcat('logcit','intercept','logp','logpn','logy');
prt_spnew(results,vnames,1);
berrorre=[results.beta(2:end);results.rho]; % exclude constant
covberrorre=results.cov(2:end,2:end); % exclude constant
% LR-test for significance spatial random effects (note loglikerror is already available)
loglikerrorre=results.lik;
LR=-2*(loglikerror-loglikerrorre);
dof=1;
probability=1-chis_prb(LR,dof);
% Note: probability > 0.05 implies rejection of spatial fixed effects
fprintf(1,'LR-test significance spatial random effects, degrees of freedom and probability = %9.4f,%6d,%9.4f \n',LR,dof,probability);
% ----------------------------------------------------------------------------------------
% Hausman test FE versus RE of model + spatial autocorrelation
hausman=(berrorfe-berrorre)'*inv(covberrorre-covberrorfe)*(berrorfe-berrorre);
dof=length(berrorfe);
probability=1-chis_prb(abs(hausman),dof);
% Note: probability > 0.05 implies rejection of random effects model in favor of fixed effects model
fprintf(1,'Hausman test-statistic, degrees of freedom and probability = %9.4f,%6d,%9.4f \n',hausman,dof,probability);