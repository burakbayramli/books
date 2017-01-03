function probs = model_probs(varargin)
% PURPOSE: computes and prints posterior model probabilities using log-marginals
% ---------------------------------------------------
%  USAGE: probs = model_probs(results1,results2,results3, ...)
%  where: results_matrix is a  matrix of results structures returned by estimation
%         functions, sar_g, sdm_g, sac_g, sem_g
% e.g. result1 = sar_g(y,x,W,ndraw,nomit,prior);
%      result2 = sem_g(y,x,W,ndraw,nomit,prior);
% results_matrix = [result1 result2];
% model_probs(results_matrix);
% ---------------------------------------------------
%  RETURNS: probs = a vector of posterior model probabilities
% ---------------------------------------------------

% written by:
% James P. LeSage, 7/2003
% Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

nmodels = length(varargin);

lmarginal = [];
for i=1:nmodels
results = varargin{i};
fields = fieldnames(results);
nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'mlike')
        lmarginal = [lmarginal results.mlike];
    end;
 end;
end; % end of for loop
detval = results.lndet;
 
nrho = length(detval);

% now scale using all of the vectors of log-marginals
% we must scale before exponentiating 
adj = max(max(lmarginal));
madj = lmarginal - adj;

xx = exp(madj);

% trapezoid rule integration
yy = matmul(detval(:,1),ones(nrho,nmodels));
isum = zeros(1,nmodels);
isum = sum((yy(2:nrho,:) + yy(1:nrho-1,:)).*(xx(2:nrho,:) - xx(1:nrho-1,:))/2);

% compute posterior probabilities
psum = sum(isum);
probs = isum/psum;
probs = probs';
