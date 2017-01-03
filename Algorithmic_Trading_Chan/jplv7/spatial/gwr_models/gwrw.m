function wt = gwrw(xc,yc,band,type)
% PURPOSE: forms GWR weight matrix
% ----------------------------------------
% Usage: wmat = gwrw(xc,yc,band,type)
% where: xc = longitude coordinate
%        yc = latitude coordinate
%      band = bandwidth parameter 
%             (or q nearest neighbors for tricube)
%      type = 'tricube','exponential','gaussian'
% ----------------------------------------
% RETURNS: wmat = nobs x nobs weight matrix
% for all observations. Each column is associated
% with an observation
% ----------------------------------------

if nargin ~= 4
error('gwrw: wrong # of input arguments');
end;

if strcmp(type,'tricube');
dtype = 2;
q = band;
elseif strcmp(type,'gaussian');
dtype = 0;
bwidth = band*band;
elseif strcmp(type,'exponential');
dtype = 1;
bwidth = band*band;
else
error('gwrw: unrecognized type');
end;

nobs = length(xc);

% generate big distance matrix for all observations
dmat = zeros(nobs,nobs);
    for j=1:nobs;
        % generate d using GWR distances
        easti = xc(j,1);
        northi = yc(j,1);
        dx = xc - easti;
        dy = yc - northi;
        d = dx.*dx + dy.*dy;    
        dmat(:,j) = d;  
    end;

% generate distance decay matrix
wt = zeros(nobs,nobs);  
if dtype == 1,     % exponential weights
        wt = exp(-dmat/bwidth); 
elseif dtype == 0, % gaussian weights  
        sd = std(sqrt(dmat));
        tmp = matdiv(sqrt(dmat),sd*bwidth);
        wt = stdn_pdf(tmp);
elseif dtype == 2  
% case of tricube weights handled a bit differently
   % sort distance to find q nearest neighbors
 ds = sort(dmat); dmax = ds(q+1,:);
        for j=1:nobs;
 nzip = find(dmat(:,j) <= dmax(1,j));
        wt(nzip,j) = (1-(dmat(nzip,j)/dmax(1,j)).^3).^3;
        end; % end of j-loop
end;  % end of if-else    
