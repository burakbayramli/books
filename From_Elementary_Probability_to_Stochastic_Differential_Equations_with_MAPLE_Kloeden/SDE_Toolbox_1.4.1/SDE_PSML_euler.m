function density = SDE_PSML_euler(bigtheta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,NUMDEPVARS,SEED)

% Returns the approximated transition densities for the stochastic model dX(t) =... defined by
% a system of (Ito) SDEs, as described in [1]-[2] using the Euler-Maruyama scheme for X.  
% To be used in the implementation of the Parametric SML algorithm  (see SDE_PSML).
%
% IN:
%     bigtheta; complete structural parameter vector
%      OWNTIME; vector containing the equispaced simulation times sorted in ascending order. 
%               It has starting simulation-time in first and ending simulation-time in last position. 
%               Thus OWNTIME(i) - OWNTIME(i-1) = h, where h is the fixed stepsize 
%               for the numerical integration (i=2,3,...)
%         TIME; the array of unique observation times
%         VRBL; the array of unique label-variables 
%         XOBS; the matrix-shaped observed data
%       NUMSIM; the number of desired simulations for the SDE numerical integration 
%      PROBLEM; the user defined name of the current problem/experiment/example etc. (e.g. 'mySDE')
%      SDETYPE; the SDE definition: must be 'Ito';
%   NUMDEPVARS; the number of dependent variables, i.e. the SDE dimension
%         SEED; the seed for the generation of pseudo-random normal variates, i.e. the argument for randn('state',SEED);
%               type 'help randn' for details;
% OUT: density; the array of the approximated transition densities
%
% References:
% [1] A.R. Pedersen. "A new approach to maximum likelihood estimation for
% stochastic differential equations based on discrete observations".
% Scandinavian Journal of Statistics, 22, 55-71, 1995.
% [2] M.W. Brandt and P. Santa-Clara. "Simulated likelihood estimation of
% diffusions with an application to exchange rate dynamics in incomplete
% markets". Journal of Financial Economics, 63, 161-210, 2002.

% Copyright (C) 2007, Umberto Picchini  
% umberto.picchini@biomatematica.it
% http://www.biomatematica.it/Pages/Picchini.html
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

% input check
error(nargchk(10, 10, nargin));
[n1,n2]=rat(NUMSIM);
if(~isequal(n2,1) || isempty(NUMSIM) || NUMSIM <= 0)
    error('The number of trajectories NUMSIM must be a positive integer');
end
[n1,n2]=rat(NUMDEPVARS);
if(~isequal(n2,1) || isempty(NUMDEPVARS) || NUMDEPVARS <= 0)
    error('The number of variables NUMDEPVARS must be a positive integer');
end
switch upper(SDETYPE)
    case 'ITO'
        ok = 1;
    otherwise
        error('The Euler-Maruyama scheme is defined only for Ito SDE');
end

n = length(unique(TIME));
h = OWNTIME(2) - OWNTIME(1);
if(h > min(diff(TIME(diff(TIME)>0))))
   error('Choose a smaller integration stepsize');
end
nvar = length(unique(VRBL));
xhat_endpoint = zeros(1,NUMSIM*nvar);
density = zeros(n-1,NUMSIM);
time = unique(TIME);


% by controlling the seed, the Wiener increments are kept fixed during a given optimization procedure  
if(isempty(SEED))
    randn('state',0);
else
    randn('state',SEED);
end 


for j=1:n-1 
    
    if(j==1)
        t = time(j);
        [t, xstart] = feval([PROBLEM, '_sdefile'], t, [], 'init', bigtheta,SDETYPE,NUMDEPVARS,NUMSIM);
        discretization = [time(j):h:time(j+1)];
        d = length(discretization);
        if(d<3)
           error('Choose a smaller integration stespize');
        end
        XVARS = zeros(d-1,nvar*NUMSIM);  
        xstart = xstart';
        XVARS(1,:) = xstart([1:size(xstart,1)]' * ones(1,NUMSIM), :)'; % fills the XVARS(1,:) columns with the appropriate starting values; ugly but faster than XVARS(1,:) = repmat(xstart,1,NUMSIM);
    else
        xobs = XOBS(j,:); 
        discretization = [time(j):h:time(j+1)];
        d = length(discretization);
        if(d<3)
           error('Choose a smaller integration stespize');
        end
        XVARS = zeros(d-1,nvar*NUMSIM);
        xobs = xobs';
        XVARS(1,:) = xobs([1:size(xobs,1)]' * ones(1,NUMSIM), :)'; % fills the XVARS(1,:) columns with the appropriate observed values; ugly but faster than XVARS(1,:) = repmat(xobs,1,NUMSIM);
        t = time(j);
    end

    for i= 2:d-1
       x = XVARS(i-1, :)  ;
       Winc = sqrt(h)*randn(1,nvar * NUMSIM);  % the Wiener increment(s) dWi; 'randn' is a built-in Matlab function which is a refinement of the Polar Marsaglia method (--> VERY GOOD!!)

       [f,g] = feval([PROBLEM, '_sdefile'], t, x, [], bigtheta,SDETYPE,NUMDEPVARS,NUMSIM);     % the sdefile output
       
       XVARS(i , :) = x + f * h + g .* Winc ;  % the Euler-Maruyama scheme for Ito SDEs with DIAGONAL noise
       
       t = discretization(i) ;   % now both t and j refer to the end-of-interval   
    end  % for j=(2:d)
    
    xhat_endpoint = XVARS(d-1,:);  % the approximation of X at the interval "endpoint"

    if(nvar==1)
        varcovar_endpoint = g.^2 * h;  % The variance-covariance matrix at the interval "endpoint" for each simulation of a SDEs with DIAGONAL noise
        drift_endpoint = f;  % The drift at the interval "end-point" for each simulation of a SDEs with DIAGONAL noise
        xobsmatrix = repmat(XOBS(j+1,:),1,NUMSIM) ;
        density(j,:) = (2*pi)^(-1/2)*(varcovar_endpoint).^(-1/2) .* exp(-1/2 * (xobsmatrix - xhat_endpoint - h * drift_endpoint).^2 ./ varcovar_endpoint);  % a normal probability density functiun for each simulated trajectory
    else  % vectorized code for multidimensional SDEs with DIAGONAL noise (for ease of clearness the non-vectorized code is reported below) 
        xhat_endpoint = reshape(xhat_endpoint',nvar,[])';  % ugly but necessary; now the 1 x (NUMSIM x nvar) row vector xhat_endpoint is rearranged in a NUMSIM x nvar matrix
        f_endpoint = reshape(f',nvar,[])'; % the same as above, now f (the drift at the interval end-point) is rearranged in a NUMSIM x nvar matrix
        g_endpoint = reshape(g',nvar,[])'; % the same as above, now g (the diffusion at the interval end-point) is rearranged in a NUMSIM x nvar matrix
        var_endpoint = g_endpoint.^2 * h;  % The variances at the interval "end-point" for all the simulations of a SDE with DIAGONAL noise; a NUMSIM x nvar matrix
        xobsmatrix = repmat(XOBS(j+1,:),NUMSIM,1); 
        % Now we compute the multivariate normal probability density function at the j-th time for every
        % trajectory. Notice that, since we consider ONLY SDEs with
        % DIAGONAL noise, we can easily vectorize the code: e.g. the
        % determinant of the covariance matrix is simply the product of the
        % diagonal elements (--> the product of the variances)
        density(j,:) = ( (2*pi)^(-nvar/2)*((prod(var_endpoint,2)).^(-1/2)) .* exp(-1/2 * sum((xobsmatrix - xhat_endpoint - h * f_endpoint).^2 ./ var_endpoint ,2)) )'; 
%       Here follows the non-vectorized version 
%        for r=1:NUMSIM
%            xhat_endpoint_r = xhat_endpoint(nvar*(r - 1) + 1 : r * nvar);
%            varcovar_endpoint_r = diag(g(nvar*(r - 1) + 1 : r * nvar).^2) * h;  % The variance-covariance matrix at the interval "endpoint" for the r-th simulation of a SDEs with DIAGONAL noise
%            drift_endpoint_r = f(nvar*(r - 1) + 1 : r * nvar);  % The drift at the interval "endpoint" for the r-th simulation of a SDEs with DIAGONAL noise
%            density(j,r) = (2*pi)^(-nvar/2)*(det(varcovar_endpoint_r))^(-1/2) * exp(-1/2 * (XOBS(j+1,:) - xhat_endpoint_r - h * drift_endpoint_r) * inv(varcovar_endpoint_r) * (XOBS(j+1,:) - xhat_endpoint_r - h * drift_endpoint_r)');  % a multivariate normal density
%        end
    end
end



