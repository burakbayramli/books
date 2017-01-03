function ok = SDE_stats(bigtheta,xhat,PROBLEM,OWNTIME,NUMDEPVARS,NUMSIM,SDETYPE,INTEGRATOR,SEED)

% Computes descriptive statistics at the process end-time
%
% usage:
% SDE_stats(bigtheta,xhat,PROBLEM,OWNTIME,NUMDEPVARS,NUMSIM,SDETYPE,INTEGRATOR,SEED);
% SDE_stats(bigtheta,[],PROBLEM,OWNTIME,NUMDEPVARS,NUMSIM,SDETYPE,INTEGRATOR,SEED)
%
% IN:        bigtheta; complete vector of structural model parameters 
%     xhat (optional); the SDE approximated solution
%             PROBLEM; the user defined name of the current problem/experiment/example etc. (e.g. 'mySDE')
%             OWNTIME; vector containing the equispaced simulation times sorted in ascending order. 
%                      It has starting simulation-time in first and ending simulation-time in last position. 
%                      Thus OWNTIME(i) - OWNTIME(i-1) = h, where h is the fixed stepsize 
%                      for the numerical intregration (i=2,3,...)
%          NUMDEPVARS; the number of dependent variables, i.e. the SDE dimension
%              NUMSIM; the number of desired simulations for the SDE numerical integration
%             SDETYPE; the SDE definition: can be 'Ito' or 'Strat' (Stratonovich)
%          INTEGRATOR; the SDE fixed stepsize numerical integration method: can be 'EM' (Euler-Maruyama) or 'Mil' (Milstein)
%                SEED; the seed for the generation of pseudo-random normal variates, i.e. the argument for randn('state',SEED);
%                      type 'help randn' for details;

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
error(nargchk(9, 9, nargin));
[n1,n2]=rat(NUMSIM);
if(~isequal(n2,1) || isempty(NUMSIM) || NUMSIM <= 0)
    error('The number of trajectories NUMSIM must be a positive integer');
end
[n1,n2]=rat(NUMDEPVARS);
if(~isequal(n2,1) || isempty(NUMDEPVARS) || NUMDEPVARS <= 0)
    error('The number of variables NUMDEPVARS must be a positive integer');
end
if(strcmp(upper(SDETYPE),'ITO')==0 && strcmp(upper(SDETYPE),'STRAT')==0)
    error('SDETYPE must be ''ITO'' or ''STRAT'' (Stratonovich)');
end
if(strcmp(upper(INTEGRATOR),'EM')==0 && strcmp(upper(INTEGRATOR),'MIL')==0)
    error('INTEGRATOR must be ''EM'' (Euler-Maruyama) or ''MIL'' (Milstein)');
end

if(isempty(xhat))
   xhat = SDE_integrator(bigtheta,PROBLEM,OWNTIME,NUMDEPVARS,NUMSIM,SDETYPE,INTEGRATOR,SEED);
end

handle = waitbar(0,'Computing descriptive statistics...');
for i=1:NUMDEPVARS
    x = xhat(end,i:NUMDEPVARS:end); % the simulated values at the end-time T for the current dependent variables
    x = x(~isnan(x));               % removes NaN's
    avg = mean(x);                  % sample mean
    waitbar(1*i/(10*NUMDEPVARS));
    sd   = std(x);                  % standard deviation
    waitbar(2*i/(10*NUMDEPVARS));
    med  = perctile(x',50);         % median
    waitbar(3*i/(10*NUMDEPVARS));
    lb = perctile(x',2.5);          % Lower Bound of the 95% confidence area at time T, obtained by taking the 2.5% percentile of the simulated trajectories
    waitbar(4*i/(10*NUMDEPVARS));
    ub = perctile(x',97.5);         % Upper Bound of the 95% confidence area at time T, obtained by taking the 97.5% percentile of the simulated trajectories
    waitbar(5*i/(10*NUMDEPVARS));
    q1 = perctile(x',25);           % first quartile 
    waitbar(6*i/(10*NUMDEPVARS));
    q3 = perctile(x',75);           % third quartile 
    waitbar(7*i/(10*NUMDEPVARS));
    skew = SDE_skewness(x);         % the skewness
    waitbar(8*i/(10*NUMDEPVARS));
    kurt = SDE_kurtosis(x);         % the kurtosis
    waitbar(9*i/(10*NUMDEPVARS));
    m2 = SDE_moment(x,2);   % the raw moment of order 2
    m3 = SDE_moment(x,3);   % the raw moment of order 3
    m4 = SDE_moment(x,4);   % the raw moment of order 4
    m5 = SDE_moment(x,5);   % the raw moment of order 5
    m6 = SDE_moment(x,6);   % the raw moment of order 6
    m7 = SDE_moment(x,7);   % the raw moment of order 7
    waitbar(10*i/(10*NUMDEPVARS));
    fprintf('\n\n--------------------------------------------------------------------------------------------------');
    if(NUMDEPVARS==1)
       fprintf('\n-------------------------- MONTE-CARLO STATISTICS for X_T ----------------------------------------');
    else
       fprintf('\n-------------------------- MONTE-CARLO STATISTICS FOR X_T (VARIABLE %d) ---------------------------',i);
    end   
    fprintf('\n--------------------------------------------------------------------------------------------------');
    fprintf('\n');
    fprintf('\nProcess mean at time %d:                                         %d',OWNTIME(end),avg);
    fprintf('\nProcess variance at time %d:                                     %d',OWNTIME(end),sd^2);
    fprintf('\nProcess median at time %d:                                       %d',OWNTIME(end),med);
    fprintf('\n95 percent confidence interval for the trajectories at time %d:  [%d, %d]',OWNTIME(end),lb,ub);
    fprintf('\nProcess first and third quartiles at time %d:                    [%d, %d]',OWNTIME(end),q1,q3);
    fprintf('\nProcess skewness at time %d:                                     %d',OWNTIME(end),skew);
    fprintf('\nProcess kurtosis at time %d:                                     %d',OWNTIME(end),kurt);
    fprintf('\nProcess moment of order 2 at time %d:                            %d',OWNTIME(end),m2);
    fprintf('\nProcess moment of order 3 at time %d:                            %d',OWNTIME(end),m3);
    fprintf('\nProcess moment of order 4 at time %d:                            %d',OWNTIME(end),m4);
    fprintf('\nProcess moment of order 5 at time %d:                            %d',OWNTIME(end),m5);
    fprintf('\nProcess moment of order 6 at time %d:                            %d',OWNTIME(end),m6);
    fprintf('\nProcess moment of order 7 at time %d:                            %d',OWNTIME(end),m7);
end
fprintf('\n');
close(handle);
    