function ok = SDE_graph(bigtheta,xhat,yesdata,PROBLEM,SDETYPE,INTEGRATOR,NUMDEPVARS,OWNTIME,MODEL,NUMSIM,TIME,XOBS,SEED)

% Plots the empirical mean, confidence intervals, quartiles, histograms and observations (when available) from the SDE solution process
%
% usage: SDE_graph(bigtheta,xhat,1,PROBLEM,SDETYPE,INTEGRATOR,NUMDEPVARS,OWNTIME,[],NUMSIM,TIME,XOBS,SEED)
%        SDE_graph(bigtheta,xhat,0,PROBLEM,SDETYPE,INTEGRATOR,NUMDEPVARS,OWNTIME,[],NUMSIM,[],[],SEED)
%        SDE_graph(bigtheta,xhat,1,PROBLEM,SDETYPE,INTEGRATOR,NUMDEPVARS,OWNTIME,MODEL,NUMSIM,TIME,XOBS,SEED)
%        SDE_graph(bigtheta,xhat,0,PROBLEM,SDETYPE,INTEGRATOR,NUMDEPVARS,OWNTIME,MODEL,NUMSIM,[],[],SEED)
%        SDE_graph(bigtheta,[],0,PROBLEM,SDETYPE,INTEGRATOR,NUMDEPVARS,OWNTIME,MODEL,NUMSIM,[],[],SEED)
%        SDE_graph(bigtheta,[],1,PROBLEM,SDETYPE,INTEGRATOR,NUMDEPVARS,OWNTIME,MODEL,NUMSIM,TIME,XOBS,SEED)
%
% IN:     bigtheta; the array of the model structural parameters
%         xhat (optional); the SDE approximated solution
%         yesdata; can be 1 if data are available and 0 otherwise
%         PROBLEM; the user defined name of the current problem/experiment/example etc. (e.g. 'mySDE')
%         SDETYPE; the SDE definition: can be 'Ito' or 'Strat' (Stratonovich)
%         INTEGRATOR; the SDE fixed stepsize numerical integration method: can be 'EM' (Euler-Maruyama) or 'Mil' (Milstein)
%         NUMDEPVARS; the number of dependent variables, i.e. the SDE dimension
%         OWNTIME; vector containing the equispaced simulation times sorted in ascending order. 
%                  It has starting simulation-time in first and ending simulation-time in last position. 
%                  Thus OWNTIME(i) - OWNTIME(i-1) = h, where h is the fixed stepsize 
%                  for the numerical intregration (i=2,3,...)
%         MODEL (optional); the model name (e.g. 'M1a', 'M1b', etc.). This can be left empty ([]). It is useful for 
%                           illustration purposes e.g. when running SDE_library_run.m
%         NUMSIM; the number of desired simulations for the SDE numerical integration 
%         XOBS; the matrix-shaped observed data; should be empty ([]) when yesdata=0
%         TIME; the array of unique observation times; should be empty ([]) when yesdata=0
%         SEED; the seed for the generation of pseudo-random normal variates, i.e. the argument for randn('state',SEED);
%               type 'help randn' for details;

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
error(nargchk(13, 13, nargin));
[n1,n2]=rat(NUMSIM);
if(~isequal(n2,1) || isempty(NUMSIM) || NUMSIM <= 0)
    error('The number of trajectories NUMSIM must be a positive integer');
end
[n1,n2]=rat(NUMDEPVARS);
if(~isequal(n2,1) || isempty(NUMDEPVARS) || NUMDEPVARS <= 0)
    error('The number of variables NUMDEPVARS must be a positive integer');
end
if(yesdata ~=0 && yesdata ~=1)
    error('yesdata must be 0 or 1')
end

if(isempty(xhat))
   xhat = SDE_integrator(bigtheta,PROBLEM,OWNTIME,NUMDEPVARS,NUMSIM,SDETYPE,INTEGRATOR,SEED);
end

handle = waitbar(0,'Plotting simulations...');
for i=1:NUMDEPVARS
    lbxhat = perctile(xhat(:,i:NUMDEPVARS:end)',2.5);  % Lower Bound of the 95% confidence area obtained by taking, at each time, the 2.5% percentile of the simulated trajectories
    waitbar(i/(8*NUMDEPVARS));
    ubxhat = perctile(xhat(:,i:NUMDEPVARS:end)',97.5); % Upper Bound of the 95% confidence area obtained by taking, at each time, the 97.5% percentile of the simulated trajectories
    waitbar(2*i/(8*NUMDEPVARS));
    q1xhat = perctile(xhat(:,i:NUMDEPVARS:end)',25);   % first quartile of the simulated trajectories
    waitbar(3*i/(8*NUMDEPVARS));
    q3xhat = perctile(xhat(:,i:NUMDEPVARS:end)',75);   % third quartile of the simulated trajectories
    waitbar(4*i/(8*NUMDEPVARS));
    meanxhat = mean(xhat(:,i:NUMDEPVARS:end),2);       % the empirical mean of the process
    waitbar(5*i/(8*NUMDEPVARS));
    
    % Plots of time vs simulated trajectories (vs data if yesdata=1)
    figure
    % data
    if (yesdata)
    %    t = TIME((VRBL == i));
        t = TIME;
        y = XOBS(:,i);
        plot(t, y, 'bo');
        hold on;
    end
    plot(OWNTIME,xhat(:,i:NUMDEPVARS:end),'k-'), hold on
    xlabel('t','Fontsize',13,'Rotation',0);
    if(NUMDEPVARS==1)
        ylabel_text = sprintf('X_t');
    else
        ylabel_text = sprintf('X_t^{(%d)}',i);
    end
    ylabel(ylabel_text,'Fontsize',13,'Rotation',0);
    titlestring = sprintf('Model %s: numerical solution over %d trajectories',MODEL,NUMSIM);
    if(yesdata)
       titlestring = sprintf('Model %s: numerical solution over %d trajectories and observations',MODEL,NUMSIM);
    end
    title(titlestring,'Fontsize',12);
    hold off;
    waitbar(6*i/(8*NUMDEPVARS));
    
    % Plots of time vs process mean/quartiles/95% confidence intervals of the trajectories (vs data if yesdata=1)
    figure
    % data
    if (yesdata)
    %    t = TIME((VRBL == i));
        t = TIME;
        y = XOBS(:,i);
        plot(t, y, 'bo');
        hold on;
    end
    %              the empirical 95% CI,                first and third quartile              the process empirical mean
    plot(OWNTIME,lbxhat,'k--',OWNTIME,ubxhat,'k--',OWNTIME,q1xhat,'k:',OWNTIME,q3xhat,'k:',   OWNTIME,meanxhat,'g-') 
    xlabel('t','Fontsize',13,'Rotation',0);
    ylabel(ylabel_text,'Fontsize',13,'Rotation',0);
    titlestring = sprintf('Model %s: Empirical mean, 95 percent CI, q1-q3 quartiles of the numerical solution over %d trajectories',MODEL,NUMSIM);
    if(yesdata)
        titlestring = sprintf('Model %s: Empirical mean, 95 percent CI, q1-q3 quartiles of the numerical solution over %d trajectories and observations',MODEL,NUMSIM);
    end
    title(titlestring,'Fontsize',12);
    hold off;
    waitbar(7*i/(8*NUMDEPVARS));
    
    % Plots the histogram of the trajectories at the end-time
    figure
    hist(xhat(end,i:NUMDEPVARS:end),20);
    xlabel('X_T','Fontsize',13,'Rotation',0);
    if(NUMDEPVARS==1)
        titlestring = sprintf('Model %s: histogram of X_t at end-time T=%d',MODEL,OWNTIME(end));
    else
        titlestring = sprintf('Model %s: histogram of X_t^{(%d)} at end-time T=%d',MODEL,i,OWNTIME(end));
    end
    title(titlestring,'Fontsize',12);
    waitbar(8*i/(8*NUMDEPVARS));
end
close(handle);