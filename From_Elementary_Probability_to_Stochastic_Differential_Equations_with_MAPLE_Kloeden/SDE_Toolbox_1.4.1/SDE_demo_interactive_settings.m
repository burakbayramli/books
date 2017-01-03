function bigtheta = SDE_demo_interactive_settings()

% Asks the user to provide interactively the simulation settings for SDE_demo.m 
% Returns the SDE model structural parameters.
%
% usage: bigtheta = SDE_demo_interactive_settings;
%
% Definitions for the global variables
%-------------------------------------------------------------------------------------------------------------------------------
% global NUMDEPVARS; the number of dependent variables, i.e. the SDE dimension
% global NUMSIM; the number of desired simulations for the SDE numerical integration 
% global OWNTIME; vector containing the equispaced simulation times sorted in ascending order. 
%                 It has starting simulation-time in first and ending simulation-time in last position. 
%                 Thus OWNTIME(i) - OWNTIME(i-1) = h, where h is the fixed stepsize 
%                 for the numerical intregration (i=2,3,...)
% global PROBLEM; the user defined name of the current problem/experiment/example etc. (e.g. 'mySDE')
% global FIXRANDSEED; fix the seed for the normal variates generation ('Y') or not ('N')? In the latter case
%                     a new seed is pseudo-randomly chosen each time the function is invoked. 
% global DW; the stochastic Wiener increments dW with dW(1,:) = 0; 
%
% OUT: bigtheta; complete structural parameter vector

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

global NUMSIM PROBLEM NUMDEPVARS OWNTIME FIXRANDSEED DW;


%::::::::::: read and check the keyboard inputs :::::::::::::::: 
NUMSIM = input('\n\nNumber of trajectories to be simulated: ');
[n1,n2]=rat(NUMSIM);
if(~isequal(n2,1) || isempty(NUMSIM))
    error('The number of trajectories must be an integer');
end
T0 = input('\nStarting time T0: ');
if(isempty(T0))
    error('T0 must be specified');
end
T  = input('\nFinal time T: ');
if(isempty(T))
    error('T must be specified');
end
if(T<=T0)
    error('T must be greater than T0')
end
h  = input('\nIntegration stepsize h  (should be << T - T0): ');
if(isempty(h))
    error('The stepsize h must be specified');
end
if(h>(T-T0))
    error('Choose a stepsize << (T - T0)')
end
a  = input('\nThe SDE ''a'' parameter: ');
if(isempty(a))
    error('''a'' must be specified');
end
Xzero = input('\nThe SDE initial condition X0 (must be non-zero): ');
if(isempty(Xzero))
    error('X0 must be specified');
elseif(Xzero==0)
    error('X0 must be non-zero');
end

%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%
%                            
%
% Problem Settings: 
PROBLEM = 'demo';    % the problem name
NUMDEPVARS = 1;      % the number of dependent variables
OWNTIME = T0:h:T;    % the desired discretization for [T0,T]
FIXRANDSEED = 'Y';   % fix the initial state of the normal generator seed 
                     % to get repeatable results [Y/N] (see below)
%
%                        
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


N = length(OWNTIME);
% create the Wiener increments
switch FIXRANDSEED
case 'Y'                   % fix the initial state to get repeatable results (case 'Y')
      randn('state',0)      
case 'N' 
      randn('state',sum(100*clock))
end

% Creation of Wiener increments: notice that in this demo for ease of illustration the 'antithetic
% variates' method for the increments generation is NOT employed
fprintf('\n\n...Creating the matrix of Wiener random increments...\n\n');
DW = sqrt(h)*[zeros(1,NUMDEPVARS * NUMSIM);randn(N-1,NUMDEPVARS * NUMSIM)];  % the stochastic Wiener increments dW with dW(1,:) = 0, since W(0)=0, where W(0) is the Wiener process at time t=0
                                                                             % 'randn' is a built-in Matlab function which is a refinement of the Polar Marsaglia method (--> VERY GOOD!!)

% store the SDE parameters into the bigtheta array
bigtheta(1) = Xzero;  
bigtheta(2) = a; 