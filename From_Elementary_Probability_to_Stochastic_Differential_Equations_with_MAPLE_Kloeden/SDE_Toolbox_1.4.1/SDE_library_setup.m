function sde_setup_output = SDE_library_setup(sde_setup_input)

% Library setup: here the number of the state variables, the parameters for
% the models of the library and basic constructs for the library usage are
% defined.
%
% IN:  sde_setup_input; a structure containing the following fields:
%                       sde_setup_input.loaddata; load data from datafile? can be Yes ('Y') or Not ('N')
%                       sde_setup_input.parestimate; estimate parameters? can be Yes ('Y') or Not ('N')
%                       sde_setup_input.time; the array of unique observation times
%                       sde_setup_input.vrbl; the array of unique label-variables
%                       sde_setup_input.xobs; the matrix-shaped observed data
% OUT: sde_setup_output; a structure containing the following fields:
%                        sde_setup_output.bigtheta; complete structural parameter vector
%                        sde_setup_output.parbase; the same as bigtheta, provides parameters starting values for the optimization procedure 
%                        sde_setup_output.parmask; an array containing ones in correspondence of the parameters in bigtheta to be estimated 
%                                                  and zeros in correspondence of the parameters to be held fixed (constant); it has the same 
%                                                  length of bigtheta.                                
%                        sde_setup_output.problem; the name of the current problem/experiment/example etc. (e.g. 'M1')
%                        sde_setup_output.sdetype; the SDE definition: can be 'Ito' or 'Strat' (Stratonovich)
%                        sde_setup_output.numdepvars; the number of dependent variables, i.e. the SDE dimension
%                        sde_setup_output.numsim; the number of desired simulations for the SDE numerical integration 
%                        sde_setup_output.model; the model name (e.g. 'M1a', 'M1b', etc.).
%                        sde_setup_output.owntime; vector containing the equispaced simulation times sorted in ascending order. 
%                                                  It has starting simulation-time in first and ending simulation-time in last position. 
%                                                  Thus OWNTIME(i) - OWNTIME(i-1) = h, where h is the fixed stepsize for the numerical intregration (i=2,3,...)
%                        sde_setup_output.time; the array of unique observation times
%                        sde_setup_output.vrbl; the array of unique label-variables 
%                        sde_setup_output.integrator; the SDE fixed stepsize numerical integration method: can be 'EM' (Euler-Maruyama) or 'Mil' (Milstein)

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

MODEL = upper(input('\nWrite the name of the chosen model (e.g. M1a for MODEL 1 with Ito definition, M1b for MODEL 1 with Stratonovich definition, etc.): ','s'));

% For each model the name of the problem (e.g. 'M1a'), the number of
% the state variables (NUMDEPVARS), the SDE type (Ito or Stratonovich) and
% the parameters are specified.
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

LOADDATA = sde_setup_input.loaddata;
PARESTIMATE = sde_setup_input.parestimate;
TIME = sde_setup_input.time;
VRBL = sde_setup_input.vrbl;
XOBS = sde_setup_input.xobs;
    
switch MODEL
    
    case 'M1A'
          PROBLEM = 'M1';
          SDETYPE = 'Ito';
          NUMDEPVARS = 1;
          fprintf('\nYou choose     dXt = -a * Xt * dt + sigma * dWt,   X(0) = X0');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             a  = input('\n\nWrite the value of the ''a'' parameter: ');
             if(isempty(a))
                error('''a'' must be specified');
             end
             sigma  = input('\nWrite the value of the ''sigma'' parameter: ');
             if(isempty(sigma))
                error('''sigma'' must be specified');
             end
             Xzero = input('\nWrite the value of the initial condition X0: ');
             if(isempty(Xzero))
                error('X0 must be specified');
             end
             % store the parameters into the bigtheta array
             bigtheta(1) = Xzero;  
             bigtheta(2) = a; 
             bigtheta(3) = sigma;
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n'); 
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,1,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) should always be set to zero (corresponds to the SDE initial condition)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    case 'M1B' 
          PROBLEM = 'M1';
          SDETYPE = 'Strat';
          NUMDEPVARS = 1;
          fprintf('\n\nYou choose     dXt = -a * Xt * dt + sigma o dWt,   X(0) = X0');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             a  = input('\n\nWrite the value of the ''a'' parameter: ');
             if(isempty(a))
                error('''a'' must be specified');
             end
             sigma  = input('\nWrite the value of the ''sigma'' parameter: ');
             if(isempty(sigma))
                error('''sigma'' must be specified');
             end
             Xzero = input('\nWrite the value of the initial condition X0: ');
             if(isempty(Xzero))
                error('X0 must be specified');
             end
             % store the parameters into the bigtheta array
             bigtheta(1) = Xzero;  
             bigtheta(2) = a; 
             bigtheta(3) = sigma;     
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n');
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,1,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) should always be set to zero (corresponds to the SDE initial condition)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          
     %-------------------------------------------------------------------------------------------------------------------
     
     case 'M2A' 
          PROBLEM = 'M2';
          SDETYPE = 'Ito';
          NUMDEPVARS = 1;
          fprintf('\n\nYou choose     dXt = (a * Xt + b) * dt + sigma * dWt,   X(0) = X0');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             a  = input('\n\nWrite the value of the ''a'' parameter: ');
             if(isempty(a))
                error('''a'' must be specified');
             end
             b  = input('\nWrite the value of the ''b'' parameter: ');
             if(isempty(b))
                error('''b'' must be specified');
             end
             sigma  = input('\nWrite the value of the ''sigma'' parameter: ');
             if(isempty(sigma))
                error('''sigma'' must be specified');
             end
             Xzero = input('\nWrite the value of the initial condition X0: ');
             if(isempty(Xzero))
                error('X0 must be specified');
             end
            % store the parameters into the bigtheta array
             bigtheta(1) = Xzero;  
             bigtheta(2) = a; 
             bigtheta(3) = b;
             bigtheta(4) = sigma;
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n');
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,1,1,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) should always be set to zero (corresponds to the SDE initial condition)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     case 'M2B' 
          PROBLEM = 'M2';
          SDETYPE = 'Strat';
          NUMDEPVARS = 1;
          fprintf('\n\nYou choose     dXt = (a * Xt + b) * dt + sigma o dWt,   X(0) = X0');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             a  = input('\n\nWrite the value of the ''a'' parameter: ');
             if(isempty(a))
                error('''a'' must be specified');
             end
             b  = input('\nWrite the value of the ''b'' parameter: ');
             if(isempty(b))
                error('''b'' must be specified');
             end
             sigma  = input('\nWrite the value of the ''sigma'' parameter: ');
             if(isempty(sigma))
                error('''sigma'' must be specified');
             end
             Xzero = input('\nWrite the value of the initial condition X0: ');
             if(isempty(Xzero))
                error('X0 must be specified');
             end
            % store the parameters into the bigtheta array
             bigtheta(1) = Xzero;  
             bigtheta(2) = a; 
             bigtheta(3) = b;
             bigtheta(4) = sigma;   
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n');
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,1,1,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) should always be set to zero (corresponds to the SDE initial condition)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          
     %-------------------------------------------------------------------------------------------------------------------
     
     case 'M3A' 
          PROBLEM = 'M3';
          SDETYPE = 'Ito';
          NUMDEPVARS = 1;
          fprintf('\n\nYou choose     dXt = (a - sigma^2/2) * dt + sigma * dWt,   X(0) = X0');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             a  = input('\n\nWrite the value of the ''a'' parameter: ');
             if(isempty(a))
                error('''a'' must be specified');
             end
             sigma  = input('\nWrite the value of the ''sigma'' parameter: ');
             if(isempty(sigma))
                error('''sigma'' must be specified');
             end
             Xzero = input('\nWrite the value of the initial condition X0: ');
             if(isempty(Xzero))
                error('X0 must be specified');
             end
            % store the parameters into the bigtheta array
             bigtheta(1) = Xzero;  
             bigtheta(2) = a; 
             bigtheta(3) = sigma;
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n'); 
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,1,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) should always be set to zero (corresponds to the SDE initial condition)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     case 'M3B' 
          PROBLEM = 'M3';
          SDETYPE = 'Strat';
          NUMDEPVARS = 1;
          fprintf('\n\nYou choose     dXt = (a - sigma^2/2) * dt + sigma o dWt,   X(0) = X0');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             a  = input('\n\nWrite the value of the ''a'' parameter: ');
             if(isempty(a))
                error('''a'' must be specified');
             end
             sigma  = input('\nWrite the value of the ''sigma'' parameter: ');
             if(isempty(sigma))
                error('''sigma'' must be specified');
             end
             Xzero = input('\nWrite the value of the initial condition X0: ');
             if(isempty(Xzero))
                error('X0 must be specified');
             end
            % store the parameters into the bigtheta array
             bigtheta(1) = Xzero;  
             bigtheta(2) = a; 
             bigtheta(3) = sigma;
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n'); 
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,1,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) should always be set to zero (corresponds to the SDE initial condition)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          
     %-------------------------------------------------------------------------------------------------------------------  
     
     case 'M4A' 
          PROBLEM = 'M4';
          SDETYPE = 'Ito';
          NUMDEPVARS = 1;
          fprintf('\n\nYou choose     dXt = a * Xt * dt + b * Xt * dWt,   X(0) = X0');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             a  = input('\n\nWrite the value of the ''a'' parameter: ');
             if(isempty(a))
                error('''a'' must be specified');
             end
             b  = input('\nWrite the value of the ''b'' parameter: ');
             if(isempty(b))
                error('''b'' must be specified');
             end
             Xzero = input('\nWrite the value of the initial condition X0: ');
             if(isempty(Xzero))
                error('X0 must be specified');
             end
            % store the parameters into the bigtheta array
             bigtheta(1) = Xzero;  
             bigtheta(2) = a; 
             bigtheta(3) = b;
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n'); 
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,1,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) should always be set to zero (corresponds to the SDE initial condition)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     case 'M4B' 
          PROBLEM = 'M4';
          SDETYPE = 'Strat';
          NUMDEPVARS = 1;
          fprintf('\n\nYou choose     dXt = (a * Xt - 1/2 * b^2 * Xt) * dt + b * Xt o Wt,   X(0) = X0');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             a  = input('\n\nWrite the value of the ''a'' parameter: ');
             if(isempty(a))
                error('''a'' must be specified');
             end
             b  = input('\nWrite the value of the ''b'' parameter: ');
             if(isempty(b))
                error('''b'' must be specified');
             end
             Xzero = input('\nWrite the value of the initial condition X0: ');
             if(isempty(Xzero))
                error('X0 must be specified');
             end
            % store the parameters into the bigtheta array
             bigtheta(1) = Xzero;  
             bigtheta(2) = a; 
             bigtheta(3) = b;
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n'); 
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,1,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) should always be set to zero (corresponds to the SDE initial condition)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          
     %------------------------------------------------------------------------------------------------------------------- 
     
     case 'M5A' 
          PROBLEM = 'M5';
          SDETYPE = 'Ito';
          NUMDEPVARS = 1;
          fprintf('\n\nYou choose     dXt = (a * Xt + c) * dt + (b * Xt + d) * dWt,   X(0) = X0');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             a  = input('\n\nWrite the value of the ''a'' parameter: ');
             if(isempty(a))
                error('''a'' must be specified');
             end
             b  = input('\nWrite the value of the ''b'' parameter: ');
             if(isempty(b))
                error('''b'' must be specified');
             end
             c  = input('\nWrite the value of the ''c'' parameter: ');
             if(isempty(c))
                error('''c'' must be specified');
             end
             d  = input('\nWrite the value of the ''d'' parameter: ');
             if(isempty(d))
                error('''d'' must be specified');
             end
             Xzero = input('\nWrite the value of the initial condition X0: ');
             if(isempty(Xzero))
                error('X0 must be specified');
             end
            % store the parameters into the bigtheta array
             bigtheta(1) = Xzero;  
             bigtheta(2) = a; 
             bigtheta(3) = b;
             bigtheta(4) = c;
             bigtheta(5) = d;
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n'); 
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,1,1,1,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) should always be set to zero (corresponds to the SDE initial condition)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     case 'M5B' 
          PROBLEM = 'M5';
          SDETYPE = 'Strat';
          NUMDEPVARS = 1;
          fprintf('\n\nYou choose     dXt = [(a - 1/2 * b) * Xt + c - 1/2 * b * d] * dt + (b * Xt + d) o dWt,   X(0) = X0');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             a  = input('\n\nWrite the value of the ''a'' parameter: ');
             if(isempty(a))
                error('''a'' must be specified');
             end
             b  = input('\nWrite the value of the ''b'' parameter: ');
             if(isempty(b))
                error('''b'' must be specified');
             end
             c  = input('\nWrite the value of the ''c'' parameter: ');
             if(isempty(c))
                error('''c'' must be specified');
             end
             d  = input('\nWrite the value of the ''d'' parameter: ');
             if(isempty(d))
                error('''d'' must be specified');
             end
             Xzero = input('\nWrite the value of the initial condition X0: ');
             if(isempty(Xzero))
                error('X0 must be specified');
             end
            % store the parameters into the bigtheta array
             bigtheta(1) = Xzero;  
             bigtheta(2) = a; 
             bigtheta(3) = b;
             bigtheta(4) = c;
             bigtheta(5) = d;
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n'); 
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,1,1,1,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) should always be set to zero (corresponds to the SDE initial condition)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          
     %-------------------------------------------------------------------------------------------------------------------    
     
     case 'M6A' 
          PROBLEM = 'M6';
          SDETYPE = 'Ito';
          NUMDEPVARS = 1;
          fprintf('\n\nYou choose     dXt = [1/2 * a * (a - 1) * Xt ^ (1-2/a)] * dt + a * Xt ^ (1-1/a) * dWt,   X(0) = X0');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             a  = input('\n\nWrite the value of the ''a'' parameter: ');
             if(isempty(a))
                error('''a'' must be specified');
             end
             Xzero = input('\nWrite the value of the initial condition X0: ');
             if(isempty(Xzero))
                error('X0 must be specified');
             end
             % store the parameters into the bigtheta array
             bigtheta(1) = Xzero;  
             bigtheta(2) = a; 
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n'); 
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) should always be set to zero (corresponds to the SDE initial condition)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     case 'M6B' 
          PROBLEM = 'M6';
          SDETYPE = 'Strat';
          NUMDEPVARS = 1;
          fprintf('\n\nYou choose     [a * Xt ^ (1-1/a)] o dWt,   X(0) = X0');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             a  = input('\n\nWrite the value of the ''a'' parameter: ');
             if(isempty(a))
                error('''a'' must be specified');
             end
             Xzero = input('\nWrite the value of the initial condition X0: ');
             if(isempty(Xzero))
                error('X0 must be specified');
             end
            % store the parameters into the bigtheta array
             bigtheta(1) = Xzero;  
             bigtheta(2) = a; 
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n'); 
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) should always be set to zero (corresponds to the SDE initial condition)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          
     %-------------------------------------------------------------------------------------------------------------------     
     
     case 'M7A' 
          PROBLEM = 'M7';
          SDETYPE = 'Ito';
          NUMDEPVARS = 1;
          fprintf('\n\nYou choose     dXt = [-1/2 * a^2 * Xt] * dt + a * sqrt(1 - Xt^2) * dWt,   X(0) = X0');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             a  = input('\n\nWrite the value of the ''a'' parameter: ');
             if(isempty(a))
                error('''a'' must be specified');
             end
             Xzero = input('\nWrite the value of the initial condition X0: ');
             if(isempty(Xzero))
                error('X0 must be specified');
             end
            % store the parameters into the bigtheta array
             bigtheta(1) = Xzero;  
             bigtheta(2) = a;  
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n'); 
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) should always be set to zero (corresponds to the SDE initial condition)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     case 'M7B' 
          PROBLEM = 'M7';
          SDETYPE = 'Strat';
          NUMDEPVARS = 1;
          fprintf('\n\nYou choose     dXt = a * sqrt(1 - Xt^2) o dWt,   X(0) = X0');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             a  = input('\n\nWrite the value of the ''a'' parameter: ');
             if(isempty(a))
                error('''a'' must be specified');
             end
             Xzero = input('\nWrite the value of the initial condition X0: ');
             if(isempty(Xzero))
                error('X0 must be specified');
             end
            % store the parameters into the bigtheta array
             bigtheta(1) = Xzero;  
             bigtheta(2) = a;  
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n'); 
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) should always be set to zero (corresponds to the SDE initial condition)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          
     %-------------------------------------------------------------------------------------------------------------------   
     
     case 'M8A' 
          PROBLEM = 'M8';
          SDETYPE = 'Ito';
          NUMDEPVARS = 1;
          fprintf('\n\nYou choose     dXt = [a^2 * Xt * (1 + Xt^2)] * dt + a * (1 + Xt^2) * dWt,   X(0) = X0');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             a  = input('\n\nWrite the value of the ''a'' parameter: ');
             if(isempty(a))
                error('''a'' must be specified');
             end
             Xzero = input('\nWrite the value of the initial condition X0: ');
             if(isempty(Xzero))
                error('X0 must be specified');
             end
            % store the parameters into the bigtheta array
             bigtheta(1) = Xzero;  
             bigtheta(2) = a;  
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n'); 
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) should always be set to zero (corresponds to the SDE initial condition)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     case 'M8B'
          PROBLEM = 'M8';
          SDETYPE = 'Strat';
          NUMDEPVARS = 1;
          fprintf('\n\nYou choose     dXt = a * (1 + Xt^2) o dWt,   X(0) = X0');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             a  = input('\n\nWrite the value of the ''a'' parameter: ');
             if(isempty(a))
                error('''a'' must be specified');
             end
             Xzero = input('\nWrite the value of the initial condition X0: ');
             if(isempty(Xzero))
                error('X0 must be specified');
             end
            % store the parameters into the bigtheta array
             bigtheta(1) = Xzero;  
             bigtheta(2) = a;  
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n'); 
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) should always be set to zero (corresponds to the SDE initial condition)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          
     %------------------------------------------------------------------------------------------------------------------- 
     
     case 'M9A' 
          PROBLEM = 'M9';
          SDETYPE = 'Ito';
          NUMDEPVARS = 2;
          fprintf('\n\nYou choose:     dXt1 = [beta11 * alpha1 + beta12 * alpha2 - beta11 * Xt1 - beta12 * Xt2] * dt + sigma1 * dWt1,   X1(0) = X01');
          fprintf('\n                dXt2 = [beta21 * alpha1 + beta22 * alpha2 - beta21 * Xt1 - beta22 * Xt2] * dt + sigma2 * dWt2,   X2(0) = X02');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             Xzero1  = input('\n\nWrite the value of the ''X01'' parameter: ');
             Xzero2  = input('\n\nWrite the value of the ''X02'' parameter: ');
             alpha1  = input('\n\nWrite the value of the ''alpha1'' parameter: ');
             alpha2  = input('\n\nWrite the value of the ''alpha2'' parameter: ');
             beta11  = input('\n\nWrite the value of the ''beta11'' parameter: ');
             beta12  = input('\n\nWrite the value of the ''beta12'' parameter: ');
             beta21  = input('\n\nWrite the value of the ''beta21'' parameter: ');
             beta22  = input('\n\nWrite the value of the ''beta22'' parameter: ');
             sigma1  = input('\n\nWrite the value of the ''sigma1'' parameter: ');
             sigma2  = input('\n\nWrite the value of the ''sigma2'' parameter: ');
             if(isempty(beta11)||isempty(beta21)||isempty(beta12)||isempty(beta22)||isempty(alpha1)||isempty(alpha2)||isempty(sigma1)||isempty(sigma2)||isempty(Xzero1)||isempty(Xzero2))
                error('All the parameters must be specified');
             end
            % store the parameters into the bigtheta array
             bigtheta(1) = Xzero1;   
             bigtheta(2) = Xzero2;  
             bigtheta(3) = alpha1; 
             bigtheta(4) = alpha2; 
             bigtheta(5) = beta11; 
             bigtheta(6) = beta12; 
             bigtheta(7) = beta21; 
             bigtheta(8) = beta22;
             bigtheta(9) = sigma1;
             bigtheta(10)= sigma2; 
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n'); 
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,0,1,1,1,1,1,1,1,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) and PARMASK(2) should always be set to zero (correspond to the SDE initial conditions)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     case 'M9B' 
          PROBLEM = 'M9';
          SDETYPE = 'Strat';
          NUMDEPVARS = 2;
          fprintf('\n\nYou choose:     dXt1 = [beta11 * alpha1 + beta12 * alpha2 - beta11 * Xt1 - beta12 * Xt2] * dt + sigma1 o dWt1,   X1(0) = X01');
          fprintf('\n                dXt2 = [beta21 * alpha1 + beta22 * alpha2 - beta21 * Xt1 - beta22 * Xt2] * dt + sigma2 o dWt2,   X2(0) = X02');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             Xzero1  = input('\n\nWrite the value of the ''X01'' parameter: ');
             Xzero2  = input('\n\nWrite the value of the ''X02'' parameter: ');
             alpha1  = input('\n\nWrite the value of the ''alpha1'' parameter: ');
             alpha2  = input('\n\nWrite the value of the ''alpha2'' parameter: ');
             beta11  = input('\n\nWrite the value of the ''beta11'' parameter: ');
             beta12  = input('\n\nWrite the value of the ''beta12'' parameter: ');
             beta21  = input('\n\nWrite the value of the ''beta21'' parameter: ');
             beta22  = input('\n\nWrite the value of the ''beta22'' parameter: ');
             sigma1  = input('\n\nWrite the value of the ''sigma1'' parameter: ');
             sigma2  = input('\n\nWrite the value of the ''sigma2'' parameter: ');
             if(isempty(beta11)||isempty(beta21)||isempty(beta12)||isempty(beta22)||isempty(alpha1)||isempty(alpha2)||isempty(sigma1)||isempty(sigma2)||isempty(Xzero1)||isempty(Xzero2))
                error('All the parameters must be specified');
             end
            % store the parameters into the bigtheta array
             bigtheta(1) = Xzero1;   
             bigtheta(2) = Xzero2;  
             bigtheta(3) = alpha1; 
             bigtheta(4) = alpha2; 
             bigtheta(5) = beta11; 
             bigtheta(6) = beta12; 
             bigtheta(7) = beta21; 
             bigtheta(8) = beta22;
             bigtheta(9) = sigma1;
             bigtheta(10)= sigma2;  
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n'); 
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,0,1,1,1,1,1,1,1,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) and PARMASK(2) should always be set to zero (correspond to the SDE initial conditions)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          
     %-------------------------------------------------------------------------------------------------------------------
     
     case 'M10A'
          PROBLEM = 'M10';
          SDETYPE = 'Ito';
          NUMDEPVARS = 1;
          fprintf('\nYou choose     dXt = a * (b - Xt) * dt + sigma * sqrt(Xt) * dWt,   X(0) = X0');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             a  = input('\n\nWrite the value of the ''a'' parameter: ');
             if(isempty(a))
                error('''a'' must be specified');
             end
             b  = input('\nWrite the value of the ''b'' parameter: ');
             if(isempty(b))
                error('''b'' must be specified');
             end
             sigma  = input('\nWrite the value of the ''sigma'' parameter: ');
             if(isempty(sigma))
                error('''sigma'' must be specified');
             end
             Xzero = input('\nWrite the value of the initial condition X0: ');
             if(isempty(Xzero))
                error('X0 must be specified');
             end
             % store the parameters into the bigtheta array
             bigtheta(1) = Xzero;  
             bigtheta(2) = a; 
             bigtheta(3) = b;
             bigtheta(4) = sigma;
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n'); 
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,1,1,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) should always be set to zero (corresponds to the SDE initial condition)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    case 'M10B' 
          PROBLEM = 'M10';
          SDETYPE = 'Strat';
          NUMDEPVARS = 1;
          fprintf('\n\nYou choose     dXt = [a * (b - Xt) -1/4 * sigma^2] * dt + sigma * sqrt(Xt) o dWt,   X(0) = X0');
          if( (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'N')) || (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y')) ||  (strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'N')) )
             a  = input('\n\nWrite the value of the ''a'' parameter: ');
             if(isempty(a))
                error('''a'' must be specified');
             end
             b  = input('\nWrite the value of the ''b'' parameter: ');
             if(isempty(b))
                error('''b'' must be specified');
             end
             sigma  = input('\nWrite the value of the ''sigma'' parameter: ');
             if(isempty(sigma))
                error('''sigma'' must be specified');
             end
             Xzero = input('\nWrite the value of the initial condition X0: ');
             if(isempty(Xzero))
                error('X0 must be specified');
             end
             % store the parameters into the bigtheta array
             bigtheta(1) = Xzero;  
             bigtheta(2) = a; 
             bigtheta(3) = b;
             bigtheta(4) = sigma;     
             PARBASE = bigtheta; % the complete array of the user defined parameter values
          elseif( strcmp(LOADDATA,'Y') && strcmp(PARESTIMATE,'Y') )
             fprintf('\n');
             bigtheta(1:NUMDEPVARS) = XOBS(1,:);  % subsititute the state variable(s) initial condition(s) with the first observed value(s)
             PARBASE = [];
          end  % if( (strcmp(LOADDATA,'N') || strcmp(LOADDATA,'n')) && (strcmp(PARESTIMATE,'Y') || strcmp(PARESTIMATE,'y')) )
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          PARMASK = [0,1,1,1];  % write 1 for parameters to be estimated and 0 for fixed parameters. WARNING: PARMASK(1) should always be set to zero (corresponds to the SDE initial condition)
          %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          
     %-------------------------------------------------------------------------------------------------------------------
     
    otherwise
          error('The model name ''%s'' is not included in the list of available models...please check.',MODEL);
end
%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::








%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%                                  DO NOT MODIFY THE CODE BELOW   *    DO NOT MODIFY THE CODE BELOW   *    DO NOT MODIFY THE CODE BELOW    *    DO NOT MODIFY THE CODE BELOW
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%                                  DO NOT MODIFY THE CODE BELOW   *    DO NOT MODIFY THE CODE BELOW   *    DO NOT MODIFY THE CODE BELOW    *    DO NOT MODIFY THE CODE BELOW
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%                                  DO NOT MODIFY THE CODE BELOW   *    DO NOT MODIFY THE CODE BELOW   *    DO NOT MODIFY THE CODE BELOW    *    DO NOT MODIFY THE CODE BELOW
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%                                  DO NOT MODIFY THE CODE BELOW   *    DO NOT MODIFY THE CODE BELOW   *    DO NOT MODIFY THE CODE BELOW    *    DO NOT MODIFY THE CODE BELOW
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%                                  DO NOT MODIFY THE CODE BELOW   *    DO NOT MODIFY THE CODE BELOW   *    DO NOT MODIFY THE CODE BELOW    *    DO NOT MODIFY THE CODE BELOW
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::





% General settings
NUMSIM = input('\nNumber of trajectories to be simulated: ');
[n1,n2]=rat(NUMSIM);
if(~isequal(n2,1) || isempty(NUMSIM) || NUMSIM <= 0)
    error('The number of trajectories NUMSIM must be a positive integer');
end

if(strcmp(LOADDATA,'N'))
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
   fprintf('\nChoose the integration stepsize (should be << T - T0):');
   h  = input('\nh: ');
   if(h<=0 || isempty(h))
       error('The stepsize must be a positive number') 
   end 
   if(h>(T-T0))
       error('Choose a smaller stepsize')
   end
elseif (strcmp(LOADDATA,'Y'))
       T0 = min(TIME);
       T = max(TIME);
       fprintf('\nChoose the integration stepsize (should be << %d):', min(diff(TIME(diff(TIME)>0))));
       h  = input('\nh: ');
       if(h > min(diff(TIME(diff(TIME)>0))))
          error('Choose a smaller stepsize')
       end
       if(h<=0 || isempty(h))
          error('The stepsize must be a positive number') 
       end    
end


%:::::: the desired discretization for [T0,T] :::::::::::::::::::::
OWNTIME = T0:h:T;    
if(OWNTIME(end)~=T)
   OWNTIME(end)=T;
end
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

if(strcmp(SDETYPE,'Ito'))
   fprintf('\nChoose an integration method:');
   fprintf('\nMethods for Ito SDE: Euler-Maruyama (EM), Milstein (Mil);');
   INTEGRATOR = upper(input('\n''EM'' or ''Mil''?: ','s'));
   if(strcmp(INTEGRATOR,'EM')==0 && strcmp(INTEGRATOR,'MIL')==0)
       error('You must specify ''EM'' or ''Mil''.');
   end
else
   INTEGRATOR = 'MIL';
end

if (strcmp(LOADDATA,'N') && strcmp(PARESTIMATE,'Y'))
    NTIMEOBS  = input('\nNumber of observation time-points: '); 
    [n1,n2]=rat(NTIMEOBS);
    if(~isequal(n2,1) || isempty(NTIMEOBS) || NTIMEOBS <=0)
       error('The number of observation time-points must be a positive integer');
    end
    delta = (T-T0)/(NTIMEOBS-1);
    TIME = T0:delta:T;   % create NTIMEOBS equally spaced observation-times into [T0,T] (TIME should be such that length(TIME)<<length(OWNTIME))
    if(TIME(end)~=T)
       TIME(end)=T;
    end
    % Now replicate the values in TIME depending on the NUMDEPVARS value (useful for future versions of SDE_Toolbox)
    TIME = TIME(ones(1,NUMDEPVARS),:);
    TIME = TIME(:).';
    VRBL = [1:NUMDEPVARS];
    % Now replicate the values in VRBL depending on the TIME value (useful for future versions of SDE_Toolbox)
    VRBL = repmat(VRBL,1,length(unique(TIME)));
end
                     

sde_setup_output = struct('bigtheta',bigtheta,'parbase',PARBASE,'parmask',PARMASK,'problem',PROBLEM,'sdetype',SDETYPE,'numdepvars',NUMDEPVARS,'numsim',NUMSIM,'model',MODEL,'owntime',OWNTIME,'time',TIME,'vrbl',VRBL,'integrator',INTEGRATOR);   
    