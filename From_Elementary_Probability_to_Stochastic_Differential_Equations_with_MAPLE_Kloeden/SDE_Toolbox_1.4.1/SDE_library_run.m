% Main model-library routine: simulation, numerical solution and parameter estimation of Ito and Stratonovich SDEs.
%
% usage: SDE_library_run;

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


fprintf('\n\n:::::::::::::::::::::::::::::::::::::::::::::::::::::::  SDE TOOLBOX  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n\n');
fprintf('                             SIMULATION AND ESTIMATION OF STOCHASTIC DIFFERENTIAL EQUATIONS WITH MATLAB   \n\n');
fprintf('                                                  http://sdetoolbox.sourceforge.net                                                 \n');
fprintf('                                                                                                                                    \n');
fprintf('                                                  Copyright (C) 2007, Umberto Picchini                                              \n');
fprintf('                                                  umberto.picchini@biomatematica.it                                                 \n');
fprintf('                                                  http://www.biomatematica.it/pages/picchini.html                                 \n\n');
fprintf(' This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License.       \n\n');
fprintf(':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::');


% list the available SDE models
SDE_model_description;

PARESTIMATE = upper(input('\n\n\nDo you want to estimate parameters from data? [Y/N]: ','s'));
if(strcmp(PARESTIMATE,'N')==0 && strcmp(PARESTIMATE,'Y')==0)
       error('You must specify ''Y'' or ''N''.');
end

if(strcmp(PARESTIMATE,'Y'))  
    
    %:::: ESTIMATE PARAMETERS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     
     % Load data from file or simulate data by SDE numerical integration?
     LOADDATA = upper(input('\nDo you want to load data from an ASCII file? [Y/N]: ','s'));
     if(strcmp(LOADDATA,'N')==0 && strcmp(LOADDATA,'Y')==0)
        error('You must specify ''Y'' or ''N''.');
     end
     if(strcmp(LOADDATA,'Y'))
        DATAFILE = input('\nWrite the name (case sensitive) of the datafile WITHOUT the .dat extension: ','s'); 
        [XOBS,TIME,VRBL] = SDE_getdata(DATAFILE); % get the data from an ASCII file
        sde_setup_input = struct('loaddata','Y','parestimate','Y','time',TIME,'xobs',XOBS,'vrbl',VRBL);
        sde_setup_output = SDE_library_setup(sde_setup_input); % bigtheta contains the structural SDE model parameters
        bigtheta = sde_setup_output.bigtheta;
        PARBASE = sde_setup_output.parbase;
        PARMASK = sde_setup_output.parmask;
        PROBLEM = sde_setup_output.problem;
        SDETYPE = sde_setup_output.sdetype;
        NUMDEPVARS = sde_setup_output.numdepvars;
        NUMSIM = sde_setup_output.numsim;
        MODEL = sde_setup_output.model;
        OWNTIME = sde_setup_output.owntime;
        VRBL = sde_setup_output.vrbl;
        INTEGRATOR = sde_setup_output.integrator;
     else % Generate one set of data-points for given TRUE parameters 'bigtheta'
        sde_setup_input = struct('loaddata','N','parestimate','Y','time',[],'xobs',[],'vrbl',[]);
        sde_setup_output = SDE_library_setup(sde_setup_input); % bigtheta contains the structural SDE model parameters
        bigtheta = sde_setup_output.bigtheta;
        PARBASE = sde_setup_output.parbase;
        PARMASK = sde_setup_output.parmask;
        PROBLEM = sde_setup_output.problem;
        SDETYPE = sde_setup_output.sdetype;
        NUMDEPVARS = sde_setup_output.numdepvars;
        NUMSIM = sde_setup_output.numsim;
        MODEL = sde_setup_output.model;
        OWNTIME = sde_setup_output.owntime;
        TIME = sde_setup_output.time;
        VRBL = sde_setup_output.vrbl;
        INTEGRATOR = sde_setup_output.integrator;
        numsim=NUMSIM;
        NUMSIM=1;  % generate one data-set for given TRUE parameters 'bigtheta'
        XOBS = SDE_predict(bigtheta,PROBLEM,SDETYPE,OWNTIME,TIME,NUMDEPVARS,NUMSIM,VRBL,INTEGRATOR,0);  % simulated ("observed") data
        NUMSIM=numsim; % go back to original
     end
     
     if(strcmp(SDETYPE,'Ito')==1)
         % Ito SDEs can be estimated either with the parametric and the
         % non-parametric procedures
        PARESTMETHOD = upper(input('\nChoose the parameter estimation method: parametric [PAR, only for Ito SDEs] or non-parametric [NPAR]: ','s'));
     else
         % Stratonovich SDEs can be estimated only with the non-parametric
         % procedure
        PARESTMETHOD = 'NPAR';
        fprintf('\nThe Non-Parametric parameter estimation procedure is automatically employed with Stratonovich SDEs\n');
     end
     if(strcmp(PARESTMETHOD,'PAR')==0 && strcmp(PARESTMETHOD,'NPAR')==0)
         error('You must specify ''PAR'' or ''NPAR''.');
     elseif(strcmp(PARESTMETHOD,'PAR')==1 && strcmp(SDETYPE,'Ito')==0)
             error('The PAR method is only available for Ito SDEs; choose NPAR to estimate Stratonovich SDEs')
     elseif(strcmp(PARESTMETHOD,'PAR')==1 && strcmp(INTEGRATOR,'EM')==0) 
             error('The PAR method can only be used together with the Euler-Maruyama (EM) integration scheme; choose the Milstein integration scheme or use the NPAR estimation method')
     end
     
     if(strcmp(LOADDATA,'N'))
       % Specify an initial guess (starting values) for the parameters to be optimized
       nparfree = sum(PARMASK); % the number of parameters to be optimized
       for i=1:nparfree
           fprintf('\nWrite the initial guess for FREE parameter #%2d (check with PARMASK)',i);
           theta(i) = input('\nStarting value: ');
       end
       bigtheta = SDE_param_unmask(theta,PARMASK,PARBASE);  % retrieve the full array of parameters
     elseif(strcmp(LOADDATA,'Y'))
       % Specify the values for constant parameters and an initial guess (starting values) for the parameters to be optimized
       npar = length(PARMASK); % the total number of parameters 
       for i=NUMDEPVARS+1:npar  % skip the parameter(s) corresponding to initial condition(s) (these are equal to the first observed value(s))
           if(PARMASK(i)==1)
             fprintf('\nWrite the initial guess for FREE parameter #%2d (check with PARMASK)',i);
             bigtheta(i) = input('\nStarting value: ');
           else
             fprintf('\nWrite the value for CONSTANT parameter #%2d (check with PARMASK)',i);
             bigtheta(i) = input('\nValue: '); 
           end
       end 
     end

     PARBASE = bigtheta; % update PARBASE accordingly, useful for SDE_param_unmask.m
     % optimization setup  
     [PARMIN,PARMAX,MYOPT] = SDE_library_optsetup(bigtheta);
     [theta, thetandx, ACTMIN, ACTMAX]  = SDE_param_mask(bigtheta,PARMASK,PARMIN,PARMAX); % theta contains the starting values for the free parameters; ACTMIN (ACTMAX) is the array of the minimum (maximum) allowed values for the free parameters

     % minimization of the negative log-likelihood function using optimization settings specified in SDE_library_optsetup.m: returns the
     % set of the FREE optimized parameters
     fprintf('\nESTIMATING PARAMETERS...');
     if(strcmp(PARESTMETHOD,'PAR'))
        [theta, FVAL, EXITFLAG, OUTPUT] = fminsearchbnd('SDE_PSML',theta,ACTMIN,ACTMAX,MYOPT,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,PARBASE,PARMIN,PARMAX,PARMASK,INTEGRATOR,NUMDEPVARS,0);
     elseif(strcmp(PARESTMETHOD,'NPAR'))
        [theta, FVAL, EXITFLAG, OUTPUT] = fminsearchbnd('SDE_NPSML',theta,ACTMIN,ACTMAX,MYOPT,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,PARBASE,PARMIN,PARMAX,PARMASK,INTEGRATOR,NUMDEPVARS,0);
     end
     
     bigtheta = SDE_param_unmask(theta,PARMASK,PARBASE); % the full set of optimized and fixed parameters
     
     % Display the estimated and constant parameters and compute the free parameters' asymptotic 95% confidence intervals
     if(strcmp(PARESTMETHOD,'PAR'))
         SDE_ParConfInt('SDE_PSML',theta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,PARBASE,PARMIN,PARMAX,PARMASK,INTEGRATOR,NUMDEPVARS,0);
     elseif(strcmp(PARESTMETHOD,'NPAR'))
         SDE_ParConfInt('SDE_NPSML',theta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,PARBASE,PARMIN,PARMAX,PARMASK,INTEGRATOR,NUMDEPVARS,0);
     end
     
     % SDE model numerical integration corresponding to the optimized parameters 'bigtheta'
     xhat = SDE_integrator(bigtheta,PROBLEM,OWNTIME,NUMDEPVARS,NUMSIM,SDETYPE,INTEGRATOR,0);
     % Computes mean, 95% confidence intervals, q1-q3 quartiles and
     % histogram(s) for the numerical approximation corresponding to the optimized
     % parameters
     SDE_graph(bigtheta,xhat,1,PROBLEM,SDETYPE,INTEGRATOR,NUMDEPVARS,OWNTIME,MODEL,NUMSIM,TIME,XOBS,0);
     % Computes descriptive statistics for the numerical approximation corresponding to the optimized
     % parameters
     SDE_stats(bigtheta,xhat,PROBLEM,OWNTIME,NUMDEPVARS,NUMSIM,SDETYPE,INTEGRATOR,0);
     
 else %:::: DO NOT ESTIMATE PARAMETERS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     
      % Load data from file or simulate data by SDE numerical integration?
      LOADDATA = upper(input('\nDo you want to load data from an ASCII file? [Y/N]: ','s'));
      if(strcmp(LOADDATA,'N')==0 && strcmp(LOADDATA,'Y')==0)
         error('You must specify ''Y'' or ''N''.');
      end
      if(strcmp(LOADDATA,'Y'))
        DATAFILE = input('\nWrite the name (case sensitive) of the datafile WITHOUT the .dat extension: ','s'); 
        [XOBS,TIME,VRBL] = SDE_getdata(DATAFILE); % get the data from an ASCII file
        % read and check the keyboard inputs 
        sde_setup_input = struct('loaddata','Y','parestimate','N','time',TIME,'xobs',XOBS,'vrbl',VRBL);
        sde_setup_output = SDE_library_setup(sde_setup_input); % bigtheta contains the structural SDE model parameters
        bigtheta = sde_setup_output.bigtheta;
        PARBASE = sde_setup_output.parbase;
        PARMASK = sde_setup_output.parmask;
        PROBLEM = sde_setup_output.problem;
        SDETYPE = sde_setup_output.sdetype;
        NUMDEPVARS = sde_setup_output.numdepvars;
        NUMSIM = sde_setup_output.numsim;
        MODEL = sde_setup_output.model;
        OWNTIME = sde_setup_output.owntime;
        VRBL = sde_setup_output.vrbl;
        INTEGRATOR = sde_setup_output.integrator;
        % SDE model numerical integration for given parameters 'bigtheta'
        xhat = SDE_integrator(bigtheta,PROBLEM,OWNTIME,NUMDEPVARS,NUMSIM,SDETYPE,INTEGRATOR,0);
        % Computes descriptive statistics
        SDE_stats(bigtheta,xhat,PROBLEM,OWNTIME,NUMDEPVARS,NUMSIM,SDETYPE,INTEGRATOR,0);
        % Computes mean, 95% confidence intervals, q1-q3 quartiles and histogram(s) for the numerical approximation
        SDE_graph(bigtheta,xhat,1,PROBLEM,SDETYPE,INTEGRATOR,NUMDEPVARS,OWNTIME,MODEL,NUMSIM,TIME,XOBS,0);
      elseif (strcmp(LOADDATA,'N'))
        sde_setup_input = struct('loaddata','N','parestimate','N','time',[],'xobs',[],'vrbl',[]);
        sde_setup_output = SDE_library_setup(sde_setup_input); % bigtheta contains the structural SDE model parameters
        bigtheta = sde_setup_output.bigtheta;
        PARBASE = sde_setup_output.parbase;
        PARMASK = sde_setup_output.parmask;
        PROBLEM = sde_setup_output.problem;
        SDETYPE = sde_setup_output.sdetype;
        NUMDEPVARS = sde_setup_output.numdepvars;
        NUMSIM = sde_setup_output.numsim;
        MODEL = sde_setup_output.model;
        OWNTIME = sde_setup_output.owntime;
        TIME = sde_setup_output.time;
        VRBL = sde_setup_output.vrbl;
        INTEGRATOR = sde_setup_output.integrator;
        % SDE model numerical integration for given parameters 'bigtheta'
        xhat = SDE_integrator(bigtheta,PROBLEM,OWNTIME,NUMDEPVARS,NUMSIM,SDETYPE,INTEGRATOR,0);
        % Computes descriptive statistics
        SDE_stats(bigtheta,xhat,PROBLEM,OWNTIME,NUMDEPVARS,NUMSIM,SDETYPE,INTEGRATOR,0);
        % Computes mean, 95% confidence intervals, q1-q3 quartiles and histogram(s) for the numerical approximation
        SDE_graph(bigtheta,xhat,0,PROBLEM,SDETYPE,INTEGRATOR,NUMDEPVARS,OWNTIME,MODEL,NUMSIM,[],[],0);
      end    
end






