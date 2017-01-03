    
function [out1,out2,out3] = M9_sdefile(t,x,flag,bigtheta,SDETYPE,NUMDEPVARS,NUMSIM)

% SDE model definition: drift, diffusion, derivatives and initial conditions.
%
% [out1,out2,out3] = M9_sdefile(t,x,flag,bigtheta,SDETYPE,NUMDEPVARS,NUMSIM)
%
% IN:     t; working value of independent variable (time)
%         x; working value of dependent variable 
%         flag; a switch, with values 'init' or otherwise
%         bigtheta; complete structural parameter vector
%         SDETYPE; the SDE definition: can be 'Ito' or 'Strat' (Stratonovich)
%         NUMDEPVARS; the number of dependent variables, i.e. the SDE dimension
%         NUMSIM; the number of desired simulations for the SDE numerical integration 
% OUT:    out1; in case of flag='init' is just the initial time, otherwise it is the (vector of) SDE drift(s)
%         out2; in case of flag='init' is the initial value of the dependent variables. Otherwise it is the SDE diffusion(s)
%         out3; in case of flag='init' it is nothing. Otherwise it is the SDE's partial derivative(s) of the diffusion term 

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


% Parameters
Xzero1 = bigtheta(1);  
Xzero2 = bigtheta(2);  
alpha1 = bigtheta(3); 
alpha2 = bigtheta(4);
beta11 = bigtheta(5);
beta12 = bigtheta(6);
beta21 = bigtheta(7);
beta22 = bigtheta(8);
sigma1 = bigtheta(9);
sigma2 = bigtheta(10);

if nargin < 3 || isempty(flag)
    
   xsplitted  =  SDE_split_sdeinput(x,NUMDEPVARS);
   
  %::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
  %::::::::::::::::::::::::::::::::::::::::::  DEFINE HERE THE SDE  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  %:::::::::::::::::::::::: (define the initial conditions at the bottom of the page) ::::::::::::::::::::::::::::::::::::::::::::::::::::
  %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  X1 = xsplitted{1}; % e.g. for a thre-dimensional SDE write X1 = xsplitted{1}; X2 = xsplitted{2}; X3 = xsplitted{3};
  X2 = xsplitted{2};

   switch upper(SDETYPE)
   case 'ITO'
       driftX1 = beta11 * alpha1 + beta12 * alpha2 - beta11 * X1 - beta12 * X2;    % the Ito SDE drift w.r.t. X1
       diffusionX1 = sigma1;      % the Ito SDE diffusion w.r.t. X1
       derivativeX1 = 0;         % the diffusion derivative w.r.t. X1
       driftX2 = beta21 * alpha1 + beta22 * alpha2 - beta21 * X1 - beta22 * X2;    % the Ito SDE drift w.r.t. X2
       diffusionX2 = sigma2;      % the Ito SDE diffusion w.r.t. X2
       derivativeX2 = 0;         % the diffusion derivative w.r.t. X2
   case 'STRAT'
       driftX1 = beta11 * alpha1 + beta12 * alpha2 - beta11 * X1 - beta12 * X2;    % the Statonovich SDE drift w.r.t. X1
       diffusionX1 = sigma1;      % the Statonovich SDE diffusion w.r.t. X1
       derivativeX1 = 0;         % the diffusion derivative w.r.t. X1
       driftX2 = beta21 * alpha1 + beta22 * alpha2 - beta21 * X1 - beta22 * X2;    % the Statonovich SDE drift w.r.t. X2
       diffusionX2 = sigma2;      % the Statonovich SDE diffusion w.r.t. X2
       derivativeX2 = 0;         % the diffusion derivative w.r.t. X2
   end
   
    out1 = zeros(1,NUMDEPVARS*NUMSIM);
    out1(1:NUMDEPVARS:end) = driftX1;
    out1(2:NUMDEPVARS:end) = driftX2;
    out2 = zeros(1,NUMDEPVARS*NUMSIM);
    out2(1:NUMDEPVARS:end) = diffusionX1;
    out2(2:NUMDEPVARS:end) = diffusionX2;
    out3 = zeros(1,NUMDEPVARS*NUMSIM);
    out3(1:NUMDEPVARS:end) = derivativeX1;
    out3(2:NUMDEPVARS:end) = derivativeX2;
    
   %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
else
    
    switch(flag)
    case 'init'  
        out1 = t;
        
%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
%::::::::::::::::::::::  DEFINE HERE THE SDE INITAL CONDITIONS  :::::::::::::::::::::::::::::::::
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

        out2 = [Xzero1 Xzero2];   % write here the SDE initial condition(s)
        
%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: :::::::::::::::::::::::::::::::::
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        
        out3 = [];
        
        
    otherwise
        error(['Unknown flag ''' flag '''.']);
    end
end

