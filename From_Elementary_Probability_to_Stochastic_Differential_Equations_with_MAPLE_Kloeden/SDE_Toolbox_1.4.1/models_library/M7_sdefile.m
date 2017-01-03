    
function [out1,out2,out3] = M7_sdefile(t,x,flag,bigtheta,SDETYPE,NUMDEPVARS,NUMSIM)

% SDE model definition: drift, diffusion, derivatives and initial conditions.
%
% [out1,out2,out3] = M7_sdefile(t,x,flag,bigtheta,SDETYPE,NUMDEPVARS,NUMSIM)
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
Xzero = bigtheta(1);  
a     = bigtheta(2); 


if nargin < 3 || isempty(flag)
    
  xsplitted  =  SDE_split_sdeinput(x,NUMDEPVARS);  
    
  %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
  %::::::::::::::::::::::  DEFINE HERE THE SDE  :::::::::::::::::::::::::::::::::
  %::::: (define the initial conditions at the bottom of the page) ::::::::::::::
  %::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::     
  
  x = xsplitted{1}; % e.g. for a thre-dimensional SDE write X1 = xsplitted{1}; X2 = xsplitted{2}; X3 = xsplitted{3}; 
   
   switch upper(SDETYPE)
   case 'ITO'
       driftX = -1/2 * a^2 * x;     % the Ito SDE drift
       diffusionX = a * sqrt(1 - x.^2);      % the Ito SDE diffusion
       derivativeX = -a * x ./ (sqrt(1 - x.^2));         % the diffusion derivative w.r.t. x
   case 'STRAT'
       driftX = 0;     % the Stratonovich SDE drift
       diffusionX = a * sqrt(1 - x.^2);      % the Stratonovich SDE diffusion
       derivativeX = -a * x ./ (sqrt(1 - x.^2));         % the diffusion derivative w.r.t. x
   end
    
   %::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   %::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   %::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   
   out1 = zeros(1,NUMDEPVARS*NUMSIM);
   out1(1:NUMDEPVARS:end) = driftX;        
   out2 = zeros(1,NUMDEPVARS*NUMSIM);
   out2(1:NUMDEPVARS:end) = diffusionX;    
   out3 = zeros(1,NUMDEPVARS*NUMSIM);
   out3(1:NUMDEPVARS:end) = derivativeX;  
    
else

    switch(flag)
    case 'init'  
        out1 = t;
        
%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
%::::::::::::::::::::::  DEFINE HERE THE SDE INITAL CONDITIONS  :::::::::::::::::::::::::::::::::
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  

        out2 = Xzero;   % write here the SDE initial condition(s)
        
%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: :::::::::::::::::::::::::::::::::
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  
        
        out3 = [];
        
        
    otherwise
        error(['Unknown flag ''' flag '''.']);
    end
end

