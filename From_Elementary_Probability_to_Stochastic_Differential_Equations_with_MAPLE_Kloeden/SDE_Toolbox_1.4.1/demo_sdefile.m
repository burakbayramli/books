    
function [out1,out2,out3] = demo_sdefile(t,x,flag,bigtheta)

% SDE model definition: drift, diffusion, derivatives and initial condition.
%
% [out1,out2,out3] = demo_sdefile(t,x,flag,bigtheta)
%
% IN:     t; working value of independent variable (time)
%         x; working value of dependent variable 
%         flag; a switch, with values 'init' or otherwise
%         bigtheta; complete structural parameter vector
%         global SDETYPE; the SDE definition: can be 'Ito' or 'Strat' (Stratonovich)
%         global NUMDEPVARS; the number of dependent variables, i.e. the SDE dimension
%         global NUMSIM; the number of desired simulations for the SDE numerical integration 
% OUT:    out1; in case of flag='init' is just the initial time, otherwise it is the (vector of) SDE drift(s)
%         out2; in case of flag='init' is the initial value of the dependent variables. Otherwise it is the (vector of) SDE diffusion(s)
%         out3; in case of flag='init' it is nothing. Otherwise it is the (vector of) SDE partial derivative(s) of the diffusion term 
%

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

global SDETYPE NUMDEPVARS NUMSIM;


% Parameters
Xzero     = bigtheta(1);
a         = bigtheta(2);


if nargin < 3 || isempty(flag)
    
  xsplitted  =  SDE_split_sdeinput(x,NUMDEPVARS);  
    
  %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
  %::::::::::::::::::::::  DEFINE HERE THE SDE  :::::::::::::::::::::::::::::::::
  %::::: (define the initial conditions at the bottom of the page) ::::::::::::::
  %::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::     
  
  x = xsplitted{1}; % e.g. for a thre-dimensional SDE write X1 = xsplitted{1}; X2 = xsplitted{2}; X3 = xsplitted{3};
   
   switch SDETYPE
   case 'Ito'
       driftX = 1/2 * a^2 * x;  % the Ito SDE drift
       diffusionX = a * x;      % the Ito SDE diffusion
       derivativeX = a;         % the diffusion derivative w.r.t. x
   case 'Strat'
       driftX = 0;              % the Stratonovich SDE drift
       diffusionX = a * x;      % the Stratonovich SDE diffusion
       derivativeX = a;         % the diffusion derivative w.r.t. x
   end
    
   out1 = driftX;           % fill with the drift value(s); 
   out2 = diffusionX;       % fill with the diffusion value(s); 
   out3 = derivativeX;      % fill with the first derivative(s) of the diffusion(s) term(s) with respect to the state variable(s); 
    
   % Example: if you have k dependent variables Y1,...,Yk and the system of SDEs
   % dY = f(t,Y)dt + g(t,Y)dW,    where dY is (k x 1), f is (k x 1), g is a
   % DIAGONAL (k x m) matrix and W is (m x 1)
   % then: out1 = [driftY1,...,driftYk];               
   %       out2 = [diffusionY1,...,diffusionYk];   
   %       out3 = [derivativeY1,...,derivativeYk]; 
   % where derivativeYi is the partial derivative of the diffusion term for
   % the ith equation with respect to the variable Yi (i=1,...,k).
   %:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
   %::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   %::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   
   % Now let MATLAB vectorize the output over the NUMSIM simulations, if
   % necessary
   if(length(out1)==NUMDEPVARS)    
       out1 = out1';
       out1 = out1([1:size(out1,1)]' * ones(1,NUMSIM), :)'; % ugly but faster than out1 = repmat(out1,1,NUMSIM);
    end
    if(length(out2)==NUMDEPVARS)
       out2 = out2';
       out2 = out2([1:size(out2,1)]' * ones(1,NUMSIM), :)';
    end
    if(length(out3)==NUMDEPVARS)
       out3 = out3';
       out3 = out3([1:size(out3,1)]' * ones(1,NUMSIM), :)';
    end
    
else
    
    switch(flag)  
    case 'init'  
        out1 = t;
        
        out2 = Xzero;  % Write here SDE initial conditions
        
        out3 = [];
        
        
    otherwise
        error(['Unknown flag ''' flag '''.']);
    end
end

