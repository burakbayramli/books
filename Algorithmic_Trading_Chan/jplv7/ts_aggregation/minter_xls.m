function y = minter_xls(Y,x,z,ta,s,f,type,d,flax1,flax2);
% PURPOSE: Interface via Excel Link for multivariate temporal disaggregation
% -----------------------------------------------------------------------
% SYNTAX: y = minter_xls(Y,x,z,ta,s,f,type,d,flax1,flax2);
% -----------------------------------------------------------------------
% INPUT
%           SELECTION OF THE METHOD
%
% Common parameters:
%        ta: type of disaggregation
%            ta=1 ---> sum (flow)
%            ta=2 ---> average (index)
%            ta=3 ---> last element (stock) ---> interpolation
%            ta=4 ---> first element (stock) ---> interpolation
%        s: number of high frequency data points for each low frequency data point
%            s= 4 ---> annual to quarterly
%            s=12 ---> annual to monthly
%            s= 3 ---> quarterly to monthly
%
% Specific parameters:
%
% ==> Rossi:
%        type: preliminary univariate disaggregation
%            = 1 ---> Fernandez
%            = 2 ---> Chow-Lin
%            = 3 ---> Litterman
%
% ==> Denton:
%        d: objective function to be minimized: volatility of ...
%            d=0 ---> levels
%            d=1 ---> first differences
%            d=2 ---> second differences
%
% ==> di Fonzo:
%        type: model for the innovations
%            = 0 ---> white noise
%            = 1 ---> random walk
% 
% INPUT DATA:
%         Y : NxM 
%         x : nxMM
%         z : nxnz
% 
%                
% -----------------------------------------------------------------------
% OUTPUT: y: nxi
%       i=M  brief --> only temporally disaggregated series (all procedures)
%       i=2M detailed --> temporally disaggregated series + standard errors of estimates
%                  Available for Di Fonzo.
% -----------------------------------------------------------------------
% LIBRARY: rossi, denton, difonzo

% written by:
% Ana Abad & Enrique M. Quilis
% Instituto Nacional de Estadistica
% Paseo de la Castellana, 183
% 28046 - Madrid (SPAIN)

% -----------------------------------------------------------------------
% SELECTION OF THE METHOD

switch flax1
case 1
   % Rossi
   res = rossi(Y,x,z,ta,s,type);
case 2
   % Denton
   res = denton(Y,x,z,ta,s,d);
case 3
   % di Fonzo
   res = difonzo(Y,x,z,ta,s,type,f);
end
  
% -----------------------------------------------------------------------
% SELECTION OF OUTPUT

 switch flax2
     case 0 
         % Brief output
         y = res.y;
     case 1
         % Normal output
         switch res.meth
             case {'Multivariate Denton','Multivariate Rossi'}
                 y = res.y;           
             case {'Multivariate di Fonzo'}
                  y   = [res.y res.d_y];           
          end
 end
 