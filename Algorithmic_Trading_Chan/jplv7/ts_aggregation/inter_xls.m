function y = inter_xls(Y,x,ta,s,type,ip,d,flax1,flax2,file_name);
% PURPOSE: Interface via Excel Link for univariate temporal disaggregation
% -----------------------------------------------------------------------
% SYNTAX: y = inter_xls(Y,x,ta,s,type,ip,d,flax1,flax2,file_name);
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
% ==> Boot-Feibes-Lisman, Denton:
%        d: objective function to be minimized: volatility of ...
%            d=0 ---> levels
%            d=1 ---> first differences
%            d=2 ---> second differences
%
% ==> Chow-Lin, Litterman, Santos-Cardoso:
%        type: estimation method: 
%            type=0 ---> weighted least squares 
%            type=1 ---> maximum likelihood
% ==> Chow-Lin, Litterman, Santos-Cardoso when innovational parameter is supplied:
%         ip, such as -1 < ip < 1
%
%           INPUT DATA:
% 
% Common:  
%       Y: Nx1 --> Low-frequency time series (to be temporally disaggregated)
% Specific:
%       x: nx1 --> Denton, n=s*N (extrapolation is not feasible)
%       x: nxp --> Fernandez, Chow-Lin, Litterman, Santos-Cardoso
%          p >= 1, n >= s*N (extrapolation is feasible) 
%               
% -----------------------------------------------------------------------
% OUTPUT: y: nxi
%
%       i=1 brief --> only temporally disaggregated series (all procedures)
%       i=5 normal --> temporally disaggregated series, standard errors of estimates, 
%                  one-sigma upper and lower limits and residuals.
%                  Available for Fernandez, Chow-Lin, Litterman, Santos-Cardoso
%       i=5 detailed --> normal + ASCII file with model results (all
%                   procedures). A name for the output file should be supplied.
%
% -----------------------------------------------------------------------
% LIBRARY: bfl, denton_uni, fernandez, chowlin, litterman, ssc, 
% chowlin_fix, litterman_fix, ssc_fix, td_uni_print, td_print

% written by:
% Ana Abad & Enrique M. Quilis
% Instituto Nacional de Estadistica
% Paseo de la Castellana, 183
% 28046 - Madrid (SPAIN)

% -----------------------------------------------------------------------
% SELECTION OF THE METHOD

switch flax1
    case 1
        % Boot-Feibes-Lisman
        res=bfl(Y,ta,d,s);
    case 2
        % Denton
        res=denton_uni(Y,x,ta,d,s);
    case 3
        % Fernandez
         res=fernandez(Y,x,ta,s);
     case 4
         % Chow-Lin
         res=chowlin(Y,x,ta,s,type);
     case 5
         % Litterman
         res=litterman(Y,x,ta,s,type);
     case 6
         % Santos-Cardoso
         res=ssc(Y,x,ta,s,type);
     case 7
         % Chow-Lin, fixed innovational parameter (ip)
         res=chowlin_fix(Y,x,ta,s,type,ip);
     case 8
         % Litterman, fixed innovational parameter (ip)
         res=litterman_fix(Y,x,ta,s,type,ip);
     case 9
         % Santos-Cardoso, fixed innovational parameter (ip)
          res=ssc_fix(Y,x,ta,s,type,ip);
  end
  
% -----------------------------------------------------------------------
% SELECTION OF OUTPUT

 switch flax2
     case 1 
         % Brief output
         y = res.y;
     case 2
         % Normal output
         switch res.meth
             case {'Boot-Feibes-Lisman'}
                 y = [res.y zeros(res.s*res.N,4)];
             case {'Denton'}
                  y = [res.y zeros(res.s*res.N,3) res.u];
             case {'Fernandez','Chow-Lin','Litterman','Santos Silva-Cardoso'}
                 y = [res.y res.y_dt res.y_lo res.y_up res.u];
         end
     case 3
         % Detailed output
         switch res.meth
             case {'Boot-Feibes-Lisman'}
                 y = [res.y zeros(res.s*res.N,4)];
                 tduni_print(res,file_name);
             case {'Denton'}
                  y = [res.y zeros(res.s*res.N,3) res.u];
                  tduni_print(res,file_name);
             case {'Fernandez','Chow-Lin','Litterman','Santos Silva-Cardoso'}
                 y = [res.y res.y_dt res.y_lo res.y_up res.u];
                 td_print(res,file_name,0);                 
         end
 end
           