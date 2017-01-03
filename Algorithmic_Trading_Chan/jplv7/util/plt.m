function plt(results,vnames,arg3)
% PURPOSE: Plots results structures returned by most functions
%          by calling the appropriate plotting function
%---------------------------------------------------
% USAGE: plt(results,vnames)
% Where: results = a structure returned by an econometric function
%        vnames  = an optional vector of variable names
%                 e.g. vnames = ['y    ',
%                                'x1   ',  NOTE: fixed width
%                                'x2   ',        like all MATLAB
%                                'cterm'];       strings
% --------------------------------------------------
% NOTES: this is simply a wrapper function that calls another function
% --------------------------------------------------        
% RETURNS: nothing, just plots the results
% --------------------------------------------------
% SEE ALSO: prt()
%---------------------------------------------------   

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

% error checking on inputs
if ~isstruct(results)
error('plt: requires a structure input');
elseif nargin == 3
arg = 3;          
elseif nargin == 2
arg = 2;
elseif nargin == 1
arg = 1;
else
error('Wrong # of inputs to plt');
end;

method = results(1).meth;

% call appropriate plotting routine
switch method

case {'arma','boxcox','boxcox2','hwhite','lad','logit','mlogit','nwest','ols','olsc',...
      'olsar1','olst','probit','ridge','robust','switch_em','theil','tobit','tsls',...
      'hmarkov_em','hmarkov_em2'} 
     % call plt_reg
     if arg == 1
     plt_reg(results);
     elseif arg == 2
     plt_reg(results,vnames);
     end;

case {'thsls','sur'} 
     % call plt_eqs
     if arg == 1
     plt_eqs(results);
     elseif arg == 2
     plt_eqs(results,vnames);
     end;

case {'vare','bvar','rvar','ecm','becm','recm'} 
     % call plt_var
     if arg == 1
     plt_var(results);
     elseif arg == 2
     plt_var(results,vnames);
     end;

case {'bvar_g','rvar_g','becm_g','recm_g'} 
     % call plt_varg
     if arg == 1
     plt_varg(results);
     elseif arg == 2
     plt_varg(results,vnames);
     end;

case {'ar_g','ols_g', 'bma_g', 'tobit_g','probit_g'}
     % call plt_gibbs
     if arg == 1
     plt_gibbs(results);
     elseif arg == 2
     plt_gibbs(results,vnames);
     end;

case {'sar','sar_g','sart_g','sarp_g','far','far_g','sem','sem_g','semt_g', ...
 'semp_g','bcasetti','casetti','darp','gwr','bgwr','bgwrv','sac','sac_g', ...
 'sact_g','sacp_g','sdm','sdm_g','sdmt_g','sdmp_g','gwr_logit','gwr_probit'}     
     % call plt_spat
     if arg == 1
     plt_spat(results);
     elseif arg == 2
     plt_spat(results,vnames);
     end;

case {'tvp','tvp_garch','tvp_markov'}
     % call plt_tvp
     if arg == 1
     plt_tvp(results);
     elseif arg == 2
     plt_tvp(results,vnames);
     end;

case {'garch'}
     % call plt_garch
     if arg == 1
     plt_garch(results);
     elseif arg == 2
     plt_garch(results,vnames);
     end;

case {'hamilton','hamilton_g'}
     % call plt_ham
     if arg == 1
     plt_ham(results);
     elseif arg == 2
     plt_ham(results,vnames);
     end;


case {'dfbeta'}
     % call plt_dfb and plt_dff
     if arg == 1
     plt_dfb(results);
     pause;
     plt_dff(results);
     elseif arg == 2
     plt_dfb(results,vnames);
     pause;
     plt_dff(results);
     end;

case {'cusums'}
     plt_cus(results);
  
case {'fturns'}
     if arg == 1
  plt_turns(results); 
  elseif arg == 2
     plt_turns(results,vnames); % vnames is really cstruct
  elseif arg == 3
     plt_turns(results,vnames,arg3);
  end;
  
otherwise
error('results structure not known by plt function');

end;

