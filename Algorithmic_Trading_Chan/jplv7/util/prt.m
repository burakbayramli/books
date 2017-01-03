function prt(results,vnames,fid)
% PURPOSE: Prints results structures returned by most functions
%          by calling the appropriate printing function
%---------------------------------------------------
% USAGE: prt(results,vnames,fid)
% Where: results = a results structure returned an econometric function
%        vnames  = an optional vector of variable names
%        fid     = file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%---------------------------------------------------               
%                 e.g. vnames = ['y    ',
%                                'x1   ',  NOTE: fixed width
%                                'x2   ',        like all MATLAB
%                                'cterm'];       strings
%                 e.g. fid = fopen('ols.out','wr');
% --------------------------------------------------
% NOTES: you may use prt(results,[],fid) to print
%        output to a file with no vnames
%        this is simply a wrapper function that calls another function
% --------------------------------------------------        
% RETURNS:
%        nothing, just prints the regression results
% --------------------------------------------------
% SEE ALSO: plt()
%---------------------------------------------------   

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

% error checking on inputs
if ~isstruct(results)
error('prt: requires a structure input');
elseif nargin == 3
arg = 0;
 [vsize junk] = size(vnames); % user may supply a blank argument
   if vsize > 0
   arg = 3;          
   end;
elseif nargin == 2
arg = 2;
elseif nargin == 1
arg = 1;
else
error('Wrong # of inputs to prt');
end;

method = results(1).meth;

% call appropriate printing routine
switch method

case {'arma','boxcox','boxcox2','hwhite','lad','logit','mlogit','nwest','ols','olsc',...
      'olsar1','olst','probit','ridge','robust','theil','tobit','tsls'} 
     % call prt_reg
     if arg == 1
     prt_reg(results);
     elseif arg == 2
     prt_reg(results,vnames);
     elseif arg == 3
     prt_reg(results,vnames,fid);
     else
     prt_reg(results,[],fid);
     end;

case {'switch_em','hmarkov_em'}
     % call prt_swm
     if arg == 1
     prt_swm(results);
     elseif arg == 2
     prt_swm(results,vnames);
     elseif arg == 3
     prt_swm(results,vnames,fid);
     else
     prt_swm(results,[],fid);
     end;

case {'psem','semsfe','semtfe','semstfe'} % <=================== spatial panel error models
     % call prt_sp
     if arg == 1
     prt_sp(results);
     elseif arg == 2
     prt_sp(results,vnames);
     elseif arg == 3
     prt_sp(results,vnames,fid);
     else
     prt_sp(results,[],fid);
     end;

case {'psem','semsfe','semtfe','semstfe','psar','sarsfe','sartfe','sarstfe','sarsre','semsre'} % <=================== spatial panel autoregressive models
     % call prt_spnew
     if arg == 1
     prt_spnew(results);
     elseif arg == 2
     prt_spnew(results,vnames);
     elseif arg == 3
     prt_spnew(results,vnames,fid);
     else
     prt_spnew(results,[],fid);
     end;

case {'thsls','sur'} 
     % call prt_eqs
     if arg == 1
     prt_eqs(results);
     elseif arg == 2
     prt_eqs(results,vnames);
     elseif arg == 3
     prt_eqs(results,vnames,fid);
     else
     prt_eqs(results,[],fid);
     end;

case {'sem_gmm','sem2_gmm','sac_gmm','sar_gmm'} 
     % call prt_gmm
     if arg == 1
     prt_gmm(results);
     elseif arg == 2
     prt_gmm(results,vnames);
     elseif arg == 3
     prt_gmm(results,vnames,fid);
     else
     prt_gmm(results,[],fid);
     end;


case {'ar','vare','bvar','rvar','ecm','becm','recm'} 
     % call prt_var
     if arg == 1
     prt_var(results);
     elseif arg == 2
     prt_var(results,vnames);
     elseif arg == 3
     prt_var(results,vnames,fid);
     else
     prt_var(results,[],fid);
     end;

case {'bvar_g','rvar_g','becm_g','recm_g'} 
     % call prt_varg
     if arg == 1
     prt_varg(results);
     elseif arg == 2
     prt_varg(results,vnames);
     elseif arg == 3
     prt_varg(results,vnames,fid);
     else
     prt_varg(results,[],fid);
     end;

case {'johansen','adf','cadf','phillips'}
     % call prt_coint
     if arg == 1
     prt_coint(results);
     elseif arg == 2
     prt_coint(results,vnames);
     elseif arg == 3
     prt_coint(results,vnames,fid);
     else
     prt_coint(results,[],fid);
     end;

case {'coda','raftery','apm','momentg'}
     % call prt_coda
     if arg == 1
     prt_coda(results);
     elseif arg == 2
     prt_coda(results,vnames);
     elseif arg == 3
     prt_coda(results,vnames,fid);
     else
     prt_coda(results,[],fid);
     end;
     
case {'ar_g','ols_g', 'ols_gc', 'bma_g', 'tobit_g','probit_g','probit_gm'}
     % call prt_gibbs
     if arg == 1
     prt_gibbs(results);
     elseif arg == 2
     prt_gibbs(results,vnames);
     elseif arg == 3
     prt_gibbs(results,vnames,fid);
     else
     prt_gibbs(results,[],fid);
     end;

case {'sar','sar_g','sart_g','sarp_g','sar_c','sar_gv','sarp_gc','sar_gbma'}
     % call prt_sar
     if arg == 1
     prt_sar(results);
     elseif arg == 2
     prt_sar(results,vnames);
     elseif arg == 3
     prt_sar(results,vnames,fid);
     else
     prt_sar(results,[],fid);
     end;

case {'sem','sem_g','semt_g','semp_g'}
     % call prt_sem
     if arg == 1
     prt_sem(results);
     elseif arg == 2
     prt_sem(results,vnames);
     elseif arg == 3
     prt_sem(results,vnames,fid);
     else
     prt_sem(results,[],fid);
     end;

case {'semip_g','semip_gc','semit_g'}
     % call prt_semip
     if arg == 1
     prt_sem(results);
     elseif arg == 2
     prt_semip(results,vnames);
     elseif arg == 3
     prt_semip(results,vnames,fid);
     else
     prt_semip(results,[],fid);
     end;

case {'sdm','sdm_g','sdmp_g','sdmt_g','sdm_gmm'}
     % call prt_sdm
     if arg == 1
     prt_sdm(results);
     elseif arg == 2
     prt_sdm(results,vnames);
     elseif arg == 3
     prt_sdm(results,vnames,fid);
     else
     prt_sdm(results,[],fid);
     end;

case {'far','far_g','far_gc'}
      % call prt_far
     if arg == 1
     prt_far(results);
     elseif arg == 2
     prt_far(results,vnames);
     elseif arg == 3
     prt_far(results,vnames,fid);
     else
     prt_far(results,[],fid);
     end;
    
case{'sac','sac_g','sacp_g','sact_g'}
      % call prt_sac
     if arg == 1
     prt_sac(results);
     elseif arg == 2
     prt_sac(results,vnames);
     elseif arg == 3
     prt_sac(results,vnames,fid);
     else
     prt_sac(results,[],fid);
     end;
    
case{'moran','lmerror','lratios','walds','lmsar'}
     % call prt_spat
     if arg == 1
     prt_spat(results);
     elseif arg == 2
     prt_spat(results,vnames);
     elseif arg == 3
     prt_spat(results,vnames,fid);
     else
     prt_spat(results,[],fid);
     end;

case {'mess','mess_g','mess_g1','mess_g2','mess_g3','messv_g3','messt_g', ...
'messt_g1','messt_g2','messt_g3','messvt_g3','messp_g','messp_g1','messp_g2', ...
'messp_g3','messpv_g3'}     
     % call prt_mess
     if arg == 1
     prt_mess(results);
     elseif arg == 2
     prt_mess(results,vnames);
     elseif arg == 3
     prt_mess(results,vnames,fid);
     else
     prt_mess(results,[],fid);
     end;

case {'gwr','bgwr','bgwrv','gwr_logit','gwr_probit'}
     % call prt_gwr
     if arg == 1
     prt_gwr(results);
     elseif arg == 2
     prt_gwr(results,vnames);
     elseif arg == 3
     prt_gwr(results,vnames,fid);
     else
     prt_gwr(results,[],fid);
     end; 

case {'casetti','darp','bcasetti'}
     % call prt_cas
     if arg == 1
     prt_cas(results);
     elseif arg == 2
     prt_cas(results,vnames);
     elseif arg == 3
     prt_cas(results,vnames,fid);
     else
     prt_cas(results,[],fid);
     end;  

case {'tvp','tvp_garch','tvp_markov'}
     % call prt_tvp
     if arg == 1
     prt_tvp(results);
     elseif arg == 2
     prt_tvp(results,vnames);
     elseif arg == 3
     prt_tvp(results,vnames,fid);
     else
     prt_tvp(results,[],fid);
     end;  
      
case {'garch'}
     % call prt_garch
     if arg == 1
     prt_garch(results);
     elseif arg == 2
     prt_garch(results,vnames);
     elseif arg == 3
     prt_garch(results,vnames,fid);
     else
     prt_garch(results,[],fid);
     end;  

case {'hamilton','hamilton_g'}
     % call prt_ham
     if arg == 1
     prt_ham(results);
     elseif arg == 2
     prt_ham(results,vnames);
     elseif arg == 3
     prt_ham(results,vnames,fid);
     else
     prt_ham(results,[],fid);
     end;  
     


 case {'felogit'}
     % call prt_felogit
     if arg == 1
     prt_felogit(results);
     elseif arg == 2
     prt_felogit(results,vnames);
     elseif arg == 3
     prt_felogit(results,vnames,fid);
     else
     prt_felogit(results,[],fid);
     end;  
     
  case {'multilogit'}
      arg = 1;
      [vsize junk] = size(vnames); % user may supply a blank argument
      if vsize > 0
      arg = 2;          
      end;
      [vsize junk] = size(cnames); % user may supply a blank argument
      if vsize > 0
      arg = 3;          
      end;
      [vsize junk] = size(fid); % user may supply a blank argument
      if vsize > 0
      arg = 4;          
      end;

     % call prt_multilogit
     if arg == 1
     prt_multilogit(results);
     elseif arg == 2
     prt_multilogit(results,vnames);
     elseif arg == 3
     prt_multilogit(results,vnames,cnames);
     elseif arg == 4
     prt_multilogit(results,vnames,cnames,fid);
     else
     prt_multilogit(results,[],[],fid);
     end;  

 
    
otherwise
error('results structure not known by prt function');

end;

