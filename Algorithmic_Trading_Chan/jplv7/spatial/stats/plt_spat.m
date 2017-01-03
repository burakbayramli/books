function plt_spat(results,vnames)
% PURPOSE: Plots output using spatial regression results structures
%---------------------------------------------------
% USAGE: plt_spat(results,vnames)
% Where: results = a structure returned by a spatial regression 
%        vnames  = an optional vector of variable names
%                  e.g. vnames = strvcat('y','constant','x1');
%--------------------------------------------------- 
%  RETURNS: nothing, just plots the spatial regression results
% --------------------------------------------------
% NOTE: user must supply pause commands, none are in plt_spat function
%       e.g. plt_spat(results);
%            pause;
%            plt_spat(results2);
% --------------------------------------------------
% SEE ALSO: prt, plt
%---------------------------------------------------   

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if ~isstruct(results)
 error('plt_spat requires structure argument');
end;

nobs = results.nobs;

switch results.meth

% plot actual vs predicted and residuals
case {'sac','sar','far','sem'}
tt=1:nobs;
clf;
subplot(2,1,1), plot(tt,results.y,'-',tt,results.yhat,'--');
title([upper(results.meth), '   Actual vs. Predicted']);
legend('Actual','Predicted');

subplot(2,1,2), plot(tt,results.resid)
title('Residuals');

case {'sar_g','sart_g','sar_gc','sart_gc'}
y = results.y;
yhat = results.yhat;
resid = y - yhat;

tt=1:nobs;
clf;
subplot(2,2,1), plot(tt,results.y,'-',tt,yhat,'--');
legend('Actual','Predicted');
if strcmp(results.meth,'sar_g');
 title(['SAR heteroscedastic Gibbs','   Actual vs. Predicted']);
elseif strcmp(results.meth,'sar_gc');
 title(['SAR heteroscedastic Gibbs','   Actual vs. Predicted']);
elseif strcmp(results.meth,'sart_g');
 title(['SAR Tobit Gibbs','   Actual vs. Predicted']);
end;
subplot(2,2,2), plot(tt,resid);
title('Residuals');
subplot(2,2,3), plot(tt,results.vmean);
title('Mean of V_i draws');
subplot(2,2,4), pltdens(results.pdraw);
title('Posterior Density for rho');

case {'sem_g','semt_g','sem_gc','semt_gc'}
y = results.y;
yhat = results.yhat;
resid = y - yhat;

tt=1:nobs;
clf;
subplot(2,2,1), plot(tt,results.y,'-',tt,yhat,'--');
legend('Actual','Predicted');
if strcmp(results.meth,'sem_g');
 title(['SEM Gibbs','   Actual vs. Predicted']);
else
 title(['SEM Tobit Gibbs','   Actual vs. Predicted']);
end;
subplot(2,2,2), plot(tt,resid);
title('Residuals');
subplot(2,2,3), plot(tt,results.vmean);
title('Mean of V_i draws');
subplot(2,2,4), pltdens(results.pdraw,0.1);
title('Posterior Density for rho');


case {'semp_g','semp_gc'}

y = results.y;
yhat = results.yhat;
resid = y - yhat;

tt=1:nobs;
clf;
subplot(2,2,1), plot(tt,results.y,'-',tt,yhat,'--');
legend('Actual','Predicted');
axis([1 nobs 0 1.2]);
title(['SEM Probit Gibbs', '   Actual vs. Predicted']);
subplot(2,2,2), plot(tt,resid);
axis auto;
title('Residuals');
subplot(2,2,3), plot(tt,results.vmean);
title('Mean of V_i draws');
subplot(2,2,4), pltdens(results.pdraw,0.1);
title('Posterior Density for lambda');

case {'sac_g','sact_g'}

y = results.y;
yhat = results.yhat;
resid = y - yhat;

tt=1:nobs;
clf;
subplot(3,1,1), plot(tt,results.y,'-',tt,yhat,'--');
legend('Actual','Predicted');
if strcmp(results.meth,'sac_g');
title(['SAC Gibbs', '   Actual vs. Predicted']);
else
title(['SAC Tobit Gibbs', '   Actual vs. Predicted']);
end; 
subplot(3,1,2), plot(tt,resid);
title('Residuals');
subplot(3,1,3), plot(tt,results.vmean);
title('Mean of V_i draws');
pause;
clf;
subplot(2,1,1), pltdens(results.pdraw,0.1);
xlabel('Posterior Density for rho');
subplot(2,1,2), pltdens(results.ldraw,0.1);
xlabel('Posterior Density for lambda');

case {'sacp_g'}
y = results.y;
yhat = results.yhat;
resid = y - yhat;

tt=1:nobs;
clf;
subplot(3,1,1), plot(tt,results.y,'-',tt,yhat,'--');
legend('Actual','Predicted');
axis([1 nobs 0 1.2]);
title(['SAC Probit Gibbs', '   Actual vs. Predicted']);
subplot(3,1,2), plot(tt,resid);
axis auto;
title('Residuals');
subplot(3,1,3), plot(tt,results.vmean);
title('Mean of V_i draws');
pause;
clf;
subplot(2,1,1), pltdens(results.pdraw,0.1);
xlabel('Posterior Density for rho');
subplot(2,1,2), pltdens(results.ldraw,0.1);
xlabel('Posterior Density for lambda');


case {'sdm'}
y = results.y;
yhat = results.yhat;
resid = y - yhat;

tt=1:nobs;
clf;
subplot(2,1,1), plot(tt,results.y,'-',tt,yhat,'--');
legend('Actual','Predicted');
 title(['SDM model','   Actual vs. Predicted']);
subplot(2,1,2), plot(tt,resid);
title('Residuals');

case {'sdm_g','sdmt_g','sdm_gc','sdmt_gc'}
y = results.y;
yhat = results.yhat;
resid = y - yhat;

tt=1:nobs;
clf;
subplot(2,2,1), plot(tt,results.y,'-',tt,yhat,'--');
legend('Actual','Predicted');
if strcmp(results.meth,'sdm_g');
 title(['SDM Gibbs','   Actual vs. Predicted']);
else
 title(['SDM Tobit Gibbs','   Actual vs. Predicted']);
end;
subplot(2,2,2), plot(tt,resid);
title('Residuals');
subplot(2,2,3), plot(tt,results.vmean);
title('Mean of V_i draws');
subplot(2,2,4), pltdens(results.pdraw,0.1);
title('Posterior Density for rho');


case {'sdmp_g','sdmp_gc'}

y = results.y;
yhat = results.yhat;
resid = y - yhat;

tt=1:nobs;
clf;
subplot(2,2,1), plot(tt,results.y,'-',tt,yhat,'--');
legend('Actual','Predicted');
axis([1 nobs 0 1.2]);
title(['SDM Probit Gibbs', '   Actual vs. Predicted']);
subplot(2,2,2), plot(tt,resid);
axis auto;
title('Residuals');
subplot(2,2,3), plot(tt,results.vmean);
title('Mean of V_i draws');
subplot(2,2,4), pltdens(results.pdraw,0.1);
title('Posterior Density for lambda');


% here we have to construct predicted using draws
case {'far_g','far_gc'}

y = results.y;
yhat = results.yhat;
resid = y - yhat;

tt=1:nobs;
clf;
subplot(311), plot(tt,results.y,'-',tt,yhat,'--');
legend('Actual','Predicted');
title(['FAR Gibbs', '   Actual vs. Predicted']);
subplot(312), plot(tt,resid);
title('Residuals');
subplot(313), pltdens(results.pdraw);
title('Posterior density for rho');


case {'casetti','darp','bcasetti'}
subplot(1,1,1);

if results.exp == 0 % x-y expansion
 beta = results.beta;
 if strcmp(results.meth,'bcasetti') == 0
 b0 = results.b0;
 else
 b0 = mean(results.b0draw);
 b0 = b0';
 end;
 [nobs nvar] = size(beta);
 nvar = nvar/2;
 betax = matmul(b0(2:2+nvar-1,1)',ones(nobs,nvar)) + beta(:,1:nvar);
 betay = matmul(b0(2:2+nvar-1,1)',ones(nobs,nvar)) + beta(:,nvar+1:2*nvar);

 xc = results.xc;
 [xcs xci] = sort(xc);
 pltx = zeros(nobs,nvar);
 pltx = betax(xci,:);
 tt=1:nobs;

  if nargin < 2 % case of no vnames
   for j=1:nvar;
   plot(tt,pltx(:,j));
   title(['X-Direction Expanded Coefficient ',num2str(j)]);
   xlabel('sorted by x-direction, left=smallest x');
   ylabel('coefficient');
   pause;
   end; % end of j-loop over expanded-variables
  else % we have user-supplied vnames
   for j=1:nvar;
   plot(tt,pltx(:,j));
   title(['X-Direction Expanded Coefficient ',vnames(j+2,:)]);
   xlabel('sorted by x-direction, left=smallest x');
   ylabel('coefficient');
   pause;
   end; % end of j-loop over expanded-variables
  end; 

 yc = results.yc;
 [ycs yci] = sort(yc);
 pltx = betay(yci,:);

  if nargin < 2 % case of no vnames
   for j=1:nvar;
   plot(tt,pltx(:,j));
   title(['Y-Direction Expanded Coefficient ',num2str(j)]);
   xlabel('sorted by y-direction, left=smallest y');
   ylabel('coefficient');
   pause;
   end; % end of j-loop over expanded-variables
  else
   for j=1:nvar;
   plot(tt,pltx(:,j));
   title(['Y-Direction Expanded Coefficient ',vnames(j+2,:)]);
   xlabel('sorted by y-direction, left=smallest y');
   ylabel('coefficient');
   pause;
   end; % end of j-loop over expanded-variables
  end;

elseif results.exp == 1 % distance expansion
dvec = results.dist;

 [ycs yci] = sort(dvec);
 beta = results.beta;
 if strcmp(results.meth,'bcasetti') == 0
 b0 = results.b0;
 else
 b0 = mean(results.b0draw);
 b0 = b0';
 end;
 [nobs nvar] = size(beta);
 pltx = matmul(b0(2:2+nvar-1,1)',ones(nobs,nvar)) + beta(yci,:);
 tt=1:nobs;
  if nargin < 2 % case of no vnames
   for j=1:nvar;
   plot(tt,pltx(:,j));
   title(['Distance Expanded Coefficient ',num2str(j)]);
   xlabel('sorted by distance from ctr=left');
   ylabel('coefficient');
   pause;
   end; % end of j-loop over expanded-variables
  else
   for j=1:nvar;
   plot(tt,pltx(:,j));
   title(['Distance Expanded Coefficient ',vnames(j+2,:)]);
   xlabel('sorted by distance from ctr=left');
   ylabel('coefficient');
   pause;
   end; % end of j-loop over expanded-variables
  end;

end; % end of if results.exp ==0,1

% Now plot actual vs predicted 
tt=1:nobs;
clf;
subplot(2,1,1), plot(tt,results.y,'-',tt,results.yhat,'--');
legend('Actual','Predicted');
title([upper(results.meth), '   Actual vs. Predicted']);
subplot(2,1,2), plot(tt,results.resid)
title('Residuals');

case {'gwr'}
subplot(1,1,1);

 beta = results.beta;
 [nobs nvar] = size(beta);
 tt=1:nobs;
  if nargin < 2 % case of no vnames
   for j=1:nvar;
   plot(tt,beta(:,j));
   title(['GWR coefficient ',num2str(j)]);
   xlabel('Observations');
   ylabel('coefficient');
   pause;
   end; % end of j-loop over expanded-variables
  else
   for j=1:nvar;
   plot(tt,beta(:,j));
   title(['GWR coefficient ',vnames(j+1,:)]);
   xlabel('Observations');
   ylabel('coefficient');
   pause;
   end; % end of j-loop over expanded-variables
  end;

% Now plot actual vs predicted 
tt=1:nobs;
clf;
subplot(211), plot(tt,results.y,'-',tt,results.yhat,'--');
legend('Actual','Predicted');
title([upper(results.meth), '   Actual vs. Predicted']);
subplot(212), plot(tt,results.resid)
title('Residuals');

case {'bgwr','bgwrv'}

% find posterior means
tmp1 = mean(results.bdraw);
beta = squeeze(tmp1);
y = results.y;
yhat = zeros(nobs,1);
for i=1:nobs;
yhat(i,1) =  results.x(i,:)*beta(i,:)';
end;
e = y - yhat; 
subplot(1,1,1);

 [nobs nvar] = size(beta);
 tt=1:nobs;
  if nargin < 2 % case of no vnames
   for j=1:nvar;
   plot(tt,beta(:,j));
   title(['BGWR coefficient ',num2str(j)]);
   xlabel('Observations');
   ylabel('coefficient');
   pause;
   end; % end of j-loop over expanded-variables
  else
   for j=1:nvar;
   plot(tt,beta(:,j));
   title(['BGWR coefficient ',vnames(j+1,:)]);
   xlabel('Observations');
   ylabel('coefficient');
   pause;
   end; % end of j-loop over expanded-variables
  end;

% Now plot actual vs predicted 
tt=1:nobs;
clf;
subplot(2,1,1), plot(tt,y,'-',tt,yhat,'--');
title([upper(results.meth), '   Actual vs. Predicted']);
legend('Actual','Predicted');
subplot(2,1,2), plot(tt,e)
title('Residuals');
pause;

% now plot sige and vi estimates
tt=1:nobs;
clf;
subplot(2,1,1), plot(tt,results.vmean);
title([upper(results.meth), '   V_i estimates']);
subplot(2,1,2), plot(tt,results.smean)
title('sigma estimates');
pause;

case {'semip_g','semip_gc'}

y = results.y;
yhat = stdn_cdf(results.yhat);
theta = results.amean;

tt=1:nobs;
clf;
subplot(2,2,1), plot(tt,results.y,'-',tt,yhat,'--');
legend('Actual','Predicted');
axis([1 nobs 0 1.2]);
title(['Spatial Probit with individual effects', '   Actual vs. Predicted']);
subplot(2,2,2), plot(tt,theta);
axis auto;
title('Individual effects');
subplot(2,2,3), plot(tt,results.vmean);
title('Mean of V_i draws');
subplot(2,2,4), pltdens(results.pdraw);
title('Posterior Density for rho');


otherwise
error('method not recognized by plt_spat');
end;





