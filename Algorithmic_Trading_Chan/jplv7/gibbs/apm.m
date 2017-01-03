function result = apm(results1,results2)
% PURPOSE: computes Geweke's chi-squared test for two sets of MCMC sample draws
% ------------------------------------------------
% USAGE: result = apm(results1,results2)
% where: results1 = a structure returned by momentg
%        results2 = a structure returned by momentg
% ------------------------------------------------
% RETURNS: a structure:
%             results.meth  = 'apm'  
%             results.ndraw = ndraw1+ndraw2
%             results.nvar  = # of variables
%             results.p1    = ndraw1/(ndraw1+ndraw2)
%             results.p2    = ndraw2/(ndraw1+ndraw2)
%      results(i).pmean(k)  = posterior mean for variable i
%                            for k = nse, nse1,nse2,nse3
%      results(i).nse(k)    = nse for variable i
%                             for k = nse, nse1,nse2,nse3
%      results(i).prob(k)   = chi-sq test prob for variable i
%                             for k = nse, nse1,nse2,nse3
% ------------------------------------------------
% SEE ALSO: coda(), prt()
% ------------------------------------------------
% REFERENCES: Geweke (1992), `Evaluating the accuracy of sampling-based
% approaches to the calculation of posterior moments', in J.O. Berger,
% J.M. Bernardo, A.P. Dawid, and A.F.M. Smith (eds.) Proceedings of
% the Fourth Valencia International Meeting on Bayesian Statistics,
% pp. 169-194, Oxford University Press
% Also: `Using simulation methods for Bayesian econometric models: 
% Inference, development and communication', at: www.econ.umn.edu/~bacc
% -----------------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu
 
% NOTE: this code draws heavily on MATLAB programs written by
% Siddartha Chib available at: www.econ.umn.edu/~bacc
% I have repackaged it to make it easier to use.

if ~isstruct(results1)
error('apm: requires a structure from momentg as input');
elseif ~isstruct(results2)
error('apm: requires a structure from momentg as input');
end;

nvar  = results1(1).nvar;
nvar2 = results2(1).nvar;
if nvar ~= nvar2; 
error('apm: structure arguments have different # of variables');
end;

ndraw1 = results1(1).ndraw;
ndraw2 = results2(1).ndraw;
result.p1 = ndraw1/(ndraw1+ndraw2);
result.p2 = ndraw2/(ndraw1+ndraw2);
result.ndraw = ndraw1+ndraw2;

result.meth = 'apm';
result.nvar = nvar;

ng = nvar;
nf = 2;

% pull out information
   for i=1:nvar;
   j=1;
   g(j,i) = results1(i).pmean;
   sdnum1(j,i) = results1(i).nse;
   sdnum2(j,i) = results1(i).nse1;
   sdnum3(j,i) = results1(i).nse2;
   sdnum4(j,i) = results1(i).nse3;
   j=2;
   g(j,i) = results2(i).pmean;
   sdnum1(j,i) = results2(i).nse;
   sdnum2(j,i) = results2(i).nse1;
   sdnum3(j,i) = results2(i).nse2;
   sdnum4(j,i) = results2(i).nse3;
   end;

for i=1:nvar;
   for k=1:4;
      eg=0; nse=0; wtsum=0;
      if k==1; sdnum=sdnum1; 
      elseif k==2; sdnum=sdnum2;
      elseif k==3; sdnum=sdnum3; 
      elseif k==4; sdnum=sdnum4;
      end;
      gvar=zeros(nf-1);
      for j=1:nf;
         eg=eg+g(j,i)/(sdnum(j,i))^2;
         wtsum=wtsum+1/(sdnum(j,i))^2;
      end;
      eg=eg/wtsum;
      nse=1/sqrt(wtsum);
      for j=1:nf-2;
         gvar(j,j)=(sdnum(j,i))^2+(sdnum(j+1,i))^2;
         gvar(j,j+1)=-(sdnum(j+1,i))^2;
         gvar(j+1,j)=gvar(j,j+1);
      end;
      gvar(nf-1,nf-1)=(sdnum(nf-1,i))^2+(sdnum(nf,i))^2;
      ginv=inv(gvar);
      g1=g(1:nf-1,i); g2=g(2:nf,i);
      cstat=(g2-g1)'*ginv*(g2-g1); 
      df=nf-1;
      p = 1-chis_prb(cstat,df);
      result(i).pmean(k) = eg;
      result(i).nse(k) = nse;
      result(i).prob(k) = p;
   end;
end;

