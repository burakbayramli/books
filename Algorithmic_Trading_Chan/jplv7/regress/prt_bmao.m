function prt_bmas(results,vnames,fid)
% PURPOSE: print results from sar_gcbma, sem_gcbma functions
% -------------------------------------------------
% usage: prt_bmas(results,vnames)
% where results is a structure returned by sar_gbma
%       vnames is a vector of variable names

[nmodels,nvar] = size(results.models);
if nargin < 3
fid = 1;
end;

mprob = results.mprob(end-nmodels+1:end,1);
vprob = results.vprob;

fprintf(fid,'OLS BMA Model information \n');
fprintf(fid,'# of unique models found = %10d \n',results.munique);
fprintf(fid,'# of MCMC draws = %10d \n',results.ndraw);

if (nvar < 10 & nargin == 1)
pflag = 0;
else % the user has supplied variable names
     % or the # of variables is large
pflag = 1;
end;

switch pflag;

case {0} % we have only 10 variables so we print in this format

for i=1:nmodels
    m = ['model ' num2str(i)];
    fprintf(fid,'%8s',m);
    for j=1:nvar
    fprintf(fid,'%5d ',results.models(i,j));
    end;
fprintf(fid,'%8.4f \n',mprob(i,1));
end;
m = ['probs  '];
    fprintf(fid,'%8s',m);
    for j=1:nvar
        fprintf(fid,'%5.3f ',vprob(1,j));
    end;
    fprintf(fid,'\n');
   
case {1} % we have a large # of variables so we print differently
 rnames = [];
 for i=3:nvar+2;
 rnames = strvcat(rnames,vnames(i,:)); 
 end;

cnames = [];
for i=1:nmodels;
 cnames = strvcat(cnames,['model' num2str(i)]);
end;
in.cnames = cnames;
 
fprintf(fid,'%16s','Variables');
for i=1:nmodels
fprintf(fid,'%6s',['m' num2str(i)]);
end;
fprintf(fid,'%8s','Probs');
fprintf(fid,'\n');

for i=1:nvar
fprintf(fid,'%16s',rnames(i,:));
for j=1:nmodels;
fprintf(fid,'%6d',results.models(j,i));
end;

fprintf(fid,'%12.6f',vprob(1,i));
fprintf(fid,'\n');
end;

fprintf(fid,'%16s',' ');
fprintf(fid,'%s',' ');
for i=1:nmodels
fprintf(fid,'%5.3f ',mprob(i,1));
end;
fprintf(fid,'\n');

otherwise
error('prt_bma: problem with switch statement');

end;



