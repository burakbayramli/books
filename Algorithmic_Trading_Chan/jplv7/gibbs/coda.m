function result = coda(draws,vnames,info,fid)
% PURPOSE: MCMC convergence diagnostics, modeled after Splus coda
% ------------------------------------------------------
% USAGE:              coda(draws,vnames,info,fid)
%        or: result = coda(draws)
% where: draws = a matrix of MCMC draws (ndraws x nvars)
%       vnames = (optional) string vector of variable names (nvar x 1)
%         info = (optional) structure setting input values
%       info.q = Raftery quantile           (default = 0.025) 
%       info.r = Raftery level of precision (default = 0.01)
%       info.s = Raferty probability for r  (default = 0.950)
%       info.p1 = 1st % of sample for Geweke chi-sqr test (default = 0.2)
%       info.p2 = 2nd % of sample for Geweke chi-sqr test (default = 0.5)
%           fid = file id for printing to a file, e.g. fid = fopen('cout','w');
% ------------------------------------------------------
% NOTES: you may supply only some of the info-structure arguments
%        the remaining ones will take on default values
% ------------------------------------------------------ 
% RETURNS: output to command window if nargout = 0
%          autocorrelation estimates
%          Rafterty-Lewis MCMC diagnostics
%          Geweke NSE, RNE estimates
%          Geweke chi-sqr prob on means from info.p1 vs info.p2
%          a results structure if nargout = 1
%          result.ndraw    = # of draws
%          result.nvar     = # of variables
%          result.p1       = p1 input argument (or default)
%          result.p2       = p2 input argument (or default)
%          result.q        = Raftery q-value
%          result.r        = Raftery r-value
%          result.s        = Raftery s-value
%          result(i).pmean = posterior mean for variable i
%          result(i).pstd  = posterior std deviation
%          result(i).nse   = nse assuming no serial correlation for variable i
%          result(i).rne   = rne assuming no serial correlation for variable i
%          result(i).nse1  = nse using 4% autocovariance tapered estimate
%          result(i).rne1  = rne using 4% autocovariance taper
%          result(i).nse2  = nse using 8% autocovariance taper
%          result(i).rne2  = rne using 8% autocovariance taper
%          result(i).nse3  = nse using 15% autocovariance taper
%          result(i).rne3  = rne using 15% autocovariance taper
%          result(i).nburn = number of draws required for burn-in
%          result(i).nprec = number of draws required to achieve r precision
%          result(i).kthin = skip parameter for 1st-order Markov chain
%          result(i).irl   = I-statistic from Raftery and Lewis (1992)
%          result(i).kind  = skip parameter sufficient to get independence chain
%          result(i).nmin  = # draws if the chain is white noise
%          result(i).n     = nburn + nprec
%          result(i).auto1 = autocorrelation at lag 1
%          result(i).auto5 = autocorrelation at lag 5
%          result(i).auto10= autocorrelation at lag 10
%          result(i).auto50= autocorrelation at lag 50
%
% -------------------------------------------------------
% SEE ALSO: gmoment, apm, raftery
% -------------------------------------------------------
% REFERENCES: Geweke (1992), `Evaluating the accuracy of sampling-based
% approaches to the calculation of posterior moments', in J.O. Berger,
% J.M. Bernardo, A.P. Dawid, and A.F.M. Smith (eds.) Proceedings of
% the Fourth Valencia International Meeting on Bayesian Statistics,
% pp. 169-194, Oxford University Press
% Also: `Using simulation methods for Bayesian econometric models: 
% Inference, development and communication', at: www.econ.umn.edu/~bacc
% Best, N.G., M.K. Cowles, and S.K. Vines (1995)  CODA: Manual
% version 0.30. Biostatistics Unit, Cambridge U.K. http://www.mrc-bsu.cam.ac.uk
% -----------------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

num_draws = length(draws);
if num_draws < 500
error('coda: at least 500 draws are required');
end;


if nargout == 1 % don't print, return a structure
    pflag = 1;
else
    pflag = 0; % print to the command window
end;

if nargin == 4
    if ~isstruct(info)
        error('coda: must supply options as a structure');
    end;
nflag = 0;
 [vsize junk] = size(vnames); % user may supply a blank vnames argument
   if vsize > 0
   nflag = 1; % we have variable names         
   end;
fields = fieldnames(info);
nf = length(fields);
q = 0.025; r = 0.01; s = 0.95;
p1 = 0.2; p2 = 0.5;
for i=1:nf
    if strcmp(fields{i},'q')
        q = info.q;
    elseif strcmp(fields{i},'r')
        r = info.r;
    elseif strcmp(fields{i},'s')
        s = info.s;
    elseif strcmp(fields{i},'p1')
        p1 = info.p1;
    elseif strcmp(fields{i},'p2');
        p2 = info.p2;
    end;
end;

elseif nargin == 3
    if ~isstruct(info)
        error('coda: must supply options as a structure');
    end;
nflag = 0;
 [vsize junk] = size(vnames); % user may supply a blank vnames argument
   if vsize > 0
   nflag = 1; % we have variable names         
   end;
fields = fieldnames(info);
nf = length(fields);
q = 0.025; r = 0.01; s = 0.95;
p1 = 0.2; p2 = 0.5;
fid = 1;
for i=1:nf
    if strcmp(fields{i},'q')
        q = info.q;
    elseif strcmp(fields{i},'r')
        r = info.r;
    elseif strcmp(fields{i},'s')
        s = info.s;
    elseif strcmp(fields{i},'p1')
        p1 = info.p1;
    elseif strcmp(fields{i},'p2');
        p2 = info.p2;
    end;
end;

elseif nargin == 2
nflag = 1; % we have variable names
q = 0.025; r = 0.01; s = 0.95; % set default values
p1 = 0.2; p2 = 0.5;
fid = 1;

elseif nargin == 1
 nflag = 0; % no variable names
q = 0.025; r = 0.01; s = 0.95; % set default values
p1 = 0.2; p2 = 0.5;
fid = 1;
    
else
    error('Wrong # of arguments to coda');
end;

result.q = q;
result.r = r;
result.s = s;

[ndraw nvar] = size(draws);

if nflag == 0 % no variable names make some up
Vname = [];
  for i=1:nvar
    Vname{i} = str2mat(['variable ',num2str(i)]);
  end;

elseif (nflag == 1) % the user supplied variable names
Vname = [];
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar
 error('Wrong # of variable names in coda -- check vnames argument');
 end;
 nmax = min(nsize,16); % truncate vnames to 16-characters
 for i=1:nvar
 Vname{i} = vnames(i,1:nmax);
 end;
end; % end of nflag issue

% =======> do SACF diagnostics
nlag = 50;
aout = zeros(50,nvar);
for i=1:nvar;
aout(:,i) = sacf(draws(:,i),nlag,1);
end;

% pull out sacf's at 1,5,10,50
aprt = zeros(nvar,4);
aprt(:,1) = aout(1,:)';
aprt(:,2) = aout(5,:)';
aprt(:,3) = aout(10,:)';
aprt(:,4) = aout(50,:)';

% ========> do Raftery-Lewis diagnostics
rafout =  raftery(draws,q,r,s);
rout = zeros(nvar,5);
for i=1:nvar;
  rout(i,1) = rafout(i).kthin;
  rout(i,2) = rafout(i).nburn;
  rout(i,3) = rafout(i).n;
  rout(i,4) = rafout(i).nmin;
  rout(i,5) = rafout(i).irl;
end;

% =========> do Geweke diagnostics
geweke = momentg(draws);

% =========> split sample into 1st p1 percent and last p2 percent
%            and run Geweke chi-squared test
result(1).p1 = p1;
result(1).p2 = p2;
nobs1 = round(p1*ndraw);
nobs2 = round(p2*ndraw);
draws1 = draws(1:nobs1,:);
draws2 = trimr(draws,nobs2,0);

res1 = momentg(draws1);
res2 = momentg(draws2);

resapm = apm(res1,res2);

if pflag == 0 % print results to command window

% =======> print SACF diagnostics    
fprintf(1,'MCMC CONVERGENCE diagnostics \n');
fprintf(1,'Based on sample size = %10d \n',ndraw);
fprintf(1,'Autocorrelations within each parameter chain \n');


vstring = 'Variable';
lstring1 = 'Lag 1';
lstring2 = 'Lag 5';
lstring3 = 'Lag 10';
lstring4 = 'Lag 50';

cnames = strvcat(lstring1,lstring2,lstring3,lstring4);
rnames = vstring;
for i=1:nvar
rnames = strvcat(rnames,Vname{i});
end;
in.fmt = '%12.3f';
in.fid = fid;
in.cnames = cnames;
in.rnames = rnames;

mprint(aprt,in);

% print results with vnames 
fprintf(fid,'Raftery-Lewis Diagnostics for each parameter chain \n');
fprintf(fid,'(q=%6.4f, r=%8.6f, s=%8.6f)\n',q,r,s);
cstring1 = 'Thin';
cstring2 = 'Burn';
cstring3 = 'Total(N)';
cstring4 = '(Nmin)';
cstring5 = 'I-stat';

cnames = strvcat(cstring1,cstring2,cstring3,cstring4,cstring5);
in2.fmt = strvcat('%10d','%10d','%10d','%10d','%10.3f');
in2.cnames = cnames;
in2.fid = fid;
in2.rnames = rnames;

mprint(rout,in2);

% =========> print Geweke diagnostics

fprintf(fid,'Geweke Diagnostics for each parameter chain \n');
cs1 = 'Mean';
cs2 = 'std dev';
cs3 = 'NSE iid';
cs4 = 'RNE iid';

cnames = strvcat(cs1,cs2,cs3,cs4);
in3.fmt = '%12.6f';
gout = zeros(nvar,4);
for i=1:nvar
    gout(i,:) = [geweke(i).pmean geweke(i).pstd geweke(i).nse geweke(i).rne];
end;
in3.cnames = cnames;
in3.rnames = rnames;
in3.fid = fid;

mprint(gout,in3);

cs1 = 'NSE 4% ';
cs2 = 'RNE 4% ';
cs3 = 'NSE 8% ';
cs4 = 'RNE 8% ';
cs5 = 'NSE 15%';
cs6 = 'RNE 15%';

cnames = strvcat(cs1,cs2,cs3,cs4,cs5,cs6);
gout2 = zeros(nvar,6);
for i=1:nvar
    gout2(i,:) = [geweke(i).nse1 geweke(i).rne1 geweke(i).nse2 geweke(i).rne2 ...
                 geweke(i).nse3 geweke(i).rne3];
end;
in4.cnames = cnames;
in4.fid = fid;
in4.fmt = '%12.6f';
in4.rnames = rnames;
mprint(gout2,in4);

% =========> print Geweke chi-squared tests

c = '%';
% print results with vnames 
fprintf(1,'Geweke Chi-squared test for each parameter chain \n');
fprintf(1,'First %2.0f%s versus Last %2.0f%s of the sample \n',100*p1,c,100*p2,c);
clear in;
in.cnames = strvcat('Mean','N.S.E.','Chi-sq Prob');
in.rnames = strvcat('NSE estimate','i.i.d.','4% taper','8% taper','15% taper');
in.fmt = '%12.6f';
in.fid = fid;
for i=1:nvar
  fprintf(1,'Variable %16s\n', strjust(Vname{i},'right'));
gout3 = zeros(4,3);
 for k=1:4
    gout3(k,1) = resapm(i).pmean(k);
    gout3(k,2) = resapm(i).nse(k);
    gout3(k,3) = resapm(i).prob(k);
 end;
 
mprint(gout3,in);
end;
    
end; % end of if pflag == 0

if pflag == 1 % return results structure


result(1).nvar = nvar;
result(1).meth = 'coda';
result(1).ndraw = ndraw;

for i=1:nvar;
  result(i).kthin = rafout(i).kthin;
  result(i).nburn = rafout(i).nburn;
  result(i).n = rafout(i).n;
  result(i).nmin = rafout(i).nmin;
  result(i).irl = rafout(i).irl;
end;

for i=1:nvar
  for j=1:4;
    if j == 1
    result(i).auto1 = aprt(i,j);
    elseif j == 2
    result(i).auto5 = aprt(i,j);
    elseif j == 3
    result(i).auto10 = aprt(i,j);
    elseif j == 4
    result(i).auto50 = aprt(i,j);
    end;
  end;
end;

for i=1:nvar
  result(i).nse1 = geweke(i).nse1;
  result(i).rne1 = geweke(i).rne1;
  result(i).nse2 = geweke(i).nse2;
  result(i).rne2 = geweke(i).rne2;
  result(i).nse3 = geweke(i).nse3;
  result(i).rne3 = geweke(i).rne3;
end;

  for i=1:nvar
  result(i).pmean = geweke(i).pmean;
  result(i).pstd  = geweke(i).pstd;
  result(i).nse   = geweke(i).nse;
  result(i).rne   = geweke(i).rne;
  end;

end; % end of if pflag == 1
