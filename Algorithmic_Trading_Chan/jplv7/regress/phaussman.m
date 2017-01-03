function phaussman(result1, result2,fid)
% PURPOSE: prints haussman test, use for testing the specification of the fixed or
%			  random effects model.	
%  --------------------------------------------------------------------------------
% USAGE: phaussman(results1, results2);
%  where: results1 = a structure returned by pfixed()
%         results1 = a structure returned by prandom()
%----------------------------------------------------------------------------------               

%Written by:
% Carlos Alberto Castro
% National Planning Department
% Bogota, Colombia
% Email: ccastro@dnp.gov.co 

if nargin < 2; error('wrong # of arguments to phaussman'); end;
if nargin > 3; error('wrong # of arguments to phaussman'); end;


if ~isstruct(result1)
 error('phaussman requires a Fixed effects panel model results structure');
end;

if ~isstruct(result2)
 error('phaussman requires a Random effects panel model results structure');
end;

if nargin == 2; 
   fid = 1; 
end;


% pull out results from Fixed effects model
bfe  = result1.beta;
xmatf = result1.xmat;
sigef = result1.sige;

covf= sigef*(inv(xmatf'*xmatf));

% pull out results from Random effects model
bre  = result2.beta;
xmatr = result2.xmat;
siger = result2.sige;

covr= siger*(inv(xmatr'*xmatr));

%haussman test

[k junk]= size(bre);

bdif = (bfe-bre(1:k-1,:));
mdif = (covf-covr(1:k-1,1:k-1));
m    =  bdif'*(inv(mdif))*bdif;
p    =  1-chis_prb(m,k-1);

fprintf(fid,'\n ***** Haussman Test ******* \n');

fprintf(fid,'\n Ho: Random Effects %9.4f \n');
fprintf(fid,'\n Ha: Fixed Effects  %9.4f \n');
fprintf(fid,'\n Statistic      = %9.4f \n',m);
fprintf(fid,'\n Probability	= %9.4f \n',p);
fprintf(fid,'\n ***************************\n');







