function newpot = maxsumpot(pot,variables,partialorder,varargin)
%MAXSUMPOT Maximise or Sum a potential over variables
% newpot = maxsumpot(pot,variables,partialorder)
% max or sum a potential over variables, depending on the partialorder
% Inputs:
% pot : a potential
% variables : the variables to be eliminated
% partialorder: a structure.  If partialorder{i}.sum exists, the variables in the partialorder{i} are summed over
% -- otherwise maxed.  The ordering is such that partialorder{end} contains first variables to be eliminated.
% See also absorptionID.m and demoDecAsia.m

% get the elimination order according to the partial ordering:
if isempty(varargin); sumoverthese=1; else  sumoverthese=varargin{1}; end
if ~sumoverthese; 
    vars=potvariables(pot); variables=setdiff(vars,variables); 
end
tmpvariables=variables;
var=[]; dosum=[];
for partcount=length(partialorder):-1:1
    if isfield(partialorder{partcount},'sum')
        c = find(ismember(tmpvariables,partialorder{partcount}.sum));
        dosum=[dosum ones(1,length(c))];
    end
    if isfield(partialorder{partcount},'max')
        c = find(ismember(tmpvariables,partialorder{partcount}.max));
        dosum=[dosum zeros(1,length(c))];
    end
    var=[var tmpvariables(c)];
    tmpvariables=setdiff(tmpvariables,var);
    if isempty(tmpvariables); break; end
end
newpot=pot;
for i=1:length(var)
    v=var(i);
    if dosum(i)
        newpot=sumpot(newpot,v);
    else
        newpot=maxpot(newpot,v);
    end
end
