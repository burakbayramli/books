function samples=GibbsSample(pot,init,ConditioningSets,nsamples,varargin)
%GIBBSSAMPLE Gibbs sampling for a set of potentials
%samples=GibbsSample(pot,init,ConditioningSets,nsamples,<use junction tree>)
% pot is a structure of potentials
% init is the vector of initial states
% ConditioningSets{i} contains the variables conditioned on in step i
% nsamples is the number of samples
% if the optional argument toggles between using a junction tree for
% efficient sampling (but contains overhead in computing the junction tree)
% The default is to use the junction tree
% see also demoGibbsSample.m
C=ConditioningSets;
samples(:,1)=init(:);
D=length(init);
if nargin==5
    doJT=varargin{1};
else
    doJT=1;
end
s=2;
while s<nsamples+1
    for c=1:length(C)
        v=setdiff(1:D,C{c});
        if doJT % use Junction tree
            samples(v,s)=potsample(squeezepots(setpot(pot,C{c},samples(C{c},s-1))),1);
        else
            samples(v,s)=potsample(multpots(squeezepots(setpot(pot,C{c},samples(C{c},s-1)))),1);
        end
        samples(C{c},s)=samples(C{c},s-1);
        s=s+1;
        if s>nsamples
            break;
        end
    end
end