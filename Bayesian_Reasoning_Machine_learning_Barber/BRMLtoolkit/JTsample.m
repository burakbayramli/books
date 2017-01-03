function samples=JTsample(jtpot,infostruct,nsamples)
%JTSAMPLE Draw samples from a consistent Junction Tree
% samples=JTsample(jtpot,infostruct,nsamples)
% infostruct is a structure with information about the Junction Tree:
% infostruct.cliquetree : connectivity structure of Junction Tree cliques

% Get an elimination schedule such that each JT potential has at most 1 parent:
N=length(potvariables(jtpot));
[dum schedule]=singleparenttree(infostruct.cliquetree); 
samples=zeros(N,nsamples); vars=1:N;
for s=1:nsamples
    for elim=schedule
        ind=samples(:,s)>0;
        currentsamplestate=samples(ind,s);currentsamplevars=vars(ind);
        sampvars=setdiff(jtpot(elim).variables,currentsamplevars);
        samples(sampvars,s)=potsample(normpot(setpot(jtpot(elim),currentsamplevars,currentsamplestate)),1);
    end
end