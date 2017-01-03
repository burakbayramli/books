function Alearn=learnBayesNet(data,ancestors,nstates,maxNparents,U)
%LEARNBAYESNET Learn a Bayes Network from data, given an ancestral order
% Alearn=learnBayesNet(data,ancestors,nstates,maxNparents,U)
% data is a data matrix with a datum per column
% ancestors are the variables in ancestral order (oldest first)
% nstates are the number of states of the variables 1,2,3,...
% maxNparents(v) is the maximum number of parents of variable v
vars=fliplr(ancestors); bestparents=cell(1,length(vars));
for vind=1:length(vars)
    v=vars(vind);
    bestscore=-10e100;
    for nparents=0:maxNparents(v)
        parentset=mynchoosek(vars(vind+1:end),nparents);
        for p=1:size(parentset,1)
            Pa=parentset(p,:);
            bd = BDscore(data(v,:),data(Pa,:),nstates(v),nstates(Pa),U);
            if bd>bestscore
                bestscore=bd; bestparents{v}=Pa;
            end
        end
    end
end
V=length(vars);Alearn=zeros(V,V);
for v=1:V; Alearn(bestparents{v},v)=1; end