function [pot meanKLdiv]=IPF(edist,opts)
%IPF Iterated Proportional Fitting (using efficient junction tree)
% [pot meanKLdiv]=IPF(edist,opts)
% edist is a reference (or empirical distribution)
% opts.outerloop : maximum number of loops over the cliques
% opts.innerloop : maximum number of loops over potentials in each clique
% opts.tol : convergence criterion (change in mean KL over cliques)
% opts.pot : intial potentials (if not present then potentials initiliased to 1)
% opts.plotprogress=1 : display progress
if isfield(opts,'pot')
    pot=opts.pot;
else
    for p=1:length(edist)
        pot(p)=edist(p);
        pot(p).table=ones(size(pot(p).table));
    end
end
if opts.plotprogress; fprintf(1,'Computing the junction tree...\n'); end
[jtpot jtsep infostruct]=jtree(pot);
C=length(jtpot); % number of cliques
[jtpot jtsep pottoJTclique]=jtassignpot(pot,infostruct);
if opts.plotprogress; fprintf(1,'Updating the potentials:\n'); end
meanKLdiv_old=10e100;
for loop=1:opts.outerloop
    % compute the KL divergence between the marginal tables and the empirical marginals:
    infostruct.ForwardOnly=0;
    jtpotKL=absorption(jtpot,jtsep, infostruct);
    for p=1:length(pot)
        mpot=sumpot(jtpotKL(whichpot(jtpotKL,pot(p).variables,1)),pot(p).variables,0);
        kl(p)=KLdiv(edist(p),mpot);
    end
    meanKLdiv=abs(mean(kl));
    if opts.plotprogress
        fprintf(1,'mean KL divergence =%g\n',meanKLdiv);
        eloop(loop)=meanKLdiv; plot(eloop,'-o'); title('mean KL divergence'); %drawnow
    end
    if meanKLdiv_old-meanKLdiv<opts.tol; break;
    else
        meanKLdiv_old=meanKLdiv;
    end
    infostruct.ForwardOnly=1;
    for c=1:C % loop over the cliques
        if opts.plotprogress
            fprintf(1,'Outer loop %d : updating clique %d of %d cliques\n',loop,c,C);
        end
        [tree, elimseq, schedule]=istree(infostruct.cliquetree,c);
        infostruct.EliminationSchedule=schedule;
        [jtpotc jtsepc]=absorption(jtpot,jtsep, infostruct);
        boundarypot=multpots(jtsepc(nonzerovalue(infostruct.sepind(c,:))));
        cliques=find(pottoJTclique==c);
        for innerloop=1:opts.innerloop % use IPF in each clique
            for cc=1:length(cliques)
                update=cliques(cc);
                rest=setdiff(cliques,update);
                pot(update)=divpots(edist(update),sumpot(multpots([pot(rest) boundarypot]),pot(update).variables,0));
            end
        end
        if ~isempty(cliques);jtpot(c)=multpots(pot(cliques)); end % only need to update JT potentials in this clique
    end
end