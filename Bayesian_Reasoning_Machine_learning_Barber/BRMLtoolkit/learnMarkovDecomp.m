function pot=learnMarkovDecomp(edist,root,opts)
%LEARNMARKOVDECOMP learn the tables for a decomposable Markov net
% pot=learnMarkovDecomp(edist,root,opts)
% edist is a reference (or empirical distribution)
% opts.plotprogress=1 : display progress
if opts.plotprogress; fprintf(1,'Computing the junction tree...\n'); end
[jtpot jtsep infostruct]=jtree(edist);
C=length(jtpot); % number of cliques
for c=1:C
    jtpot(c)=edist(whichpot(edist,jtpot(c).variables,1));
end
for s=1:length(jtsep)
    jtsep(s)=sumpot(edist(whichpot(edist,jtsep(s).variables,1)),jtsep(s).variables,0);
end
spTree=singleparenttree(infostruct.cliquetree,root);
[tree elimset schedule]=istree(spTree);
schedule=fliplr(flipud(schedule));
if opts.plotprogress; fprintf(1,'Updating the potentials:\n'); end
pot(root)=jtpot(root);
for c=2:size(schedule,1)
    if opts.plotprogress; fprintf(1,'Updating clique %d (of %d)\n',c,C); end
    pa=schedule(c,1); ch=schedule(c,2);
    sep=infostruct.sepind(pa,ch);
    if sep>0
        pot(ch)=divpots(jtpot(ch),jtsep(sep));
    end
end