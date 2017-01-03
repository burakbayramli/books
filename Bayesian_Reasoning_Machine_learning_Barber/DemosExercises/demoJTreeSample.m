function demoJTreeSample
%DEMOJTREESAMPLE  Sample from a Junction Tree
load chestclinic
[jtpot jtsep infostruct]=jtree(pot); % setup the Junction Tree
jtpot=absorption(jtpot,jtsep,infostruct); % do full round of absorption
figure; drawNet(dag(pot),variable); title('Belief Net');
figure; drawJTree(jtpot,infostruct,variable); title('Junction Tree (separators not shown)');
nsamples=1000;
samples=JTsample(jtpot,infostruct,nsamples);
m=mean(samples-1,2); % from the samples the prob of being in state 2
fprintf(1,'Probability that a variable is in state 2 based on %d samples:\n',nsamples);
for i=1:8
    marg=table(sumpot(multpots(pot),i,0));
    fprintf(1,'variable %d : sample versus true value = %1.3f %1.3f\n',i,m(i),marg(2));
end