function demoGibbsSample
%DEMOGIBBSSAMPLE demo of Gibbs sampline
pot(1).variables=[1 2];
pot(1).table=rand([2 2]).^2;

pot(2).variables=[2 3];
pot(2).table=rand([2 2]).^2;

pot(3).variables=[3 4];
pot(3).table=rand([2 2]).^2;

pot(4).variables=[1 4];
pot(4).table=rand([2 2]).^2;
figure; drawNet(markov(pot));

fprintf(1,'unstructured sampling:\n')
C{1}=[2 3 4]; C{2}=[1 3 4]; C{3}=[2 1 4]; C{4}=[1 2 3];% conditioning sets
init=[1 1 1 1]; D=4;
samples = GibbsSample(pot,init,C,1000,0);
for d=1:D
    t=table(condpot(multpots(pot),d));
    fprintf(1,'variable %d : exact mean %f | sample mean %f\n',d,t(2),mean(samples(d,:)-1))
end

fprintf(1,'structured sampling:\n')
C{1}=[1]; C{2}=[2 3 4]; C{3}=[2]; C{2}=[1 3 4]; % conditioning sets
init=[1 1 1 1];
samples = GibbsSample(pot,init,C,1000);
for d=1:D
    t=table(condpot(multpots(pot),d));
    fprintf(1,'variable %d : exact mean %f | sample mean %f\n',d,t(2),mean(samples(d,:)-1))
end