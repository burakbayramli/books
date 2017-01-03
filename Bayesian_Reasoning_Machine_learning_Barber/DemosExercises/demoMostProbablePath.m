function demoMostProbablePath
%DEMOMOSTPROBABLEPATH demo of finding the most probable path on a Markov chain
A=zeros(9);
A(8,1)=1;A(2,1)=1;A(4,2)=1;A(3,2)=1;A(6,2)=1;A(5,2)=1;A(7,2)=1;A(7,9)=1;A(9,8)=1;
draw_layout(A',{'1','2','3','4','5','6','7','8','9'},ones(9,1));
p=A./repmat(sum(A),size(A,1),1);
a=1; % start state
b=7; % end state

disp('most probable path based on random walk:');
[optpath logprob]=mostprobablepath(log(p),a,b)

disp('shortest path, assuming equal edge weights:')
[optpath logprob]=mostprobablepath(log(A),a,b)

disp('Most Probable path : Solve using Max-Product on a Factor Graph with absorbing state:')
pabsorb=p;
pabsorb(:,b)=zeros(9,1); pabsorb(b,b)=1;

pot(1).variables=1;
tmp = zeros(9,1); tmp(a)=1;
pot(1).table=tmp;
for t=2:9
    pot(t).variables=[t t-1]; pot(t).table=pabsorb;
end
AFG = FactorGraph(pot);
[maxstate maxval mess]=maxprodFG(pot,AFG,[]);
maxstate
log(maxval)