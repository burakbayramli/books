function demoLoopyBP
%DEMOLOOPYBP demo of Belief Propagation in a multiply-connected graph
W=3; X=2; Y=4; Z=2; [w x y z]=assign(1:4);
pot(1).variables=[w x]; pot(1).table=(rand([W X]).^2);
pot(2).variables=[x y]; pot(2).table=(rand([X Y]).^2);
pot(3).variables=[y z]; pot(3).table=(rand([Y Z]).^2);
pot(4).variables=[z w]; pot(4).table=(rand([Z W]).^2);

A = FactorGraph(pot); drawFG(A~=0); % Since pot is loopy, A contains -1 entries
opt.tol=10e-5; opt.maxit=10; [marg mess A2]=LoopyBP(pot,opt);

jpot=multpots(pot); fprintf('\nExact and Loopy BP marginals:\n\n')
for i=1:length(potvariables(pot))
    fprintf('%d   Exact     Loopy BP\n',i); disp([table(condpot(jpot,i)) table(marg(i))])
end