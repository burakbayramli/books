%nm1p21b: Bessel_ftn
clear, clf
beta=0:.05:15; K=15;
tic
for i=1:length(beta) %Integration
   J151(i)=quad('Bessel_ftn',0,pi,[],0,beta(i),K)/pi;
end
toc
tic, J152=Jkb(K,beta); toc  %Recursive Computation
discrepancy=norm(J151-J152)
