function demoPCoracle
%DEMOPCORACLE demo of stucture learning using the PC algorithm
figure
[x y z w t]=assign(1:5);
pot(x).variable=x; pot(y).variables=y; pot(z).variables=[z x y]; pot(w).variables=[w y]; pot(t).variables=[t z w]; 
varinf(x).name='x'; varinf(y).name='y'; varinf(z).name='z'; varinf(w).name='w'; varinf(t).name='t';

vars=1:5; V=length(vars);
A = dag(pot); [xcord,ycord]=drawNet(A,varinf); title('oracle')
[G,S]=PCskeletonOracle(A);
% Partially orient the skeleton (using the Unmarried Collider rule only):
GpDAG=PCorient(G,S);
figure; draw_layout(GpDAG,field2cell(varinf,'name'),zeros(1,V),xcord,ycord);
title('Partially ordered DAG')