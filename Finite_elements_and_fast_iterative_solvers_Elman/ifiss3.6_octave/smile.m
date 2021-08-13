%SMILE generates driven cavity error estimate
%IFISS scriptfile: DJS; 20 October 2013.
%Copyright (c) 2013 D.J. Silvester, H.C. Elman, A. Ramage
%
fprintf('Some error estimates are surprising ...\n')
batchmode('Slogo');
load batchrun
nalogoplot(error_tot,mv,xy,x,y);
shg