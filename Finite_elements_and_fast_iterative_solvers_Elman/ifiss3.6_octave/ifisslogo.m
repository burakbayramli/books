%IFISSLOGO generates IFISS logo
%IFISS scriptfile: DJS; 8 January 2011. 
%Copyright (c) 2011 D.J. Silvester, H.C. Elman, A. Ramage 
%
fprintf('Generating IFISS logo ..\n')
batchmode('CD_logo');
load batchrun
logoplot(xsupg,xy,x,y);