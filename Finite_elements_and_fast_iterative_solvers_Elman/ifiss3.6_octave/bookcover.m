%BOOKCOVER generates ESW2 book cover picture
%IFISS scriptfile: DJS; 27 September 2013.
%Copyright (c) 2013 D.J. Silvester, H.C. Elman, A. Ramage
%
fprintf('Generating ESW2 book cover ...\n')
batchmode('NScover');
load batchrun
bookcoverplot(qmethod,xns,By,Bx,A,xy,xyp,x,y,bound);