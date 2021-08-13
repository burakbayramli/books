%CHECKMATRICES   Run sanity check
%   checkmatrices;
%   IFISS scriptfile: DJS; 14 August 2018.
% Copyright (c) 2018 D.J. Silvester, P. Nadukandi
h=1/2
NN = [156, 54, 22*h, -13*h; 54, 156, 13*h, -22*h; 22*h, 13*h, 4*h*h,-3*h*h; -13*h, -22*h, -3*h*h, 4*h*h].*(h/420)
LL=[6,-6,3*h,3*h;-6,6,-3*h,-3*h;3*h,-3*h,2*h*h,h*h;3*h,-3*h,h*h,2*h*h]*2/(h*h*h)
LM=[-36,36,-3*h,-3*h;36,-36,3*h,3*h;-33*h,3*h,-4*h*h,h*h;-3*h,33*h,h*h,-4*h*h]/(30*h)
xId= [1,2,2,1,3,4,4,3,1,2,2,1,3,4,4,3];
yId= [1,1,2,2,1,1,2,2,3,3,4,4,3,3,4,4];
KK=LL(xId,xId).*NN(yId,yId) + LM(xId,xId)'.*LM(yId,yId) + LM(xId,xId).*LM(yId,yId)' + NN(xId,xId).*LL(yId,yId);
MM= NN(xId,xId).*NN(yId,yId);
ff = [36;36;36;36;6*h;-6*h;-6*h;6*h;6*h;6*h;-6*h;-6*h;h*h;-h*h;h*h;-h*h]*h*h/144;
gohome, cd datafiles
save analyticmatrices h KK MM ff
