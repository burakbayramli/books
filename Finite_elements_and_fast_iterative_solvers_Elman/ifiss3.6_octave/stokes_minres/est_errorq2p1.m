function errorest = est_errorq2p1(xmr,eparams)
%EST_ERRORQ2P1 computes energy error estimate for Q2-P1 solution 
%   errorest = est_errorq2p1(xmr,eparams);
%   input
%          xmr        Q2-P1 solution iterate
%      eparams        structure for error estimator:
%          .ae        elementwise Poisson problem matrices
%          .xy        vertex coordinate vector  
%          .mv        element mapping matrix
%      .mbound        element edge boundary matrix 
%   output
%     errorest        energy error estimate
%
%   IFISS function: DJS; 19 May 2010.
% Copyright (c) 2010 D.J. Silvester, Qifeng Liao

%% unpack eparams structure
ae=eparams.ae; xy=eparams.xy; mv=eparams.mv; mbound=eparams.mbound;
neumannb=eparams.neumannb; eex=eparams.eex; hx=eparams.hx; hy=eparams.hy;
[error_x,error_y] = algpost_q2p1(xmr,ae,xy,mv,mbound,neumannb,eex,hx,hy);
error_div = q2divx(xy,mv,xmr);
errorest=norm(sqrt(error_x+error_y+error_div.^2),2);
