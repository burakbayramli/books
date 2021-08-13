function xrefplot(resid1,resid2,resid3,s1,s2,s3)
%XREFPLOT comparison plot of iterative residuals 
%   xrefplot(resid1,resid2,resid3,s1,s2,s3);
%   input
%          resid1, resid2, resid3      vectors of residuals
%          s1, s2, s3                  associated title strings            
%
%   IFISS function: DJS; 20 January 2011.
% Copyright (c) 2011 D.J. Silvester, H.C. Elman, A. Ramage
resid1=resid1/resid1(1); resid2=resid2/resid2(1); resid3=resid3/resid3(1);
ll1=length(resid1); ll2=length(resid2); ll3=length(resid3);
inx1=0:ll1-1;rx1=5:5:ll1;
inx2=0:ll2-1;rx2=5:5:ll2;
inx3=0:ll3-1;rx3=5:5:ll3;
%
%plot all data if fast convergence
if ll1<=9,rx1=inx1+1; end 
if ll2<=9,rx2=inx2+1; end 
if ll3<=9,rx3=inx3+1; end 
%
%  fix legend
semilogy(inx1,resid1,'-k');
hold on
semilogy(inx2(1),resid2(1),'x-r'); 
semilogy(inx3(1),resid3(1),'o-b');
%  plot data
semilogy(inx2,resid2,'-r',rx2-1,resid2(rx2),'xr'); 
semilogy(inx3,resid3,'-b',rx3-1,resid3(rx3),'ob');
hold off
axis('square')
xlabel('iteration number')
ylabel('residual reduction')
%title('LSC preconditioning')
%h=legend('exact', 'amg-ILU','amg-PDJ');
h=legend(s1,s2,s3);
set(h,'Interpreter','latex')
return
