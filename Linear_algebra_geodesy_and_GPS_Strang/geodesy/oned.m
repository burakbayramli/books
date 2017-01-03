%ONED    Code given by Mike Bevis, January 1997

clf, format compact, hold off
disp('');disp('oned output:')
n = 150;
L1t = 0.75;
L2t = 1.25;
for i = 1:2
   if i == 1
      s0 = 0;
   else
      s0 = 0.3;
   end
   s1 = 0.1;
   s2 = 0.2;
   del0 = s0*randn(1,n);
   l1 = L1t*ones(1,n) + s1*randn(1,n) - del0;
   l2 = L2t*ones(1,n) + s2*randn(1,n) - del0;
   v = [l1;l2];
   m = mean(v')';
   Cv = cov(v');
   Cm = Cv/n;
   [xe,ye] = errell(m(1,1),m(2,1),Cv,2);
   [xme,yme] = errell(m(1,1),m(2,1),Cm,2);
   if i == 1
      subplot(1,2,1)
      plot(l1,l2,'r+',xe,ye,'b-',xme,yme,'b-')
      axis('equal'), axis('square')
      xlabel('Observed l1'), ylabel('Observed l2')
      title('No error at origin')
   else
      subplot(1,2,2)
      plot(l1,l2,'r+',xe,ye,'b-',xme,yme,'b-')
      axis('equal'), axis('square')
      xlabel('Observed l1'), ylabel('Observed l2')
      title('With error at origin')
   end
   set(get(gca,'xlabel'),'FontSize',16);
   set(get(gca,'ylabel'),'FontSize',16);
   set(get(gca,'title'),'FontSize',16);
   set(gca,'FontSize',16);
   print oned -deps
end

T = [-1 +1];
dL = T*m;
Cd = T*Cm*T';
disp('estimated "absolute" coordinate for point 1')
disp(['mean = ',num2str(m(1,1))])
disp(['std. error = ',num2str(sqrt(Cm(1,1)))])
disp('estimated "absolute" coordinate for point 2')
disp(['mean = ',num2str(m(2,1))])
disp(['std. error = ',num2str(sqrt(Cm(2,2)))])
disp('estimated distance between points 1 and 2')
disp('by propagating covariance thru linear transformation T')
disp(['mean = ',num2str(dL)])
disp(['std. error = ',num2str(sqrt(Cd))])

disp('estimated distance between points 1 and 2')
disp('by differencing data matrix at front end')
del = l2-l1;
mdel = mean(del);
sddel = std(del);
sedel = sddel/sqrt(n);
disp(['mean = ',num2str(mdel)])
disp(['std. error = ',num2str(sedel)])
%%%%%%%%%%% end oned.m %%%%%%%%%%%%%%%%%%%
