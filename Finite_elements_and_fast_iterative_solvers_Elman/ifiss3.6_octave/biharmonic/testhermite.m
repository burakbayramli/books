%TESTHERMITE plots Hermite cubic shape functions
%   testhermite;
%   IFISS scriptfile: DJS; 5 September 2018.
% Copyright (c) 2018 D.J. Silvester, P. Nadukandi
s=-1:0.05:1;t=s;
%
% one dimensional shape functions
       phix1= 0.5*(1-s);  phix2= 0.5*(s+1);
       phiy1= 0.5*(1-t);  phiy2= 0.5*(t+1);
%
       ellx2= phix2.*phix2.*(1+2*phix1); ellx1= 1-ellx2;
       elly2= phiy2.*phiy2.*(1+2*phiy1); elly1= 1-elly2;
       ellx3= 2*phix1.*phix1.*phix2; ellx4= -2*phix1.*phix2.*phix2;
       elly3= 2*phiy1.*phiy1.*phiy2; elly4= -2*phiy1.*phiy2.*phiy2;
figure(1)
subplot(221), plot(s,ellx1,'-b'), axis square
subplot(222), plot(s,ellx2,'-b'), axis square
subplot(223), plot(s,ellx3,'-b'), axis square
subplot(224), plot(s,ellx4,'-b'), axis square
title('horizontal basis functions')
figure(2)
subplot(221), plot(t,elly1,'-r'), axis square
subplot(222), plot(t,elly2,'-r'), axis square
subplot(223), plot(t,elly3,'-r'), axis square
subplot(224), plot(t,elly4,'-r'), axis square
title('vertical basis functions')

%
       dellx2= 3*phix2.*phix1; dellx1= -dellx2;
       delly2= 3*phiy2.*phiy1; delly1= -delly2;
       dellx3= phix1.*(1-3*phix2);  delly3= phiy1.*(1-3*phiy2);
       dellx4= phix2.*(1-3*phix1);  delly4= phiy2.*(1-3*phiy1);
figure(3)
subplot(221), plot(s,dellx1,'-b'), axis square
subplot(222), plot(s,dellx2,'-b'), axis square
subplot(223), plot(s,dellx3,'-b'), axis square
subplot(224), plot(s,dellx4,'-b'), axis square
title('horizontal basis function derivatives')

%
       ddellx1= 1.5*s; ddellx2= -ddellx1;
       ddelly1= 1.5*t; ddelly2= -ddelly1;
       ddellx3= 0.5*(3*s-1); ddellx4= 0.5*(3*s+1);
       ddelly3= 0.5*(3*t-1); ddelly4= 0.5*(3*t+1);
figure(4)
subplot(221), plot(s,ddellx1,'-b'), axis square
subplot(222), plot(s,ddellx2,'-b'), axis square
subplot(223), plot(s,ddellx3,'-b'), axis square
subplot(224), plot(s,ddellx4,'-b'), axis square
title('horizontal basis function second derivatives')
%
return
