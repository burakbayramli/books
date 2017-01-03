%nm5p06c
clf
fp56a=inline('sin(x)./x','x'); 
fp56a2=inline('sin(1./y)./y','y'); 
fp56b=inline('exp(-x.*x)','x'); 
x0=[eps:2000]/20; x=[eps:100]/100;
subplot(221), plot(x0,fp56a(x0))
subplot(223), plot(x0,fp56b(x0))
subplot(222), y=logspace(-3,0,2000); loglog(y,abs(fp56a2(y)))
subplot(224), y=logspace(-6,-3,2000); loglog(y,abs(fp56a2(y)))
