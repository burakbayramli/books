N=80

f=@(x) sin(x);

[Qx, Qw]=  GLquad(N);
nn=elim(Qx(1:N)',Qx(1:N)',[1 3 2]);
Lag= @(x,nv) prod(bsxfun(@rdivide,bsxfun(@minus,x,nn(nv,:,:)),bsxfun(@minus,Qx(nv),nn(nv,:,:))),3);
interp_f=@(x) f(Qx')*Lag(x,1:N);

dLag= @(x,nv) Lag(x,nv).*sum(1./bsxfun(@minus,x,nn(nv,:,:)),3);

xx=0:.001:2;
plot(xx,interp_f(xx),'b')
hold on
plot(xx,f(xx),':k')

