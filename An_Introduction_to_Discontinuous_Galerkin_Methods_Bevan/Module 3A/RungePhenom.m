N=80
nd=linspace(0,2,N)';

f=@(x) sin(x);

nn=elim(nd(1:N)',nd(1:N)',[1 3 2]);
Lag= @(x,nv) prod(bsxfun(@rdivide,bsxfun(@minus,x,nn(nv,:,:)),bsxfun(@minus,nd(nv),nn(nv,:,:))),3);

interp_f=@(x) f(nd')*Lag(x,1:N);
xx=0:.001:2;
xx2=0:.1:2;
%plot(xx,interp_f(xx),'b')
plot(xx2,f(xx2),'o')
hold on

[Qx, Qw]=  GLquad(N);
Qx=Qx+1;
nn2=elim(Qx(1:N)',Qx(1:N)',[1 3 2]);
Lag2= @(x,nv) prod(bsxfun(@rdivide,bsxfun(@minus,x,nn2(nv,:,:)),bsxfun(@minus,Qx(nv),nn2(nv,:,:))),3);
interp_f2=@(x) f(Qx')*Lag2(x,1:N);
plot(xx,interp_f2(xx),'r')
print -djpg rung1.jpg
