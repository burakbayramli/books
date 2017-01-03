%nm5e02
%quad()/quad8() and int() to get CtFS coefficient in Ex 5.2
clf, clear
disp('(a)')
P=4; k=16; w0=2*pi/P; 
a=-1; b=1; N=80; tol=1e-3; trace=0;
ftn='exp(-j*k*w0*t)';  fcos=inline(ftn,'t','k','w0');
P=4; k=16; w0=2*pi/P;
a=-1; b=1; tol=0.001; trace=0;
X16_quad=quad(fcos,a,b,tol,trace,k,w0)
X16_quadl=quadl(fcos,a,b,tol,trace,k,w0)
%X16_as=adapt_smpsn3(fcos,a,b,tol,k,w0)
syms t; %declare symbolic variable %symbolic integration
Iexp=int(exp(-j*k*w0*t),t) %indefinite integral
Icos=int(cos(k*w0*t),t) %indefinite integral
X16_sym=int(cos(k*w0*t),t,-1,1) %definite integral
fprintf('\n(b)\n')
for i=1:2
  syms t %for symbolic integration
  switch i
    case 1 %rectangular wave
      ftn='exp(-j*k*w0*t)'; ftn='cos(k*w0*t)'; 
      Iexp=int(exp(-j*k*w0*t),t) %indefinite integral
      Icos=int(cos(k*w0*t),t) %indefinite integral
      X16_sym=int(cos(k*w0*t),t,a,b) %definite integral
      t=a; Icos1=eval(Icos); t=b; Icos_1=eval(Icos);
      Icos1_1=Icos1-Icos_1
    otherwise %triangular wave with tri1=inline('1-abs(t)','t');
      ftn='tri1_0(t).*exp(-j*k*w0*t)'; ftn='tri1_0(t).*cos(k*w0*t)'; 
      Iexp=int((1-abs(t)).*exp(-j*k*w0*t),t) %indefinite integral
      Icos=int((1-abs(t)).*cos(k*w0*t),t) %indefinite integral
      X16_sym=int(cos(k*w0*t),t,a,b); %definite integral
      int((1-abs(t))*cos(k*w0*t),t,-1,1)
      t=a; Iexp1=eval(Iexp); t=b; Iexp_1=eval(Iexp);
      Iexp1_1=Iexp1-Iexp_1
  end
  fejkw=inline(ftn,'t','k','w0');
  fprintf('The intgral of %30s:\n',ftn)
  tic, X16(1)=smpsns(fejkw,a,b,N,k,w0); times(1)=toc;
  tic, X16(2)=adapt_smpsn(fejkw,a,b,tol/1000,k,w0); times(2)=toc;
  tic, X16(3)=adapt_smpsn2(fejkw,a,b,tol/1000,k,w0); times(3)=toc;
  tic, X16(4)=adapt_smpsn3(fejkw,a,b,tol/10,k,w0); times(4)=toc;
  subplot(221) 
  tic, X16(5)=quad(fejkw,a,b,tol,trace,k,w0); time(5)=toc;
  hold on 
  t=-1:0.01:1;
  plot(t,tri1_0(t),t,cos(k*w0*t))
  subplot(222)
  tic, X16(6)=quadl(fejkw,a,b,tol,trace,k,w0); times(6)=toc;
  hold on
  plot(t,tri1_0(t),t,cos(k*w0*t))
  fprintf('\n   smpsns     adapt_smpsn adapt_smpsn2 adapt_smpsn3     quad        quadl\n')
  fprintf('%12.4e ',X16), fprintf('\n')
  fprintf('%12.4e ',times), fprintf('\n')

end
  
 
