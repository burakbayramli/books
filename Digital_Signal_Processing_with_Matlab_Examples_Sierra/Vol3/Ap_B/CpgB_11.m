%Example of Laplace's and KLD approximations

% Student's PDF
v=-5:0.01:5; %values set
N=length(v);
nu=3; %random variable parameter ("degrees of freedom")
ypdf=tpdf(v,nu); %chi-square PDF
py=max(ypdf); %the peak

%Laplace's (coincidence around the peak)
mE=1000; msig=0;
for sig=0.6:0.01:1,
  npdf=normpdf(v,0,sig); %normal PDF
  pn=max(npdf);
  Lpdf=(py/pn)*npdf;
  %error around the mode
  E=0; c=round(N/2);
  for nn=1:20,
     E=E+abs(ypdf(c+nn)-Lpdf(c+nn));
     E=E+abs(ypdf(c-nn)-Lpdf(c-nn));
  end;
  if E<mE, %take the minimum
     mE=E; msig=sig;
  end;
end;

npdf=normpdf(v,0,msig); %normal PDF
pn=max(npdf);
Lpdf=(py/pn)*npdf;
  
%search for min KLD
mKL=1000; msig=0;
for sig=0.6:0.01:1,
     xpdf=normpdf(v,0,sig);   
     %KLD calculation
     KL=0;
     for nn=1:N,
       aux1=ypdf(nn); aux2=xpdf(nn);
       KL=KL+(aux2*log(aux2/aux1));
     end;
     if KL<mKL, %take the minimum
        mKL=KL; msig=sig;
     end;   
end;

mpdf=normpdf(v,0,msig);

%display
figure(1)
subplot(2,1,1)
plot(v,ypdf,'k'); hold on; %plots figure
plot(v,Lpdf,'g'); %the approximation
axis([-5 5 0 0.5]);
xlabel('values'); title('Sudent´s PDF (black), Laplace approximation (green)');

subplot(2,1,2)
plot(v,ypdf,'k'); hold on; %plots figure
plot(v,mpdf,'g'); %the approximation
axis([-5 5 0 0.5]);
xlabel('values'); title('Sudent´s PDF (black), KLD approximation (green)');

mKL