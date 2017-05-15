%Propagation through nonlinearity
%Several values of variance

%gaussian random data
Nd=40000;
rdat=randn(1,Nd);
bx=-1.5:0.05:1.5; %location of histogram bins

figure(1)

for nn=1:3,
   switch nn,
   case 1, sig=0.2; 
   case 2, sig=0.4;
   case 3, sig=0.6;   
   end;
  adat=sig*rdat;
  pdat=atan(adat);
  hpt=hist(pdat,bx); 
  %data through tangent
  ttg=1; tb=0; %the tangent
  tdat=ttg*(adat)+tb;
  %histogram of data through tangent
  htg=hist(tdat,bx); 

  subplot(1,3,nn)
  plot(bx,hpt,'b'); hold on;
  plot(bx,htg,'k');
  tit=['sigma= ',num2str(sig)];
  title(tit);
  axis([-1.5 1.5 0 4500]);
  
end;