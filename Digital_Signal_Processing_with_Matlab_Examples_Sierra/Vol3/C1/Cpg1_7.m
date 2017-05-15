%Propagation through nonlinearity
%Several values of variance

%gaussian random data
Nd=40000;
rdat=randn(1,Nd);
bx=-1.5:0.05:1.5; %location of histogram bins

figure(1)

for nn=1:3,
   switch nn,
   case 1, sig=0.7; 
   case 2, sig=1;
   case 3, sig=2;   
   end;
  adat=sig*rdat;
  pdat=atan(adat);
  hpt=hist(pdat,bx); 
  subplot(1,3,nn)
  plot(bx,hpt,'k');
  tit=['sigma= ',num2str(sig)];
  title(tit);
  axis([-1.5 1.5 0 1800]);  
end
