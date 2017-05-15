% Bivariate normal PDF

x1=0:0.02:6;
x2=0:0.02:6;
N=length(x1);
%the PDF
mu1=3; mu2=3;
C=[0.4 0.1;
   0.1 0.6];
D=det(C);
K=1/(2*pi*sqrt(D)); Q=(C(1,1)*C(2,2))/(2*D);

ypdf=zeros(N,N); %space for the PDF
for ni=1:N,
   for nj=1:N,
      aux1=(((x1(ni)-mu1)^2)/C(1,1))+(((x2(nj)-mu2)^2)/C(2,2))...
         -(((x1(ni)-mu1).*(x2(nj)-mu2)/C(1,2)*C(2,1)));
     ypdf(ni,nj)= K*exp(-Q*aux1);
  end;
end;
%display
figure(1)
mesh(x1,x2,ypdf);
title('Bivariate Gaussian: 3D view');
figure(2)
contour(x1,x2,ypdf);
axis([1 5 1 5]);
title('Bivariate Gaussian PDF: top view');
