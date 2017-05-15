%Example of sigma points
%
x1=-10:0.1:10;
x2=-10:0.1:10;
pN1=length(x1);
pN2=length(x2);

%2D Gaussian---------------------------------
C=[9 0; 0 9];
mu1=0; mu2=0;
D=det(C);
K=1/(2*pi*sqrt(D)); Q=(C(1,1)*C(2,2))/(2*D);

y=zeros(pN1,pN2); %space for the PDF
for ni=1:pN1,
   for nj=1:pN2,
      aux1=(((x1(ni)-mu1)^2)/C(1,1))+(((x2(nj)-mu2).^2)/C(2,2));        
      y(ni,nj)= K*exp(-Q*aux1);
  end;
end;

ymax=max(max(y)); %the PDF peak

%sigma points
xs=zeros(2,6); %reserve space
xs0=[mu1; mu2];
xs(:,1)=xs0+[3;0]; xs(:,2)=xs0+[0;3];
xs(:,3)=xs0-[3;0]; xs(:,4)=xs0-[0;3];

figure(1)

subplot(2,1,1)
ypk=ymax+0.002;
mesh(x1,x2,y); hold on; %the PDF
view(-7,45);
%sigma verticals
plot3([mu1 mu1],[mu2 mu2],[0 ypk],'k');
for nn=1:4,
   plot3([xs(1,nn) xs(1,nn)],[xs(2,nn) xs(2,nn)],[0 ypk],'k');
end;
title('Gaussian PDF with sigma points');

subplot(2,1,2)
contour(x1,x2,y'); hold on; %the support
%sigma points:
plot(mu1,mu2,'rx','MarkerSize',10);
for nn=1:4,
plot(xs(1,nn),xs(2,nn),'rx','MarkerSize',10);
end;
title('sigma points');
