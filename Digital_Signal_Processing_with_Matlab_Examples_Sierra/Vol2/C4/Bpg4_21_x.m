% Explore a set of directions, display Lagrangians
% use after 2-D wavelet analysis (I need tfg)

th=5; %threshold

%the square
rbeg=1;
cbeg=5*Nr/8;
cw=(Nr/8)-1; 
sq=tfg(rbeg:rbeg+cw,cbeg:cbeg+cw);
np=cw+1;
[YY,XX]=meshgrid(1:np,1:np);

NA=60; %number of angles to test
SLG=zeros(1,NA); %space for Lagrangians

for nx=1:NA,   
%projection
alpha=(nx*pi)/NA; %rads
p=(-sin(alpha)*XX(:))+ (cos(alpha)*YY(:));
[aux,ix]=sort(p); %ascending order
F=sq(ix); %ordered values

%1-D Haar wavelet transform of F
Ns=length(F);
K=floor(log2(Ns)); %number of scales
wty=F;
for n=1:K,
   aux1= wty(1:2:Ns-1) + wty(2:2:Ns);
   aux2= wty(1:2:Ns-1) - wty(2:2:Ns);
   wty(1:Ns)=[aux1,aux2]/sqrt(2);
   Ns=Ns/2;
end;

%evaluation
Rs=wty.*(abs(wty)<th); %residual
Sa=sum(abs(wty(:)>th)); %value above threshold
Lg=sum(Rs(:).^2)+(Sa*(th^2)); %Lagrangian

SLG(nx)=Lg;

end;

%display
figure(1)
xa=(1:NA).*(180/NA);
plot(xa(:),SLG(:),'k');
title('Lagrangian vs. angle');
xlabel('angle in degrees');
h=gca;ht=get(h,'Title');hx=get(h,'XLabel');hy=get(h,'YLabel');hp=get(h,'Children');
set(hp,'LineWidth',2); set(ht,'FontSize',14);set(hx,'FontSize',12);set(hy,'FontSize',12);


[minL,iL]=min(SLG);
%print best angle 
angle=(iL*180)/NA 
