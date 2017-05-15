
function SLG=B_Bestdir(sq,th,NA)

%Find best direction, exploring on NA angles

SLG=zeros(1,NA); %space for Lagrangians

for nx=1:NA,   
%projection
alpha=(nx*pi)/NA; %rads
wty=B_Bset(sq,alpha); %function call

%evaluation
Rs=wty.*(abs(wty)<th); %residual
Sa=sum(abs(wty(:)>th)); %value above threshold
Lg=sum(Rs(:).^2)+(Sa*(th^2)); %Lagrangian

SLG(nx)=Lg;

end;

