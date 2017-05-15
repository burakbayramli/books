% The Choi-Williams kernel

x=linspace(0,1)-0.5; %100 values between -0.5 and 0.5
theta=x*100*2*pi; %frequencies
tau=x*1; %delays
alpha=0.001; %parameter

Psi=zeros(100,100);
for n=1:100,
   for m=1:100,
      p=-alpha*(tau(n)^2)*(theta(m)^2);
      Psi(n,m)=exp(p);
   end;
end;

mesh(Psi); view(30,60); %3D plot
set(gca,'XTickLabel',{'-0.5';'-0.3';'-0.1';'0.1'; '0.3'; '0.5'});
set(gca,'YTickLabel',{'-50';'-30';'-10';'10'; '30'; '50'});
      
title('Choi-Williams kernel'); xlabel('sec'); ylabel('Hz');
