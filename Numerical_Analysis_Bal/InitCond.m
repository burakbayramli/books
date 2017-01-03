function u=InitCond(Nx,x);

u=zeros(Nx+1,1); %% initialization 

%u(abs(x)<1)=1;   %% Initial condition N1

%u(abs(x-1)<=1)=1;  %% initial condition N2
%u(abs(x+1)<1)=.5; %% initial condition N2

for i=2:Nx    %% initial condition N3
   u(i)=exp(-100*((i-1)/(Nx)-1/2)^2);
end;    

