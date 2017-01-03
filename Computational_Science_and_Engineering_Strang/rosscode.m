%2.2  rosscode.m

%This time centered version of leapfrog called velocity Verlet makes it
%a whole lot easier to reason about what 'v' actually is in that case.

M=[9 0; 0 1];  K = [81 -6; -6 6] ;
u = [1;0]; v = [0;0];

U=[]; V=[];
for j=0:8,  dt=.63/2^j;
     u = [1;0]; v = [0;0];
     for i=1:100*2^j,
       vh=v(:,end)-(dt/2)*(M\(K*u(:,end)));
       u(:,end+1)=u(:,end)+dt*vh;
       v(:,end+1)=vh-(dt/2)*(M\(K*u(:,end)));
     end
     U(:,j+1) = u(:,end); V(:,j+1) = v(:,end);
   end
U
V



%U =

%  Columns 1 through 7 

%    0.4179    0.4613    0.5744    0.0085   -0.2904   -0.3633   -0.3812
%    0.4448    0.5843   -2.3190   -1.4381   -0.9126   -0.7800   -0.7471

%  Columns 8 through 9 

%   -0.3857   -0.3868
%   -0.7388   -0.7368


%V =

%  Columns 1 through 7 

%    0.0130   -1.8234   -0.5738    2.2929    2.3048    2.2492    2.2317
%    1.6465    4.3845    1.6423   -4.4800   -4.9263   -4.9460   -4.9453

%  Columns 8 through 9 

%    2.2271    2.2260
%   -4.9448   -4.9447



%> 
%> at t=63
%> Uexact =
%> 
%>   -0.3871
%>   -0.7361
%> 
%> 
%> Vexact =
%> 
%>    2.2255
%>   -4.9446

