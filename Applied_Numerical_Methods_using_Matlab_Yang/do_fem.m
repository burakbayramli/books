%do_fem 
% for Example 9.6
clear
N=[-1 0;-1 -1;-1/2 -1;0 -1;1/2 -1; 1 -1;1 0;1 1;1/2 1; 0 1;
-1/2 1;-1 1; -1/2 -1/4; -5/8 -7/16;-3/4 -5/8;-1/2 -5/8;
-1/4 -5/8;-3/8 -7/16; 0  0; 1/2 1/4;5/8 7/16;3/4 5/8;
1/2 5/8;1/4 5/8;3/8 7/16;-9/16 -17/32;-7/16 -17/32;
-1/2 -7/16;9/16 17/32;7/16 17/32;1/2 7/16]; %nodes
N_b=12; %the number of boundary nodes
S=[1 11 12;1 11 19;10 11 19;4 5 19;5 7 19; 5 6 7;1 2 15; 2 3 15;
3 15 17;3 4 17;4 17 19;13 17 19;1 13 19;1 13 15;7 8 22;8 9 22;
9 22 24;9 10 24; 10 19 24; 19 20 24;7 19 20; 7 20 22;13 14 18;
14 15 16;16 17 18;20 21 25;21 22 23;23 24 25;14 26 28;
16 26 27;18 27 28; 21 29 31;23 29 30;25 30 31; 
26 27 28; 29 30 31]; %triangular subregions

f962='(norm([x y]+[0.5 0.5])<0.01)-(norm([x y]-[0.5 0.5])<0.01)';
f=inline(f962,'x','y'); %(E9.6-2)
g=inline('0','x','y');

N_n= size(N,1); %the total number of nodes
N_i=N_n-N_b; %the number of interior nodes
c=zeros(1,N_n); %boundary value or 0 for boundary/interior nodes
p=fem_basis_ftn(N,S);
[U,c]=fem_coef(f,g,p,c,N,S,N_i);
%Output through the triangular mesh-type graph
figure(1), clf, trimesh(S,N(:,1),N(:,2),c)

%Output through the rectangular mesh-type graph
N_s= size(S,1); %the total number of subregions(triangles)
x0=-1; xf=1; y0=-1; yf=1;
Mx=16; dx=(xf-x0)/Mx; xi=x0+[0:Mx]*dx; 
My=16; dy=(yf-y0)/My; yi=y0+[0:My]*dy;
for i=1:length(xi)
  for j=1:length(yi)
    for s= 1:N_s %which subregion the point belongs to
      if inpolygon(xi(i),yi(j), N(S(s,:),1),N(S(s,:),2))>0
        Z(i,j)= U(s,:)*[1 xi(i) yi(j)]'; %Eq.(9.4-5b)
        break; 
      end
    end
  end      
end
figure(2), clf, mesh(xi,yi,Z)

%For comparison
bx0=inline('0');  bxf=inline('0');
by0=inline('0');  byf=inline('0');
[U,x,y]= poisson(f,g,bx0,bxf,by0,byf,[x0 xf y0 yf],Mx,My); 
figure(3), clf, mesh(x,y,U)
