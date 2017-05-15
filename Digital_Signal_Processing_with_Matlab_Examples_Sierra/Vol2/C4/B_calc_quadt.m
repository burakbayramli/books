function [Q,A]=B_calc_quadt(sq,th,Kmin,Kmax)

% quadtree calculation

n=size(sq); 
sl=2^Kmin; %smallest square side
nsl=n(1)/sl; %number of squares in a row or column
Q=zeros(n)+Kmin; A=zeros(n); L=zeros(n/sl);

[YY,XX]=meshgrid(1:sl,1:sl);
NA=60; %number of angles to test

%----------------------------------------------
%compute lagrangians of all smallest squares
for nx=0:nsl-1,
   for ny=0:nsl-1,
      %select a smallest square
      wx=1+(nx*sl):((nx+1)*sl); %range x
      wy=1+(ny*sl):((ny+1)*sl); %range y
      ssq=sq(wx,wy);
      
      SLG=B_Bestdir(ssq,th,NA); %function call
      
      %save best direction result   
	   [minL,iL]=min(SLG);
      L(1+nx,1+ny)=minL; %lagrangian
      A(wx,wy)=iL;
  end;
end;

%----------------------------------------------
% merging 4 squares into 1 when appropriate
% bottom-up iteration
gamma=0.15;

for j=Kmin+1:Kmax, %from small to bigger squares
   sl=2^j;
   nsl=n(1)/sl; %number of squares in a row or column
   [YY,XX]=meshgrid(1:sl,1:sl);

   Laux=zeros(n/sl); %for new lagrangians
   
   for nx=0:nsl-1,
   for ny=0:nsl-1,
      %select a square
      wx=1+(nx*sl):((nx+1)*sl); %range x
      wy=1+(ny*sl):((ny+1)*sl); %range y  
      ssq=sq(wx,wy);
      
      %sum the lagrangians of 4 smaller squares, and gamma
      sx=1+2*nx; sy=1+2*ny;
      Lsum=L(sx,sy)+L(1+sx,sy)+L(sx,1+sy)+L(1+sx,1+sy)+gamma;
      
      SLG=B_Bestdir(ssq,th,NA); %function call
      %best direction result
	   [minL,iL]=min(SLG);
        
 	   %merging if appropriate
 	   if minL<Lsum,
  	    Laux(1+nx,1+ny)=minL;
   	 Q(wx,wy)=j;
       A(wx,wy)=iL;
      else
       Laux(1+nx,1+ny)=Lsum;
      end;
   
   end;
   end;
  
  L=Laux;
end;  
