% Smallest sub-image analysis 
% for the larger LH

tfg=B_2D_Haar; %function call

th=5; %threshold

sq=tfg(257:512,1:256); %LH square
n=size(sq); 
Kmax=8; %256 = 2^8
%smallest square
Kmin=4; %exponent

sl=2^Kmin; %square side
nsl=n(1)/sl; %number of squares in a row or column
Q=zeros(n)+Kmin; A=zeros(n); L=zeros(n/sl);

[YY,XX]=meshgrid(1:sl,1:sl);
NA=60; %number of angles to test

disp('analysis of smallest squares');

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
     A(wx,wy)=iL; %index to angle
   
	end;
end;

disp('--Lagrangians of smallest squares');
disp('--already computed');

%display
figure (1)
subplot(1,2,1)
imagesc(A);
title('best angles')
subplot(1,2,2)
imagesc(L)
title('Lagrangians')