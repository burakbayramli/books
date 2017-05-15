% Example of Chan-Vese algorithm
%(active contours segmentation, using level sets)

I=imread('hand1.jpg'); %read b&w image
I = double(I);
[m,n] = size(I);

lambda=0.1; %parameter

%Set initial box=1, with border=-1
Phi=ones(m,n); 
Phi(1,1:n)=-1; Phi(m,1:n)=-1; 
Phi(1:m,1)=-1; Phi(1:m,n)=-1;

%Prepare loop
disp('working...');
a = 0.01;  %to avoid division by zero.
epl = 0.1; %small value
T = 10;  
dt = 0.2;

for t = 0:dt:T
   ax2=2*Phi; 
   P1=Phi(:,[2:n,n]); P2=Phi([2:m,m],:);
   P3=Phi(:,[1,1:n-1]); P4=Phi([1,1:m-1],:);
  %partial derivatives (approx.)
   Phi_x = (P1 - P3)/2;
	Phi_y = (P2 - P4)/2;
	Phi_xx = P1 - ax2 + P3;
   Phi_yy = P2 - ax2 + P4;
   Q1=Phi([2:m,m],[2:n,n]); Q2=Phi([1,1:m-1],[1,1:n-1]);
   Q3=Phi([1,1:m-1],[2:n,n]); Q4=Phi([2:m,m],[1,1:n-1]);
	Phi_xy = (Q1 + Q2 - Q3 - Q4)/4;
   
   %TV term
   Num = (Phi_xx.*Phi_y.^2 )- (2*Phi_x.*Phi_y.*Phi_xy) + (Phi_yy.*Phi_x.^2);
   Den = (Phi_x.^2 + Phi_y.^2).^(3/2) + a;
    
   %Compute averages
   c_in = sum([Phi>0].*I)/(a+sum([Phi>0]));
   c_out = sum([Phi<0].*I)/(a+sum([Phi<0]));
    
   %Update
   aux=( Num./Den - lambda*(I-c_in).^2 + lambda*(I-c_out).^2);
   Phi = Phi + dt*epl./(pi*(epl^2+Phi.^2)).*aux;
   
end;

% display of results

figure(1)
imagesc(I);
title('Original image');
colormap gray;

figure(2)
subplot(121); 
imagesc(Phi); 
title('Level Set'); 

subplot(122); 
imagesc(I); hold on;
title('Chan-Vese Segmentation');  
contour(Phi,[0,0],'m');
colormap gray;