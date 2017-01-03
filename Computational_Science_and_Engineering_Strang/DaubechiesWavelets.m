%% Matlab code for calculating and plotting Daubechies scaling function and
%% wavelets. 

% Based on Gil Strang's "Wavelets" American Scientist.
% Coded by Yossi Farjoun 2011.


D = [1+sqrt(3) 3+sqrt(3) 3-sqrt(3) 1-sqrt(3)]/4;
 
M0 = [ D(1) 0     0; ...
       D(3) D(2)  D(1);...
       0    D(4)  D(3)];

M1 = [D(2) D(1) 0;...
      D(4) D(3) D(2);
      0    0    D(4)];

 F = [0 D(1) D(4)]'*2 ;
 X = (0:2)';
 
 n=9; %desired number of iterations.
 
 %calculate the scaling function:
 for k=1:n
     F=[M0*F M1*F];
     X=[floor(X)+mod(X,1)/2 floor(X)+mod(X,1)/2+0.5];
 end
 

 [X,i]=sort(X(:));
 F=F(i)';

 X=[X' 3];
 F=[F 0];
 
 figure(1)
 plot(X,F)
 title('Scaling Function')
  
 %find where the scaling function is going in the wavelets:
 XW = (ones(4,1)*X +(0:3)'* ones(size(X)))/2;
 
 allX=unique(XW(:));
 W=zeros(4,length(allX));
 
 %fill the rest with zeros
 for l=1:4
     extraX=setdiff(allX,XW(l,:));
     W(l,:)  = [F 0*extraX];
     [~,i]=sort([XW(l,:) extraX]);
     W(l,:)=W(l,i);
 end
 
 figure(2)
 
 %plot the dot product 
 D = D(4:-1:1).*[1 -1 1 -1];
 plot(allX, D*W )
 title('Daubechies Wavelets')
 
 
 
 
 
 
 
 
 