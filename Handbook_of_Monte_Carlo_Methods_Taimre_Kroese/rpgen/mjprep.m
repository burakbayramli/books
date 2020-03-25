%mjprep.m
clear all, clf
lam1= 1; lam2 = 2; mu1= 3; mu2 = 4;
Q = [-(lam1 + lam2), lam1, lam2, 0, 0;
    mu1, -(mu1+ lam2), 0, lam2, 0; 
    mu2, 0, -(mu2 + lam1), 0, lam1;
    0, 0, mu1, -mu1, 0;
    0, mu2, 0, 0, -mu2];

q = -diag(Q); 
K = diag(1./q)*Q + eye(5);
T = 5;
n=0;
t = 0; y = 1;
yy = [y]; tt = [t];
while t < T
    A = -log(rand)/q(y);
    y =  min(find(cumsum(K(y,:))> rand));
    t = t + A;
    tt = [tt,t];
    yy= [yy,y];
    n= n+1;
end
for i=1:n
    line([tt(i),tt(i+1)],[yy(i),yy(i)],'Linewidth',3);
    line([tt(i+1),tt(i+1)],[yy(i),yy(i+1)],'Marker','.','LineStyle',':');
end
axis([0,T,1,5.1])
    
    
