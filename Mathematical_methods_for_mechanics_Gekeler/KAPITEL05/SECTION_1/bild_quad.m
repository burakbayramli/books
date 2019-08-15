function bild_quad
load daten_quad B P Q X c SCALE
LX = size(X,1);
aa = 0.1;
MAX_X = max(abs(X(:,1)));  
MAX_Y = max(abs(X(:,2)));  
SCALE = max(MAX_X,MAX_Y)*(1+aa);

plot_quad(P,B(1,:),c,SCALE,'b'), hold on
plot_quad(Q,B(2,:),c,SCALE,'g') 
for K = 1:LX
   if imag(X(K,1)) == 0
   circle(X(K,1),X(K,2),SCALE/50,'r',1); hold on
   end
end   
circle(0,0,SCALE/50,'k',1)

