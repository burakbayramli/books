% BILD017, Demo: Interpolation von 1/(1 + x*x)
% mit Interpolationspolynomen
clf
X    = linspace(-5,5,100);
P    = 1./(1 + X.*X);
Y1   = [-4, -2, 0, 2, 4];
Z1   = 1./(1 + Y1.*Y1);
D    = divdif(Y1,Z1);
IPN1 = D(1)+(X+4).*(D(2) + (X+2).*(D(3) + X.*(D(4) + D(5)*(X - 2))));
%
Y2   = [-5,-3,-1,0,1,3,5];
Z2   = 1./(1 + Y2.*Y2);
D    = divdif(Y2,Z2);
IPN2 = D(1)+(X+5).*(D(2) + (X+3).*(D(3) + (X+1) ...
       .*(D(4) + X.*(D(5) + (X - 1).*(D(6) + D(7)*(X - 3))))));
plot(X,P,'k','linewidth',2),    hold on
plot(X,IPN1,'k','linewidth',2), hold on
plot(X,IPN2,'k','linewidth',2), hold on
rr = 0.08;
for I = 1:length(Y1)
   circle(Y1(I),Z1(I),rr,'w')
end
for I = 1:length(Y2)
   circle(Y2(I),Z2(I),rr,'w')
end
grid off
axis equal tight
text(-4.6,-0.3,'f(x)','fontsize',18);
text(-4.2,0.7,'p_4','fontsize',18);
text(-3.6,1.6,'p_6','fontsize',18);
