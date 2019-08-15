% BILD037, Beispiel 1, Dennis-Schnabel
clf
X = linspace(-2.24,2.24,40);
Y = X.*X;
plot(X,Y,'k','linewidth',2);
hold on
n = 6;
X = zeros(1,n+1);
Y = X;
X(1) = 2;
Y(1) = 4;
for i = 1:n
    d = 1;
    s = 1/2^i;
    X(i+1) = X(i) - s*d;
    Y(i+1) = X(i+1)*X(i+1);
end
plot(X,Y,'k','linewidth',2);
hold on
for I = 1:length(X)
   circle(X(I),Y(I),0.09,'w')
end
plot(-3,-1,'w'), hold on
plot(3,5,'w')
%axis([-3 3 -1 5]);
axis equal tight
grid off
