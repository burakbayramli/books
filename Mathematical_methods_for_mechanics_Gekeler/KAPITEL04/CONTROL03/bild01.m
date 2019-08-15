function bild01
% Figure for Example 1, Dyer-McReynolds, p. 127

load daten1 T X U
clf
plot(T,X(1,:),'r'), hold on
plot(T,X(2,:),'g'), hold on
plot(T,U,'b');
