% Pole-zero map of G(s)
G=tf([1 5 3],[1 0 3 25]); %the transfer function G(s)
pzmap(G); %pole-zero map on the complex plane
title('pole-zero map');

