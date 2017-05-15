% Pole-zero maps of three G(s) cases
G1=tf([1],[1 1 1]); %the transfer function G1(s)
subplot(1,3,1); pzmap(G1); %pole-zero map on the complex plane
xlabel('stable G(s)');
G2=tf([1],[1 0 1]); %the transfer function G2(s)
subplot(1,3,2); pzmap(G2); %pole-zero map on the complex plane
xlabel('marginally stable G(s)');
G3=tf([1],[1 -1 1]); %the transfer function G3(s)
subplot(1,3,3); pzmap(G3); %pole-zero map on the complex plane
xlabel('unstable G(s)');
