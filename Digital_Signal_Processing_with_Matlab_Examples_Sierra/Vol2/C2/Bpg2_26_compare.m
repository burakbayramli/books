% Comparison of original and recovered signal
% (after running analysis and synthesis)

figure(1)
er=y-a';
plot(er,'k');
axis([0 512 1.2*min(er) 1.2*max(er)]);
xlabel('samples');
title('comparison of original and recovered signals');

