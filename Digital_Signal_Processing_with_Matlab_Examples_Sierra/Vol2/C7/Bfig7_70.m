% GP Illustration

%display
figure(1)
plot([0 10],[0 0],'k'); hold on
plot([0 0],[0 10],'k');
stem([2 2],[0 7],'k');
stem([4 4],[0 2],'k');
stem([8 8],[0 5],'k');
plot([6 6],[0 0],'ko');
title('Prediction problem')
xlabel('x'); ylabel('f(x)');
