%Load and visualize the bucky matrix
%
A=bucky; %rapid loading of the 60x60 bucky matrix
figure(1)
spy(A,'k');
title('the bucky ball matrix');
%
[A,v]=bucky; %load the bucky matrix and the coordinates
figure(2)
gplot(A,v,'k');
axis equal;
title('the bucky ball graph');

