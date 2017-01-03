%2.9  fiedlerplotcode.m

theta=[1:N]*2*pi/N; x=zeros(2*N,1); y=x; % Angles in graph display
x(1:2:2*N-1)=cos(theta)-1; x(2:2:2*N)=cos(theta)+1;
y(1:2:2*N-1)=sin(theta)-1; y(2:2:2*N)=sin(theta)+1;
subplot(2,2,1), gplot(W,[x,y]), title('Graph') % First of four plots
subplot(2,2,2), spy(W), title('Adjacency matrix W')
subplot(2,2,3), plot(z(1:2:2*N-1),'ko'), hold on
plot(z(2:2:2*N),'r*'), hold off, title('Fiedler components')
[c,d]=sort(z); subplot(2,2,4), spy(W(d,d)), title('Reordered Matrix W')
