% Uniform PDF
v=0:0.01:1; %values set
ypdf=unifpdf(v,0,1); %uniform PDF
plot(v,ypdf); hold on; %plots figure
axis([-0.5 1.5 0 1.1]);
xlabel('values'); title('uniform PDF');
plot([0 0],[0 1],'--k');
plot([1 1],[0 1],'--k');