
% How to use rpnorm?
% (c) Vincent Mazet, CRAN, 05/2005
% vincent.mazet@cran.uhp-nancy.fr



fprintf('\n\tRPNORM - Create 10,000 positive normal variables');

% Random variable generation
fprintf('\n\tParameters of the normal distribution:');
N = 10000;
fprintf('\n\t'); m = input('mean = ');
fprintf('\t'); s = input('standart deviation = ');
R = rpnorm(N,m,s);

% The corresponding normal distribution
x = linspace(0,m+5*s,1000);
y = exp(-(x-m).^2/2/s^2) / sqrt(pi*s^2/2) / (1+erf(m/sqrt(2*s^2)));

% Plot
fprintf('\n\tPlot the histogram...\n\n');
figure;
pas = 0.01;
edges = 0:pas:ceil(max(R));
H = histc(R,edges);
b = bar(edges,H/N/pas,1,'histc');
set(b,'FaceColor','b','EdgeColor','b');
hold on;
plot(x,y,'Color','r','LineWidth',3);
legend('the truncated normal distribution','10,000 positive normal variables');