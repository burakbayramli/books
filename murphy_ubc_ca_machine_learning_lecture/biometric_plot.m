% biometric_plot

% data from http://www.stat.psu.edu/~resources/bydata.htm
% Simplified using biometric_data_cleanup.m

rawdata = dlmread('biometric_data_simple.txt');
data.C = rawdata(:,1); % 1=male, 2=female
data.X = [rawdata(:,2) rawdata(:,3)]; % height, weight
data.featureNames = {'height ', 'weight'};
data.classNames = {'male', 'female'}; % 1,2 
Nclasses = 2;
Ndims = 2;
maleClass = find(strcmp(data.classNames, 'male'));
femaleClass = find(strcmp(data.classNames, 'female'));


colors = 'br';
figure(1); clf
maleNdx = find(data.C == maleClass);
femaleNdx = find(data.C == femaleClass);
scatter(data.X(maleNdx,1), data.X(maleNdx,2), 10, 'bx');
hold on
scatter(data.X(femaleNdx,1), data.X(femaleNdx,2), 10, 'ro');
xlabel('height')
ylabel('weight')
title('red = female, blue=male')
%print(gcf, '-djpeg', 'figures/biometric_scatterplot.jpg')
%print(gcf, '-depsc', 'figures/biometric_scatterplot.eps')

% Fit 2D Gaussians
mu = zeros(2,2);
Sigma = zeros(2,2,2);
classNdx = {maleNdx, femaleNdx};
for c=1:2
  mu(:,c)  = mean(data.X(classNdx{c},:))';
  Sigma(:,:,c)  = cov(data.X(classNdx{c},:),1);
  hold on
  plot2dgauss(mu(:,c), Sigma(:,:,c), colors(c));
end
