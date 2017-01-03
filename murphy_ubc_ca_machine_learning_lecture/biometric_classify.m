
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

%%%%%%%%%%%%%%%%%%%%%%%%
% VISUALIZE DATA

%figure(1); clf;
%plotmatrix(data.X)

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


figure(2);clf
for d=1:Ndims
  dat = data.X(:,d);
  minx = min(dat); maxx = max(dat);
  for c=1:Nclasses
    ndx = find(data.C == c);
    dat = data.X(ndx,d);
    subplot2(Ndims, Nclasses, d, c);
    %hist(dat);
    [counts, centers] = hist(dat);
    ncounts = normalize(counts);
    bar(centers,ncounts)
    title(sprintf('%s %s', data.featureNames{d}, data.classNames{c}));
    set(gca,'xlim', [minx maxx]);
  end
end




traindata = data;


%%%%%%%%%%%%%%%%%%%%%%%
% Fit class conditional 1D Gaussians to each feature
% params.mu(c,c) for feature d, class c
for d=1:Ndims
  for c=1:Nclasses
    ndx = find(traindata.C == c);
    dat = traindata.X(ndx,d);
    params.mu(c,d) = mean(dat);
    params.sigma(c,d) = std(dat);
  end
end


% Fit priors by counting
n = length(traindata.C);
for c=1:Nclasses
  params.classPrior(c) = length(find(traindata.C==c))/n;
end

% Visualize model fit
figure(2);
for d=1:Ndims
  dat = traindata.X(:,d);
  minx = min(dat); maxx = max(dat);
  for c=1:Nclasses
    subplot2(Ndims, Nclasses, d, c);
    xs = linspace(0.5*minx, 1.5*maxx, 100);
    hold on
    ps = normpdf(xs, params.mu(c,d), params.sigma(c,d));
    ps = normalize(ps);
    sf = max(ncounts) / max(ps);
    h = plot(xs, ps * sf);
    set(h, 'linewidth', 3)
    set(gca,'xlim', [minx maxx]);
  end
end


%%%%%%%%%% Test

x = [72 180];
x = [60 100];
x = [68 155];

pm = normpdf(x(1), params.mu(1,1), params.sigma(1,1)) *...
     normpdf(x(2), params.mu(1,2), params.sigma(1,2));

pf = normpdf(x(1), params.mu(2,1), params.sigma(2,1)) *...
     normpdf(x(2), params.mu(2,2), params.sigma(2,2));

post  = pm/(pm+pf)
