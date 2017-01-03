% Model selection demo (polynomial regression)

seed = 2;
rand('state', seed);
randn('state', seed);

ndata = 8;
x1d=rand(ndata,1)*10; % input points
e=randn(ndata,1); % noise
targets = (x1d-4).^2 + 5*e; % actual function
plotvals1d = [-2:0.1:12]'; % uniform grid for plotting/ testing
trueOutput = (plotvals1d-4).^2;


figure(2);clf
degs = [0 1 2 3];
for m=1:length(degs)
  deg=degs(m);
  x = polyBasis(x1d, deg);
  plotvals = polyBasis(plotvals1d, deg);

  % Set up network parameters.
  nin = deg+1;		% Number of inputs.
  nhidden = 0;		% Number of hidden units.
  nout = 1;		% Number of outputs.
  alpha = 0.01;		% Initial prior hyperparameter. 
  beta_init = 0.05;	% Initial noise hyperparameter.
  
  % Create and initialize network weight vector.
  %net = mlp(nin, nhidden, nout, 'linear', alpha, beta_init)
  net = glm(nin, nout, 'linear', alpha, beta_init);
  
  % Set up vector of options for the optimiser.
  nouter = 5;			% Number of outer loops.
  ninner = 2;			% Number of innter loops.
  options = zeros(1,18);		% Default options vector.
  options(1) = 0;			% This provides display of error values.
  options(2) = 1.0e-7;		% Absolute precision for weights.
  options(3) = 1.0e-7;		% Precision for objective function.
  options(14) = 500;		% Number of training cycles in inner loop. 
  
  % Train using scaled conjugate gradients, re-estimating alpha and beta.
  for k = 1:nouter
    %net = netopt(net, options, x, targets, 'scg');
    net = glmtrain(net, options, x, targets);
    [net, gamma, logev(m)] = evidence(net, x, targets, ninner);
  end
  
  % Evaluate error bars.
  %[y, sig2] = netevfwd(mlppak(net), net, x, t, plotvals);
  [y, sig2] = glmevfwd(net, x, targets, plotvals);
  sig = sqrt(sig2);
  
  % Plot the data, the original function, and the trained network function.
  %[y, z] = mlpfwd(net, plotvals);
  %figure(m+1); clf
  figure(2); subplot(2,2,m)
  plot(x1d, targets, 'ok')
  hold on
  plot(plotvals1d, trueOutput, 'g-');
  plot(plotvals1d, y, '-r')
  plot(plotvals1d, y + sig, 'b:');
  plot(plotvals1d, y - sig, 'b:');
  title(sprintf('m=%d,logev=%5.3f', m-1, logev(m)))

end
str = sprintf('polybayesEvidence_N%d', ndata);
%print(gcf, '-depsc', fullfile('C:\kmurphy\Teaching\stat406-spring06\Book\figures', str))

figure(1);clf
PP=exp(logev);
PP=PP/sum(PP);
bar(degs, PP)
axis([-0.5 length(degs)+0.5 0 1]);
set(gca,'FontSize',16);
aa=xlabel('M'); set(aa,'FontSize',20);
aa=ylabel('P(M|D)'); set(aa,'FontSize',20);
str = sprintf('polybayesEvidence_histo_N%d', ndata);
%print(gcf, '-depsc', fullfile('C:\kmurphy\Teaching\stat406-spring06\Book\figures', str))
