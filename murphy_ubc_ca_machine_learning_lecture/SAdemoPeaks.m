function SAdemoPeaks()
% Demo of Simulated Annealing for finding the most probable
% point of a peaky 2D surface

Z = peaks; % 49x49 surface surface

% find optimum by exhaustive search
M = max(Z(:)); % 8.0752
[row,col] = find(Z==M); % max  row 38, col 25
fprintf('max =%5.3f at r=%d,c=%d\n', M, row, col);
M = min(Z(:)); % -6.54566
[row,col] = find(Z==M); % min  row 12, col 27
fprintf('min =%5.3f at r=%d,c=%d\n', M, row, col);

global findMAX
findMAX = 1

if findMAX
  seed = 2; randn('state', seed); rand('state', seed);
  xinit = [35,25];
else
  seed = 1; randn('state', seed); rand('state', seed);
  xinit = [25,25]; % initial state is in middle of grid
end  

Sigma_prop = 2^2 * eye(2); %2^2 = variance
Nsamples  = 1000;
opts = struct(...
    'proposal', @(x) (x+(mvnrnd(zeros(2,1), Sigma_prop))), ...
    'maxIter', Nsamples, ...
    'minIter', Nsamples, ...
    'temp', @(T,iter) (0.995*T), ...
    'verbose', 0);

[xopt, fval, samples, energies, acceptRate] =  SA(@target, xinit, opts);
xopt
fval

figure; plot(energies)
ylabel('energy')
xlabel('iter')



% plot the histogram of samples
N_bins = 50; 
Nsamples = size(samples, 1); 
Ns = round(linspace(50, Nsamples, 4));
fig1 = figure;
for i=1:4
  T = Ns(i);
  figure(fig1)
  subplot(2,2,i)
  hist3(samples(1:T,:), [N_bins N_bins], 'FaceAlpha', 0.65);
  xlabel('x'); ylabel('y')
  title(sprintf('iter %d', T));
end

figure;[XX,YY]=meshgrid(1:49,1:49);
if findMAX
  surf(XX,YY,-Z);
else
  surf(XX,YY,Z);
end
xlabel('x');ylabel('y')
title('energy function')

if findMAX
  folder = 'C:\kmurphy\figures\other';
  figure(1); 
  print(gcf, '-depsc', fullfile(folder, 'SAdemoPeaksEnergyMax.eps'));
  figure(2)
  print(gcf, '-depsc', fullfile(folder, 'SAdemoPeaksHistMax.eps'))
  figure(3)
  print(gcf, '-depsc', fullfile(folder, 'SAdemoPeaksTargetMax.eps'))
else
  folder = 'C:\kmurphy\figures\other';
  figure(1); 
  print(gcf, '-depsc', fullfile(folder, 'SAdemoPeaksEnergyMin.eps'));
  figure(2)
  print(gcf, '-depsc', fullfile(folder, 'SAdemoPeaksHistMin.eps'))
  figure(3)
  print(gcf, '-depsc', fullfile(folder, 'SAdemoPeaksTargetMin.eps'))
end

%%%%%%%%%%

function  p = target(x)

global findMAX
if findMAX
  Z = -peaks;
else
  Z = peaks;
end
r = round(x(1)); c = round(x(2));
if r >= 1 & r <= size(Z,1) & c >= 1 & c <= size(Z,2)
  p = Z(r,c);
else
  p = inf; % invalid
end

