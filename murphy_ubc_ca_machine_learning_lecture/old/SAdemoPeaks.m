function SAdemoPeaks()
% Demo of Simulated Annearling for finding the most probable
% point of a peaky 2D surface

Nsamples = 5000;       
Sigma_prop = 5^2*eye(2); % 2D proposal (5^2 = variance)



Z = peaks; % 49x49 surface surface

% to find the global min,just invert
%Z = -Z;

Z = Z + abs(min(Z(:))); % ensure lowest point is non negative since probabilities must be positive
Z = Z + 1; % avoid 0 probabilities

% find optimum by exhaustive search
M = max(Z(:));
[row,col] = find(Z==M) % max peaks row 38, col 25 min peaks row 12, col 27



targetArgs = {Z};
proposalArgs = {Sigma_prop};

seed = 1; randn('state', seed); rand('state', seed);
xinit = [25,25]; % initial state is in middle of grid

initTemp = 1;
coolingFactor = 0.995;
for t=1:Nsamples
  if t==1
    temp(1) = initTemp;
  else
    temp(t) = temp(t-1) * coolingFactor; % cool down
  end
end

[samples, naccept] = SA(@target, @proposal, xinit, Nsamples,  targetArgs, proposalArgs, ...
		  [], temp);



% plot the histogram of samples 
N_bins = 50; 
Ns = [100 500 1000 Nsamples];
fig1 = figure;
fig2 = figure;
for i=1:4
  T = Ns(i);
  H = hist2d(samples(1:T,2), samples(1:T,1), N_bins); % Y,X
  figure(fig1)
  subplot(2,2,i)
  surf(H)
  xlabel('x'); ylabel('y')
  title(sprintf('temp = %4.4f', temp(Ns(i))))
  figure(fig2)
  subplot(2,2,i)
  imagesc(H); colorbar; axis xy; axis square
  xlabel('x'); ylabel('y')
  title(sprintf('temp = %4.4f', temp(Ns(i))))
end

    
% plot the cooled target
figure;
temps = [1 temp(200) temp(500)];
for i=1:3
  T = temps(i);
  Zt = Z .^ (1/T);
  subplot2(3,2,i,1)
  surf(Zt); xlabel('x'); ylabel('y')
  title(sprintf('temp = %4.4f', T))
  subplot2(3,2,i,2)
  imagesc(Zt); axis square; axis xy; colorbar
  title(sprintf('temp = %4.4f', T))
end

if 1
folder = 'C:\kmurphy\Teaching\cs340-fall06\homeworks\hw6';
figure(1); 
print(gcf, '-depsc', fullfile(folder, 'SApeaksSurfMax.eps'))
figure(2)
print(gcf, '-depsc', fullfile(folder, 'SApeaksHistMax.eps'))
figure(3)
print(gcf, '-depsc', fullfile(folder, 'SApeaksTargetMax.eps'))
end

if 0
folder = 'C:\kmurphy\Teaching\cs340-fall06\homeworks\hw6';
figure(1); 
print(gcf, '-depsc', fullfile(folder, 'SApeaksSurfMin.eps'))
figure(2)
print(gcf, '-depsc', fullfile(folder, 'SApeaksHistMin.eps'))
figure(3)
print(gcf, '-depsc', fullfile(folder, 'SApeaksTargetMin.eps'))
end

%%%%%%%%%%

function  p = target(x, Z)

Z = log(Z);
r = round(x(1)); c = round(x(2));
if r >= 1 & r <= size(Z,1) & c >= 1 & c <= size(Z,2)
  p = Z(r,c);
else
  p = -10000; % invalid
end


function xp = proposal(x, Sigma_prop)
xp = x + mvnrnd([0;0], Sigma_prop);


