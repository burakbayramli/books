function demoHopfield
%DEMOHOPFIELD demo of learning a sequence using a Hopfield net
V = 100; % number of neurons
T = 20;  % length of sequence
for t = 1:T
	pat = zeros(V,1); pat(t:t+10,1)=1;
	v(:,t) = 2*pat-1; % +/-1 encoding
end

figure
% Hebb rule:
Whebb = zeros(V,V);
for t = 1:T-1
	Whebb = Whebb + v(:,t+1)*v(:,t)';
end
Whebb = Whebb./V;
Whebb = Whebb - diag(diag(Whebb));

% Pseudo Inverse rule :
vhat = v(:,2:T);vp=v(:,1:T-1);
WPI= vhat*inv(vp'*vp)*vp';

% ML Hebb:
epochs = 20; eta = 0.005;
vv{1}=v; % just a single sequence
[W,theta,loglik]=HebbML(vv,eta,epochs);

% see if we've learned the sequence :
% flip noise percent of the first state:
noise=0.1; 
noise_vec = (rand(V,1)<noise); % randomly perturbed initial condition
v1_noise = v(:,1); v1_noise(find(noise_vec)) = -v1_noise(find(noise_vec));

% Reconstructions using deterministic dynamics:
vt(:,1)=v1_noise; vt_hebb(:,1)=v1_noise; vt_PI(:,1)=v1_noise;
for t=2:T
	vt(:,t) = 2*(sigma(W*vt(:,t-1) + theta)>0.5)-1;  % ML Hebb
	vt_hebb(:,t)=2*(sigma(Whebb*vt_hebb(:,t-1))>0.5)-1; % Standard Hebb
	vt_PI(:,t)=2*(sigma(WPI*vt_PI(:,t-1))>0.5)-1; % Pseudo Inverse
end
subplot(1,4,1); imagesc(v); colormap('gray'); title('original')
subplot(1,4,2); imagesc(vt); colormap('gray'); title('ML Hebb')
subplot(1,4,3); imagesc(vt_hebb); colormap('gray'); title('Hebb')
subplot(1,4,4); imagesc(vt_PI); colormap('gray'); title('PI')