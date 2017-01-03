function demoHopfieldLatent
T = 15; % length of sequence
V = 10; % number of neurons in at each time
H = 8; %number of latent variabels at each time
v = randn(V,T)>0; % random sequence

opts.plotprogress=1; opts.its=200; opts.eta=0.1;
[A B C D]=HopfieldHiddenNL(v,H,opts);

% reconstruction:
initnoise = 0.1;vr(:,1)=v(:,1);
toflip = find(rand(V,1)<initnoise); vr(toflip,1) = 1-vr(toflip,1); % flip initial bits:
hr(:,1)=zeros(H,1);
for t = 1:T-1
    at=D*vr(:,t) + C*hr(:,t);
    vr(:,t+1) = (at>0);
    hr(:,t+1)= 2*sigma(A*hr(:,t)+B*vr(:,t))-1;
end
figure(2); subplot(2,1,1); imagesc(v); colormap('gray'); title('original sequence')
subplot(2,1,2); imagesc(vr); colormap('gray'); title('reconstructed sequence')