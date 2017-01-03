function demoSLDStraffic
%DEMOSLDSTRAFFIC  switching linear dynamical system traffic flow model
close all;
[a ab ad bd bc cd]=assign(1:6); H=6; S=6; V=2;
% setup the traffic flow transition and emission matrices
for s=1:S
    [sa sb] = assign(ind2subv([3 2] ,s));
    A(:,:,s)=zeros(H,H);
    A(a,a,s)=1;
    A(ab,a,s)=(sa==1)*0.25+(sa==2)*0 + (sa==3)*1;
    A(ad,a,s)=(sa==1)*0.75+(sa==2)*1 + (sa==3)*0;
    A(bd,ab,s)=(sb==1)*0.5+(sb==2)*0;
    A(bc,ab,s)=(sb==1)*0.5+(sb==2)*1;
    A(cd,bc,s)=1;
    
    B(:,:,s)=zeros(V,H);
    B(1,a,s)=1; % flow at a
    B(2,[ad bd cd],s)=1; % flow at d
    
    CovH(:,:,s)=0.01*eye(H); CovH(a,a,s)=1;
    CovV(:,:,s)=1*eye(V);
    pCov(:,:,s)=0.01*eye(H);
    pMean(:,s)=zeros(H,1); pMean(a,1)=20;
    vbias(:,s)=zeros(V,1);
    meanH(:,s)=zeros(H,1);
end
prior=zeros(S,1); prior(a,1)=1; % start with flow at a
stran=condp(rand(S,S)+10*eye(S,S)); % bias the transitions to remain in the same state

% generate some training data:
T=100; s_samp = zeros(1,T); h_samp = zeros(H,T); v_samp = zeros(V,T);
st = randgen(prior); s_samp(1)   = st;
h_samp(:,1) = mvrandn(pMean(:,st), pCov(:,:,st));
for t=2:T
    stm = s_samp(t-1); st  = randgen(stran(:,stm));s_samp(t)   = st;
    h_samp(:,t) = mvrandn(A(:,:,st)*h_samp(:,t-1), CovH(:,:,st));
end
for t=1:T
    st = s_samp(t);
    v_samp(:,t) = mvrandn(B(:,:,st)*h_samp(:,t), CovV(:,:,st)) + vbias(:,st);
end
plot(v_samp','.-'); legend('measured flow a','measured flow d')

for I=1:2
    % Perform approximate Filtered inference:
    [f F alpha w loglik]=SLDSforward(v_samp,A,B,CovH,CovV,zeros(H,S),zeros(V,S),pCov,pMean,stran,prior,I);
    
    for t=1:T; 	[val, idx] = max(alpha(:,t));  maxfwd(t)  = idx; end
    fwd_errors = length(find((s_samp - maxfwd) ~= 0));
    fprintf(1,'[I=%d] #errors after forward pass : %d (%g%%)\n',I, fwd_errors,100*fwd_errors/T);
    
    [mf, dum]=SLDSmargGauss(w,alpha,f,F); % compute the single filtered Gaussian
    disp(['Mean abs deviation in h = ',num2str(mean(mean(abs(h_samp-mf))))]);
    
    % Perform approximate Smoothed inference:
    for doEC=0:1
        inference={'GPB','EC'};
        J=I; [g G gamma u]=SLDSbackward(v_samp,f,F,alpha,w,A,CovH,meanH,stran,I,J,doEC);
        
        for t=1:T; 	[val, idx] = max(gamma(:,t)); maxbwd(t)  = idx; end
        bwd_errors = length(find((s_samp - maxbwd) ~= 0));
        fprintf(1,'%s [I=%d,J=%d] #errors after backward pass : %d (%g%%)\n',...
            inference{doEC+1},I,J,bwd_errors,100*bwd_errors/T);
                
        % Compute the mean flows using the posterior mixture of Gaussians:
        s_sampmat=zeros(S,T); for t=1:T;	s_sampmat(s_samp(t),t)=1; end
        [mg, dum]=SLDSmargGauss(u,gamma,g,G); % compute the single smoothed Gaussian
        
        disp(['Mean abs deviation in h = ',num2str(mean(mean(abs(h_samp-mg))))]);
    end
end
figure; subplot(2,1,2); imagesc(1-s_sampmat); colormap bone; title('true switches');subplot(2,1,1);
plot(h_samp','.-'); title('true h'); legend('a', 'ab', 'ad', 'bd', 'bc', 'cd','orientation','horizontal');

figure; subplot(2,1,2); imagesc(1-alpha); colormap bone; title('filtered switches');subplot(2,1,1);
plot(mf','.-'); title('filtered h'); legend('a', 'ab', 'ad', 'bd', 'bc', 'cd','orientation','horizontal');

figure; subplot(2,1,2); imagesc(1-gamma); colormap bone; title('smoothed switches');subplot(2,1,1);
plot(mg','.-');  title('smoothed h'); legend('a', 'ab', 'ad', 'bd', 'bc', 'cd','orientation','horizontal');