function demoLSI
%DEMOLSI demo of latent semantic indexing

close all
p1 =[0 1 0.8 0.8 0.7 0.5]; % [influenza, flu, headache,nose,temperature,bed] occurrence probabilities
p2 =[1 0 0.8 0.8 0.7 0.5]; % [influenza, flu, headache,nose,temperature,bed] occurrence probabilities

p1 = [p1 0.05*ones(1,4)]'; % add on some random words with small occurrence
p2 = [p2 0.05*ones(1,4)]'; % add on some random words with small occurrence
p3=0.1*ones(length(p1),1); % random background  topic with small occurrence
p4=[0 0.1 0.1 0.1 0 0.2 0.8 0.8 0.9 0.9]'; % another `pet' topic

D = length(p1); % number of words in the dictionary
N=2000; % number of documents to sample
x1= rand(D,N/4)<repmat(p1,1,N/4);
x2= rand(D,N/4)<repmat(p2,1,N/4);
x3= rand(D,N/4)<repmat(p3,1,N/4);
x4=rand(D,N/4)<repmat(p4,1,N/4);

X=real([x1 x2 x3 x4]); % includes the background topic

imagesc(X); colormap gray; title('data')

[Y,E,L,m,Xtilde]=pca(X,3,0); % do PCA without the bias term

figure
opts.coloursRGB=[0 0 0; 0.8 0.8 0.8];
hinton(E*diag(L),opts); % scale the eigenvectors by their eigenvalue
title('PCA eigenvectors')

figure; imagesc(Y(1:3,:));colormap gray; title('PCA coordinates')

% Now to PLSA
opts.tol =0.0000001;
opts.plotprogress=0;
opts.maxit =2000;
opts.randinit=1;

[tpxgz,tpygz,tpz,tpxy]=plsa(X./sum(X(:)),2,opts);
figure; imagesc(tpygz');colormap gray; title('PLSA projections (2 components)')
figure; hinton(tpxgz,opts); colormap gray; title('PLSA basis (2 components)');
tpz

[tpxgz,tpygz,tpz,tpxy]=plsa(X./sum(X(:)),3,opts);
figure; imagesc(tpygz');colormap gray; title('PLSA projections (3 components)')
figure; hinton(tpxgz,opts); colormap gray; title('PLSA basis (3 components)');
tpz
