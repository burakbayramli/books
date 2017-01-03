%function demoCCA
H=1; % latent dimension
N=100; % number of datapoints

% make some training data:
A0=randn(15,H); B0=randn(30,H); h= (rand(H,N)).^5;
X=A0*h; Y=B0*h;

[A,B]=cca(X,Y,H); % learn from the training data

figure(1);subplot(2,1,2);imagesc(Y);subplot(2,1,1);imagesc(X);title('training data'); colormap gray;
figure(2);subplot(2,1,2);plot(B0,'b-s');subplot(2,1,1);plot(A0,'r-o');title('true model'); colormap gray;
figure(3);subplot(2,1,2); plot(B,'b-s');subplot(2,1,1);plot(A,'r-o');title('learned model'); colormap gray;
