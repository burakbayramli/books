function demoMRFclean
%DEMOMRFCLEAN demo of image denoising using a binary state Markov Random Field
figure
Gx=28; Gy=28; N=Gx*Gy;
st = reshape(1:N,Gx,Gy); % assign each grid point a state

W0=zeros(N,N);
for x = 1:Gx
    for y = 1:Gy
        if validgridposition(x+1,y,Gx,Gy); W0(st(x+1,y),st(x,y))=1; end
        if validgridposition(x-1,y,Gx,Gy); W0(st(x-1,y),st(x,y))=1; end
        if validgridposition(x,y+1,Gx,Gy); W0(st(x,y+1),st(x,y))=1; end
        if validgridposition(x,y-1,Gx,Gy); W0(st(x,y-1),st(x,y))=1; end
    end
end

load xclean; xclean=xclean>0; % binary clean image
subplot(1,3,1); imagesc(reshape(xclean,28,28)'); colormap bone; title('clean');
n = rand(N,1)>0.85;
xnoisy = xclean; xnoisy(n>0)=(1-xclean(n>0)); % flip pixels
subplot(1,3,2); imagesc(reshape(xnoisy,28,28)'); title('noisy');

b = 2*xnoisy-1; % bias to favour the noisy image
W=10*W0; % preference for neighbouring pixels to be in same state
opts.maxit=1; opts.minit=1; opts.xinit=xnoisy(:);
for loop=1:20
    xrestored = binaryMRFmap(W,b,1,opts);
    subplot(1,3,3); imagesc(reshape(xrestored,28,28)'); title(['restored ' num2str(loop)]); drawnow
    opts.xinit=xrestored;
end