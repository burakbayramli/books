% Display of B-splines

%space for splines
nz=100; tiv=1/nz;
b=zeros(4,5*nz); %space for splines

% the box spline (n=0)
b0=ones(1,nz);

% spline n=1
N=1;
b1L=(0:tiv:(N-tiv)).*b0; b1D=(N:-tiv:tiv).*b0;
b(N,1:((N+1)*nz))=[b1L b1D];

% splines 2..5
for N=2:5,
bL=(0:tiv:(N-tiv)).*b(N-1,1:N*nz); bD=(N:-tiv:tiv).*b(N-1,1:N*nz);
b(N,1:((N+1)*nz))=(1/N)*[bL(1:nz) bL((nz+1):N*nz)+bD(1:((N-1)*nz)) bD((((N-1)*nz)+1):N*nz)];
end;

figure(1)
subplot(2,3,1)
t=(0:tiv:(1-tiv)); plot(t,b0,'k',[1 1],[1 0],'k'); title('B-spline 0');
axis([0 1.2 0 1.2]);
subplot(2,3,2)
t=(0:tiv:(2-tiv)); plot(t,b(1,1:2*nz),'k'); title('B-spline 1');
axis([0 2 0 1]);
subplot(2,3,3)
t=(0:tiv:(3-tiv));plot(t,b(2,1:3*nz),'k'); title('B-spline 2');
axis([0 3 -0.1 0.8]);
subplot(2,3,4)
t=(0:tiv:(4-tiv));plot(t,b(3,1:4*nz),'k'); title('B-spline 3');
axis([0 4 -0.1 0.8]);
subplot(2,3,5)
t=(0:tiv:(5-tiv));plot(t,b(4,1:5*nz),'k'); title('B-spline 4');
axis([0 5 -0.1 0.8]);
subplot(2,3,6)
t=(0:tiv:(6-tiv));plot(t,b(5,1:6*nz),'k'); title('B-spline 5');
axis([0 6 -0.1 0.8]);

