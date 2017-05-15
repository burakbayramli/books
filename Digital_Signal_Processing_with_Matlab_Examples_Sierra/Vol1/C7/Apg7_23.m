%Modified S-transform
%example with linear chirp

% the signal
tiv=0.005;
t=0:tiv:(3-tiv);
fs=1/tiv;
yc=exp(-j*70*(t.^2));
y=real(yc);
Ny=length(y); %even length
m=Ny/2;

% The transform-------------------------------------

% preparation:
f=[0:m -m+1:-1]/Ny; %frequencies vector
S=fft(y); %signal spectrum

% Form a matrix of Gaussians (freq. domain)
q=[1./f(2:m+1)]';
k=1+(5*abs(f));
W=2*pi*repmat(f,m,1).*repmat(q,1,Ny);
for nn=1:m,
    W(nn,:)=k(nn)*W(nn,:); %modified S-transform
end
MG=exp((-W.^2)/2); % the matrix of Gaussians

% Form a matrix with shifted FFTs
Ss=toeplitz(S(1:m+1)',S);
Ss=[Ss(2:m+1,:)]; %remove first row (freq. zero)

% S-transform
ST=ifft(Ss.*MG,[],2);
st0=mean(y)*ones(1,Ny); %zero freq. row
ST=[st0;ST]; %add zero freq. row

% display ------------------------------------------------
figure(1)
specgram(y,64,fs);
title('spectrogram of the linear chirp');

figure(2)
Sf=0:(2*fs/Ny):(fs/2);
imagesc(t,Sf,abs(ST)); axis xy;
%set(gca,'Ydir','Normal');
title('S-transform of the linear chirp');
xlabel('Time'); ylabel('Frequency');