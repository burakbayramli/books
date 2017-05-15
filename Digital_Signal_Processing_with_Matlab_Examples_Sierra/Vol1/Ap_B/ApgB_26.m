%Modified S-transform
% Lung sound: wheezing

% the signal
[yin,fin]=wavread('wheezing.wav'); %read wav file

ndc=4; %decimation value
yo=yin(1:ndc:end); %signal decimation
No=length(yo);
fs=fin/ndc;

wy=2*pi*fs; %signal frequency in rad/s
Ts=1/fs; %time interval between samples;
to=0:Ts:(No-1)*Ts;

%extract signal segment-----------------------------
ti=1.1; %initial time of signal segment (sec)
duy=0.8; %signal segment duration (sec)
tsg=ti:Ts:(duy+ti); %time intervals set
Ni=1+(ti*fs); %number of the initial sample

aux=length(tsg); %how many samples in signal segment
y=yo(Ni:(Ni+aux-1))'; %the signal segment (transpose)
%force even length
if mod(aux,2)>0, 
    y=y(1:end-1); 
    tsg=tsg(1:end-1);
end;
Ny=length(y); %length of signal segment
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
subplot(3,1,1)
plot(to,yo,'k')
axis([0 10 -1.1 1.1]);
title('Respiration signal, complete')
subplot(3,1,2)
plot(tsg,y,'k');
title('selected signal segment')
subplot(3,1,3)
plot(tsg(1300:1800),y(1300:1800),'k'); %signal specific (edit)
axis([1.56 1.76 -0.12 0.12]);
title('zoom on wheezing part')

figure(2)
specgram(y,256,fs);
title('Respiration signal,a segment');

figure(3)
Sf=0:(2*fs/Ny):(fs/2);
imagesc(tsg,Sf,20*log10(abs(ST)),[-70 0]); axis xy;
%set(gca,'Ydir','Normal');
title('S-transform of the signal segment');
xlabel('Time'); ylabel('Frequency');