%Modified S-transform
% taking an Earthquake SAC file

%read SAC file-----------------------------------
F=fopen('1010211753osor.sac', 'r','ieee-be');
if (F==-1)
  disp('file access error');
  pause
end

% read header:
head1=fread(F, [5, 14], 'float32');
head2=fread(F, [5, 8], 'int32');
head3=fread(F, [24, 8], 'char');
% read data:
yin=fread(F,'single');  %read signal data
fclose(F);
Tin=0.01*head1(1); %sampling period (sec)
fin=1/Tin;

%preparation for analysis-------------------------
ndc=3;
yd=yin(1:ndc:end); % decimate by ndc
fs=fin/ndc; Ts=Tin*ndc; 
yo=(yd/100)'; %the signal
No=length(yo);
wy=2*pi*fs; %signal frequency in rad/s
to=0:Ts:(No-1)*Ts;

%extract signal segment-----------------------------
ti=0; %initial time of signal segment (sec)
duy=12; %signal segment duration (sec)
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
subplot(2,1,1)
plot(to,yo,'k')
axis([0 to(end) -4 4]);
title('Earthquake signal, complete')
xlabel('sec')
subplot(2,1,2)
plot(tsg,y,'k');
xlabel('sec')
title('selected signal segment')

figure(2)
specgram(y,64,fs);
title('Earthquake signal,a segment');

figure(3)
Sf=0:(2*fs/Ny):(fs/2);
imagesc(tsg,Sf,20*log10(abs(ST)),[-40 -10]); axis xy;
%set(gca,'Ydir','Normal');
title('S-transform of the signal segment');
xlabel('Time'); ylabel('Frequency');