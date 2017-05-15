% Blind Source Separation using ICA
% Example of two music records

%read source signals
[s1,fs1]=wavread('wind1.wav'); %read wav file
[s2,fs2]=wavread('b1.wav'); %read wav file
s1=s1-mean(s1); %zero-mean
s2=s2-mean(s2); %"  "  "
vr1=var(s1); s1=s1/sqrt(vr1); %variance=1
vr2=var(s2); s2=s2/sqrt(vr2); %"  "  "

% Plot histogram of each source signal - 
% this approximates pdf of each source.
figure(1);
subplot(1,2,1); hist(s1,50); 
title('histogram of sound1');
subplot(1,2,2); hist(s2,50); 
title('histogram of sound2'); 
drawnow;

s=[s1,s2]'; %combine sources

%mix of sources
N=length(s1); 
M=[0.6 0.4; 0.4 0.6]; %example of mixing matrix
x=M*s; %mixed signals

%initialization
W=eye(2,2); %unmixing matrix
y=W*x; %estimated sources

%prepare iterations
nit=50;
delta=0.5;
etpy=zeros(1,nit); %for entropies
grd=zeros(1,nit); %for gradients

disp('working...'); %ask for patience 

%iterations
for nn=1:nit,
   y=W*x; %estimated sources
   mhy=tanh(y); %for maximum entropy estimation
   deW = abs(det(W));
   h =((sum(sum(mhy)))/N) + (0.5*log(deW)); %entropy
   g = inv(W') - ((2*mhy*x')/N); %gradient matrix
   W=W+(delta*g); %update of the unmixing matrix
   etpy(nn)=h; grd(nn)=norm(g(:)); %save intermediate values
end;
   
%display
figure(2)
plot(etpy,'k'); hold on;
plot(grd,'r')
title('entropy, and norm of gradient matrix'); 
xlabel('number of iterations');

%sounds
disp('one of the sound mixes');
soundsc(x(1,:));
disp('press bar when finished');
pause %hit a key!
disp('first extracted sound');
soundsc(y(1,:));
disp('press bar when finished');
pause %hit a key!
disp('second extracted sound');
soundsc(y(2,:));

%print correlations between sources and estimations
cr=corrcoef([y' s']);
cr(3:4,1:2)


  