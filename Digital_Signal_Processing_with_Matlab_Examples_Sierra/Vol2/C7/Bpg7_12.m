% Comparison of PCA and ICA components
% example of two mixed speeches

%read two sound files
[a,fs1]=wavread('spch1.wav'); %read wav file
[b,fs1]=wavread('spch2.wav'); % "  "  "
R=2; %reduce data size for clearer picture
a=decimate(a,R);
b=decimate(b,R);
s1=(a-mean(a))'; %zero-mean
s2=(b-mean(b))'; % " "  "
vr1=var(s1); s1=s1/sqrt(vr1); %variance=1
vr2=var(s2); s2=s2/sqrt(vr2); %"  "  "

s=[s1;s2]; %combine sources

%mix of sources
N=length(s1); 
M=[0.7 0.3; 0.2 0.8]; %example of mixing matrix
x=M*s; %mixed signals

% PCA computation
A=x'/sqrt(N-1);
%singular value decomposition
[U,S,V]=svd(A); %V contains principal components 

%ICA computation
U=inv(M); 
w1=U(1,:);
w2=U(2,:);

%display
figure(1)
%scatterplot and PCA components
plot(x(1,:),x(2,:),'k.'); hold on; %scatterplot
L=3;
plot([-L*V(1,1) L*V(1,1)],[-L*V(2,1) L*V(2,1)],'r'); %PCA components
plot([-L*V(1,2) L*V(1,2)],[-L*V(2,2) L*V(2,2)],'r');
%axes
plot([-L L],[0 0],'k');
plot([0 0],[-L L],'k');
axis([-L  L -L L]);
title('scatterplot of 2 mixed speeches: PCA components'); 
xlabel('x'); ylabel('y');

figure(2)
%scatterplot and ICA components (perpendicular to w)
plot(x(1,:),x(2,:),'k.'); hold on; %scatterplot
L=3;
plot([-L*w1(2) L*w1(2)],[L*w1(1) -L*w1(1)],'r'); %ICA components
plot([-L*w2(2) L*w2(2)],[L*w2(1) -L*w2(1)],'r');
%axes
plot([-L L],[0 0],'k');
plot([0 0],[-L L],'k');
axis([-L  L -L L]);
title('scatterplot of 2 mixed speeches: ICA components'); 
xlabel('x'); ylabel('y');
