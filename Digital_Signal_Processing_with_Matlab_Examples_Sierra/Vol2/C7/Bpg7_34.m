% Kernel PCA example
% Nutritional value of food

N=700; %number of data (N<962)

% Prepare data ---------------------------------
Gfood=dlmread('food.txt',' ' );
Dfood=Gfood(:,1:7);%extract first 7 columns
OF=Dfood(1:N,:); %save original data for punctual analysis

DF=zeros(N,6); %space for data matrix
%divide by the 6th column (weight):
for n=1:5,
DF(:,n)=Dfood(1:N,n)./Dfood(1:N,6); 
end;
DF(:,6)=Dfood(1:N,7)./Dfood(1:N,6); %"  "
%subtract mean in each column
me=mean(DF,1);
aux1=DF-repmat(me,N,1);
%divide by standard deviation in each column
aux2=std(aux1,0,1);
DF=aux1./repmat(aux2,N,1);

[v,ix]= max(OF); %foods with peak values

% Principal components (no kernel) ---------------------------
[U,S,V]=svd(DF); % V contains the 6 principal components
P=V'*DF'; % Scores

%scatter plot of 2 first scores
figure(1)
scatter(P(1,:),P(2,:),16,'k'); hold on;
% special food cases:
plot(P(1,ix(2)),P(2,ix(2)),'rs','MarkerSize',16); %Max. calories
plot(P(1,ix(4)),P(2,ix(4)),'bd','MarkerSize',16); %Max. protein
plot(P(1,ix(6)),P(2,ix(6)),'cs','MarkerSize',16); %Max. sat. fat
title('Scatter plot of 2 first PCA scores');
xlabel('score 1'); ylabel('score 2');


% Kernel PCA -------------------------------------------------

sigma=0.7;
kp=2*(sigma^2); % kernel parameter
K=zeros(N,N);

%Kernel matrix computation
for i=1:N,
  for j=i:N,
    K(i,j) = exp(-(norm(DF(i,:)-DF(j,:))^2)/kp);    
    K(j,i) = K(i,j);    
  end
end

un = ones(N,N)/N;
% centering
Ku = K - un*K - K*un + un*K*un;

%Using the Kernel
[eV,eD]=eigs(Ku); %only a few
reD=diag(eD);
nc=6; %choose a subset
neV=zeros(N,nc); 
%normalize:
for i=1:nc,
   neV(:,i)=eV(:,i)/sqrt(reD(i));
end;  
sco=zeros(N,nc);
sco=Ku*neV; %KPCA scores

% scatter plot of 2 first scores
figure(2)
scatter(sco(:,1),sco(:,2),16,'k'); hold on;
% special food cases:
plot(sco(ix(2),1),sco(ix(2),2),'rs','MarkerSize',16); %Max. calories
plot(sco(ix(4),1),sco(ix(4),2),'bd','MarkerSize',16); %Max. protein
plot(sco(ix(6),1),sco(ix(6),2),'cs','MarkerSize',16); %Max. sat. fat
title('Scatter plot of 2 first KPCA scores');
xlabel('score 1'); ylabel('score 2');

