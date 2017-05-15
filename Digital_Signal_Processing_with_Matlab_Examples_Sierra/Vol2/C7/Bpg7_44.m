% example of Fisherfaces for recognition

%read face database (120 faces, of 112x92=10304 length each)
AA=zeros(10304,120); %faces are columns of the matrix
fid=fopen('AAface.bin','r'); aux=fread(fid);
AA=reshape(aux,10304,120);

% will choose 6x9 faces of 92x112 size
W=92; H=112;
C=6; %number of classes (6 people)
R=9; %number of faces per class
% faces are contained in matrix "AA"

% Preparing faces ------------------------------

% form matrix with image columns
a=C*R; Or=zeros(H*W,a); %54 columns
nn=41;ns=1; %select 60 faces
for nc=1:a,
    aux=AA(:,nn);
    Or(:,nc)=aux;
    if ns==9 %skip next face
        ns=1; nn=nn+2;
    else    
        ns=ns+1; nn=nn+1;
    end;
end;

% get average face
MF=mean(Or,2);
% subtract average face from each face
% and normalize faces
PF=zeros(H*W,a); nZ=zeros(1,a);
for nc=1:a,    
    aux=Or(:,nc)-MF;
    PF(:,nc)=aux/norm(aux);
    nZ(nc)=norm(aux); %keep for face recovery
end;

% Eigenfaces algorithm ----------------------------

% get eigenvectors
[V D]=eig(PF'*PF); 

% sort in descending order
aux=diag(D); 
[aux2, sx]=sort(-1.*aux);
eigL=aux(sx); eigV=V(:,sx);

% project to obtain eigenfaces
aux=PF*eigV;
eigF=aux/norm(aux);

% Select the first 48 (54-6) eigenfaces
K=a-C; %number of selected eigenfaces
Wp=eigF(:,1:K); %form a matrix

% build face space by projection
fs=zeros(K,a);
for nf=1:a,
    fs(:,nf)=Wp'*PF(:,nf);
end;
 
 % LDA computations
 mu=mean(fs,2); %total mean in face space
 mcl=zeros(K,C); % mean of each class
 Sw=zeros(K,K); % within scatter matrix
 Sb=zeros(K,K); % between scatter matrix
 
 for nn=1:C,
    ix=((nn-1)*R+1):(nn*R);
    mcl(:,nn)=mean(fs(:,ix),2);
    aux=zeros(K,K);
    for j=ix,
       aux=aux+((fs(:,j)-mcl(:,nn))*(fs(:,j)-mcl(:,nn))');
    end;
    Sw=Sw+aux;
    Sb=Sb+((mcl(:,nn)-mu)*(mcl(:,nn)-mu)');       
 end; 
 
% solve general eigenvalue problem
[Wx D]=eig(Sb,Sw);
% sort in descending order
aux=diag(real(D)); 
[aux2, sx]=sort(-1.*aux);
Waux=real(Wx(:,sx));
Wl=Waux(:,1:(C-1)); % select C-1 largest eigenvectors

WF=Wp*Wl; %Fisherfaces

% show Fisherfaces
figure(1)
for nn=1:C-1,
   subplot(1,5,nn)
   FF=reshape(WF(:,nn),H,W);
   imshow(FF,[]);
end; 
title('Fisherfaces');

% Testing -------------------------------
% build (Fisher) face space by projection
Ffs=zeros(C-1,a);
for nf=1:a,
    Ffs(:,nf)=WF'*PF(:,nf);
end;
 
% reconstruction
RF=zeros(H*W,a);
for nn=1:a,
    aux=WF*Ffs(:,nn); %reconstruction
    rf=aux/norm(aux);
    RF(:,nn)=rf; %save reconstructed faces
 end;
 
% plot face space clusters, taking the two larger components
figure(2) 
for nf=1:a,
   aux1=abs(Ffs(:,nf));
   [aux2,ix]=sort(aux1);
   aux=Ffs(ix,nf);
   if nf<10, plot(aux(5),aux(4),'ko'); hold on; end;
   if (nf>9 & nf<19),plot(aux(5),aux(4),'bo'); end;
   if (nf>18 & nf<28),plot(aux(5),aux(4),'ro'); end;
   if (nf>27 & nf<37),plot(aux(5),aux(4),'go'); end;
   if (nf>36 & nf<46),plot(aux(5),aux(4),'mo'); end;
   if (nf>45 & nf<55),plot(aux(5),aux(4),'co'); end;
end; 
axis([-0.2 0.2 -0.15 0.15]);
title('clusters in Fisher face space');
xlabel('component 1');
ylabel('component 2');

%----------------------------------------------------------------------
% test the 10th face for each row
% (these  6 faces have not been used during training)

%extract faces
to=zeros(H*W,6); 
to(:,1)=AA(:,50); to(:,2)=AA(:,60); to(:,3)=AA(:,70);
to(:,4)=AA(:,80);to(:,5)=AA(:,90); to(:,6)=AA(:,100);

numfig=3;
ANfish(H,W,C,to,MF,WF,a,Ffs,numfig);

%----------------------------------------------------------------------
% test 6 new faces, corresponding to new people not yet considered

%extract faces
to=zeros(H*W,6); 
to(:,1)=AA(:,9); to(:,2)=AA(:,19); to(:,3)=AA(:,29);
to(:,4)=AA(:,39);to(:,5)=AA(:,109); to(:,6)=AA(:,119);

numfig=4;
ANfish(H,W,C,to,MF,WF,a,Ffs,numfig);

