% example of Eigenfaces for recognition
% test part

%read face database (120 faces, of 112x92=10304 length each)
AA=zeros(10304,120); %faces are columns of the matrix
fid=fopen('AAface.bin','r'); aux=fread(fid);
AA=reshape(aux,10304,120);

% will choose 6x9 faces of 92x112 size
W=92; H=112;
% faces are contained in matrix "AA"

% Eigenfaces algorithm-------------------------------
% form matrix with image columns
a=6*9; Or=zeros(H*W,a); %54 columns
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

% get eigenvectors
[V D]=eig(PF'*PF); 

% sort in descending order
aux=diag(D); 
[aux2, sx]=sort(-1.*aux);
eigL=aux(sx); eigV=V(:,sx);

% project to obtain eigenfaces
aux=PF*eigV;
eigF=aux/norm(aux);

%----------------------------------------------------------------------
% build face space by projection of each face
fs=zeros(a,a);
for nf=1:a,
    fs(:,nf)=eigF'*PF(:,nf);
end;

% compute the threshold in face space
th=0;
for ni=1:a,
    for nj=ni:a,
       aux=norm(fs(:,ni)-fs(:,nj));       
       if aux>th,th=aux; end;
    end;
end;
TH=0.5*th; %threshold

% reconstruction from eigenfaces -------------------------
RF=zeros(H*W,a);
for nn=1:a,
    aux=eigF*fs(:,nn); %reconstruction
    rf=aux/norm(aux);
    RF(:,nn)=rf; %save reconstructed faces
end;

% the worst reconstruction
aux=0; won=1;
for nn=1:a,
   R=norm(PF(:,nn)-RF(:,nn)); %distance for one face
   if R>aux, aux=R; won=nn; end;
end; 
wR=aux; %worst distance 

% display distances between prepared and reconstructed faces
figure(1)
polar(0,1.2,'y'); hold on;
for nn=1:a,
   ag=((nn-1)*2*pi)/a; %angle
   R=norm(PF(:,nn)-RF(:,nn)); %distance for one face
   polar([ag ag],[0 R],'k*-');
end; 
title('distances between prepared and reconstructed faces');

% display complete reconstruction (adding average face) ----------------
figure(2)
cRF=zeros(H*W,a); nc=1;
for nn=1:a,
   aux=nZ(nn); %recover normalization factor
   cRF(:,nn)=aux*RF(:,nn)+MF; %add average face
   subplot(6,9,nn)
   aux=reshape(cRF(:,nn),H,W);
   imshow(aux,[])
end;


%----------------------------------------------------------------------
% test the 10th face for each row
% (these  6 faces have not been used during training)

%extract faces
to=zeros(H*W,6); 
to(:,1)=AA(:,50); to(:,2)=AA(:,60); to(:,3)=AA(:,70);
to(:,4)=AA(:,80);to(:,5)=AA(:,90); to(:,6)=AA(:,100);

ANeigf(H,W,to,MF,eigF,a,fs,3);

%----------------------------------------------------------------------
% test 6 new faces, corresponding to new people not yet considered
% (this part of the program is a duplicate of previous part)

%extract faces
to=zeros(H*W,6); 
to(:,1)=AA(:,9); to(:,2)=AA(:,19); to(:,3)=AA(:,29);
to(:,4)=AA(:,39);to(:,5)=AA(:,109); to(:,6)=AA(:,119);

ANeigf(H,W,to,MF,eigF,a,fs,6);

%--------------------------------
% print worst recosntruction and Threshold
wR
TH
