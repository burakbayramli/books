% test 6 faces after eigenface analysis

function ANeigf(H,W,to,MF,eigF,a,fs,numfig);

tp=zeros(H*W,6); tfs=zeros(a,6);

% prepare faces and project onto face space
for nt=1:6,
   aux=to(:,nt)-MF; %subtract average face and normalize
   tp(:,nt)=aux/norm(aux);
   tfs(:,nt)=eigF'*tp(:,nt); %project onto face space
end;

% compute minimum distance respect established faces
% in face space
Tmd=zeros(6,1); Tix=zeros(6,1);
for nt=1:6,
  ips=100; mdn=1;
  for ni=1:a,
    aux=norm(tfs(:,nt)-fs(:,ni));
    if aux<ips, ips=aux; mdn=ni; end;
  end;
  Tmd(nt)=ips; Tix(nt)=mdn;  %results for 1 of 6 faces
end;  

% display minimum distances in face space
figure(numfig)
polar(0,1,'y'); hold on; 
for nt=1:6,
   ag=((nt-1)*2*pi)/6; %angle
   polar([ag ag],[0 Tmd(nt)],'k*-');
end; 
title('distances in face space: 6 faces');

% display distances between prepared and reconstructed faces
trec=zeros(H*W,6);
figure(numfig+1)
polar(0,1.2,'y'); hold on;
for nt=1:6,
   aux=eigF*tfs(:,nt); %reconstruction
   trec(:,nt)=aux/norm(aux);
   ag=((nt-1)*2*pi)/6; %angle
   R=norm(tp(:,nt)-trec(:,nt)); %distance for one face
   polar([ag ag],[0 R],'k*-');
end; 
title('distances between prepared and reconstructed 6 faces');

% display of 6 test faces and their reconstruction
figure(numfig+2)
for nt=1:6,
   subplot(2,6,nt)
   aux=reshape(tp(:,nt),H,W);   
   imshow(aux,[]);
   subplot(2,6,nt+6)
   aux=reshape(trec(:,nt),H,W);   
   imshow(aux,[]);
end;

