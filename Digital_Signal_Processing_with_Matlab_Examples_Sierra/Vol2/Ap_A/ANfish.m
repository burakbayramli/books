% test 6 faces after Fisherface analysis

function ANfish(H,W,C,to,MF,WF,a,Ffs,numfig);

tp=zeros(H*W,6); tfs=zeros(C-1,6);

% prepare faces and project onto face space
for nt=1:6,
   aux=to(:,nt)-MF; %subtract average face and normalize
   tp(:,nt)=aux/norm(aux);
   tfs(:,nt)=WF'*tp(:,nt); %project onto face space
end;

% compute minimum distance respect established faces
% in face space
Tmd=zeros(6,1); Tix=zeros(6,1);
for nt=1:6,
  ips=100; mdn=1;
  for ni=1:a,
    aux=norm(tfs(:,nt)-Ffs(:,ni));
    if aux<ips, ips=aux; mdn=ni; end;
  end;
  Tmd(nt)=ips; Tix(nt)=mdn;  %results for 1 of 6 faces
end;  

% display minimum distances in face space
figure(numfig)
polar(0,0.2,'y'); hold on; 
for nt=1:6,
   ag=((nt-1)*2*pi)/6; %angle
   polar([ag ag],[0 Tmd(nt)],'k*-');
end; 
title('distances in face space: 6 faces');


