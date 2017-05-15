% example of Eigenfaces for recognition
% analysis part

%read face database (120 faces, of 112x92=10304 length each)
AA=zeros(10304,120); %faces are columns of the matrix
fid=fopen('AAface.bin','r'); aux=fread(fid);
AA=reshape(aux,10304,120);

% will choose 6x9 faces of 92x112 size
W=92; H=112;
% faces are contained in matrix "AA"

% display mosaic of selected faces----------------------------
figure(1)
nn=1; M=zeros(H*6,W*9);
for L=5:10,  %select 6 rows
    for C=1:9, %choose 9 faces in each row
        nn= 10*(L-1); nn=nn+C;
        fa=reshape(AA(:,nn),H,W);
        a=1+((L-5)*H); b=a+H-1; xl=a:b;
        c=1+((C-1)*W); d=c+W-1; xc=c:d;
        M(xl,xc)=fa;
    end;
end;
imshow(M,[]);
title('Set of training faces');

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
% and normalize
PF=zeros(H*W,a);
for nc=1:a,    
    aux=Or(:,nc)-MF;
    PF(:,nc)=aux/norm(aux);
 end;
 
% display average face
figure(2)
aux=reshape(MF,H,W);
imshow(aux,[]);
title('average face');

% get eigenvectors
[V D]=eig(PF'*PF); 

% sort in descending order
aux=diag(D); 
[aux2, sx]=sort(-1.*aux);
eigL=aux(sx); eigV=V(:,sx);

% display Eigenvalues----------------------------
figure(3)
plot(eigL,'k');
title('Eigenvalues')
axis([0 55 0 12]); grid;

% project to obtain eigenfaces
aux=PF*eigV;
eigF=aux/norm(aux);

% display 10 first Eigenfaces
figure(4)
for nf=1:10,
    ef=reshape(eigF(:,nf),H,W);
    subplot(2,5,nf)
    imshow(ef,[]);  
    tt=['Eigenface ', num2str(nf)]; title(tt);
end;
    
