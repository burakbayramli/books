function demoLearnThinMNDigit
%DEMOLEARNTHINMNDIGIT demo of learning a thin Markov Net for the digits
figure
load binaryalphadigs
for n=1:36
    tmp=reshape(double(dat{3,n}),20,16);
    tmp2=tmp(ceil(1:2:end),ceil(1:2:end)); % downsample for speed
    [X Y]=size(tmp2);
    x(:,n)=tmp2(:);
end
x=x+1; % convert to 1/2 coding
D=size(x,1);
% compute the entropy of pairwise marginals
for i=1:size(x,1)
    ent(i,i)=0;
    for j=i+1:size(x,1)
        ent(i,j)=EntropyEmp(x([i j],:),[2 2]);
        ent(j,i)=0;
    end
end
[val ind]=sort(ent(:),'descend');
candedges=ind2subv([D D],ind);
opts.candedges=candedges(1:D*(D-1)/2,:)
opts.Ainit=zeros(D,D);
opts.plotprogress=1;
[A Atri cl]=makeThinJT(D,10,opts);
cl=uniquepots(cl,0);

for p=1:length(cl)
    v=cl(p).variables;
    edist(p)=empdist(x,v,2*ones(1,length(v)));
end

opts.plotprogress=1;
pot=learnMarkovDecomp(edist,1,opts);
figure

% reconstruction
fprintf(1,'Reconstructing from missing data:\n')
[jtpot jtsep infostruct]=jtree(pot);
[jtpotm jtsepm]=absorption(jtpot,jtsep,infostruct);

missing=rand(size(x,1),size(x,2))>0.5; % random missing
%missing=zeros(size(x,1),size(x,2));
%missing(1:size(x,1)/2,:)=1; % right half missing
xmissing=x.*(1-missing);
vars=1:D;
for n=1:size(x,2)
    miss=missing(:,n)==1;
    [jtpot2 jtsep2]=jtassignpot(setevpot(pot,vars(~miss),x(~miss,n)),infostruct);
    [jtpot2 jtsep2]=absorption(jtpot2,jtsep2,infostruct,0); % do full round of absorption
    JTmaxstate=zeros(D,1);
    for i=1:length(jtpot2)
        [newpot JTmaxstate(jtpot2(i).variables)] = maxpot(jtpot2(i),[],0); % find max over each clique
    end
    xrecon(vars(miss),n)=JTmaxstate(vars(miss));
    xrecon(vars(~miss),n)=x(~miss,n);
    subplot(1,3,1); imagesc(reshape(x(:,n),X,Y),[0 2]); title('original');
    subplot(1,3,3); imagesc(reshape(xmissing(:,n),X,Y),[0 2]);title('missing')
    subplot(1,3,2); imagesc(reshape(xrecon(:,n),X,Y),[0 2]);title('reconstruction'); drawnow
end
fprintf(1,'%d missing values\n%d reconstruction errors\n',sum(missing(:)),sum(xrecon(:)~=x(:)));