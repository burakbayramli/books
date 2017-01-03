function demoIPFeff
%DEMOIPFEFF demo of Iterated Proportional Fitting of a Thin Junction Tree

D=10; % number of variables in Markov network

% firstly, let's construct a thin Junction Tree:
opts.Ainit=eye(D);
opts.Acand=rand(D,D)>0.5;
opts.plotprogress=1;
A=makeThinJT(D,3,opts);
W=randn(D,D); W=W.*A; % make the weights W of the form of A, but with random values
pot=uniquepots(BMpotential(W)); % use a Boltzmann machine form

% Training data:
x=potsample(pot,100); % generate some training data from this thin width Boltzmann machine
% compute the empirical marginals:
for p=1:length(pot)
    v=pot(p).variables;
    edist(p)=emppot(x,v,2*ones(1,length(v)));
end
figure; drawNet(markov(edist)); figure

% Given now only the the training data, use IPF to learn the tables of the thin JT Markov network
opts.outerloop=10;
opts.innerloop=1;
opts.plotprogress=1;
opts.tol=10e-4;

pot=IPF(edist,opts);
% find the log likelihood
L=0; for n=1:size(x,2);  L=L + sum(log(table(setpot(pot,1:D,x(:,n))))); end

% To see how well the data has been learned, let's infer the posterior
% distribution of the`missing' variables conditioned on the rest (note that
% this is not the same as learning with missing data). We then take the
% most likely state of this posterior as the reconstructed values:
missing=rand(size(x,1),size(x,2))>0.75;
xmissing=x.*(1-missing); vars=1:D;

[jtpot jtsep infostruct]=jtree(pot);
for n=1:size(x,2)
    miss=missing(:,n);
    [jtpot2 jtsep2]=jtassignpot(setevpot(pot,vars(~miss),x(~miss,n)),infostruct);
    [jtpot2 jtsep2]=absorption(jtpot2,jtsep2,infostruct,0); % do full round of absorption
    JTmaxstate=zeros(D,1);
    for i=1:length(jtpot2)
        [newpot JTmaxstate(jtpot2(i).variables)] = maxpot(jtpot2(i),[],0); % find max over each clique
    end
    xrecon(vars(miss),n)=JTmaxstate(vars(miss));
    xrecon(vars(~miss),n)=x(~miss,n);
end
figure;
subplot(1,3,1); imagesc(x,[0 2]); title('complete data');colormap hot;
subplot(1,3,2); imagesc(xrecon,[0 2]); title('reconstruction')
subplot(1,3,3); imagesc(xmissing,[0 2]); title('missing variables'); colormap bone
fprintf(1,'%d missing values\n%d reconstruction errors\n',sum(missing(:)),sum(xrecon(:)~=x(:)));