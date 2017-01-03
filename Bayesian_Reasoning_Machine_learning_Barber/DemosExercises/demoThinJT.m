function demoThinJT
figure;
N=100; % number of vertices
opts.Ainit=zeros(N);
opts.Ainit=spantree(edges(double(rand(N,N)>0.9))); %initial guess of triangulated graph
r=randperm(N); opts.Acand=triu(rand(N,N)>0.8); % candidate edges
opts.Acand=opts.Acand(r,:);
opts.plotprogress=1;
maxwidth=6;
[A Atriangulated]=makeThinJT(N,maxwidth,opts); 