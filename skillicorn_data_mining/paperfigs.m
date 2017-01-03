function base(mfn)
format short;

% machinery to create result files
% make directory name
dmfn = ['figs' mfn];
status = mkdir(dmfn);

a = csvread([mfn '.csv']);

n = size(a,1)
m = size(a,2)

ifile = [dmfn '/initial.txt'];
inf = fopen(ifile,'w');
for i = 1:n
	fprintf(inf,'%6.0f &',a(i,:));
	fprintf(inf,'\\\\ \n');
end
fclose(inf);

% normalize a

na = zeros(n,m);
for j = 1:m
	if std(a(:,j)) == 0
		na(:,j) = (a(:,j) - mean(a(:,j)));
	else
		na(:,j) = (a(:,j) - mean(a(:,j)))./std(a(:,j));
	end;
end;

% svd of unnormalized data

[u,s,v] = svd(a,0);

ufile = [dmfn '/' mfn '.u'];
vfile = [dmfn '/' mfn '.v'];
sfile = [dmfn '/' mfn '.s'];
psfile = [dmfn '/' mfn ];
uf = fopen(ufile,'w');
for i = 1:size(u,1)
	fprintf(uf,'%6.2f &',u(i,:));
	fprintf(uf,'\\\\ \n');
end
fclose(uf);

vf = fopen(vfile,'w');
for i = 1:size(v,1)
	fprintf(vf,'%6.2f &',v(i,:));
	fprintf(vf,'\\\\ \n');
end
fclose(vf);

sf = fopen(sfile,'w');
for i = 1:size(v,1)
	fprintf(sf,'%6.2f &',s(i,:));
	fprintf(sf,'\\\\ \n');
end
fclose(sf);

ss = diag(s);

ssvf1 = fopen([dmfn '/singvalue1.txt'],'w');
fprintf(ssvf1,'%4.2f, followed by %4.2f and %4.2f.\n',ss(1),ss(2),ss(3));
fclose(ssvf1);

% labelled objects
figure;

plot3(u(:,1),u(:,2),u(:,3),'r.','MarkerSize',12)
for i = 1:size(u,1)
   text(u(i,1), u(i,2), u(i,3), [' ' int2str(i)],'FontSize',11)
end
view([-150,30])
axis('auto')
xlabel('U1')
ylabel('U2')
zlabel('U3')

print('-deps2', [dmfn '/u.eps']);

% labelled attributes
figure

hold on;
plot3(v(:,1),v(:,2),v(:,3),'r.','MarkerSize',12);
for i=1:size(v,1)
	text(v(i,1),v(i,2),v(i,3),[' ' int2str(i)],'FontSize',11);
end
view([-150,30])
axis('auto')
xlabel('V1')
ylabel('V2')
zlabel('V3')

print('-deps2', [dmfn '/v.eps']);

% singular values
figure;

plot(ss,'-k+','MarkerSize',10)
ylabel('s')

print('-deps2', [dmfn '/s.eps']);

% US and SV
us = u * s;
vs = v * s;

%  plot u and v matrices scaled by the singular values
figure;

plot3(us(:,1),us(:,2),us(:,3),'r.','MarkerSize',12)
for i = 1:size(u,1)
   text(us(i,1), us(i,2), us(i,3), [' ' int2str(i)],'FontSize',11)
end
view([-150,30])
axis('auto')
xlabel('US1')
ylabel('US2')
zlabel('US3')

print('-deps2', [dmfn '/us.eps']);

% labelled attributes
figure

hold on;
plot3(vs(:,1),vs(:,2),vs(:,3),'r.','MarkerSize',12);
for i=1:size(v,1)
	text(vs(i,1),vs(i,2),vs(i,3),[' ' int2str(i)],'FontSize',11);
end
view([-150,30])
axis('auto')
xlabel('VS1')
ylabel('VS2')
zlabel('VS3')

print('-deps2', [dmfn '/sv.eps']);

% repeat for normalized A

[u,s,v] = svd(na,0);

nufile = [dmfn '/' mfn '.nu'];
nvfile = [dmfn '/' mfn '.nv'];
nsfile = [dmfn '/' mfn '.ns'];

uf = fopen(nufile,'w');
for i = 1:size(u,1)
	fprintf(uf,'%6.2f &',u(i,:));
	fprintf(uf,'\\\\ \n');
end
fclose(uf);

vf = fopen(nvfile,'w');
for i = 1:size(v,1)
	fprintf(vf,'%6.2f &',v(i,:));
	fprintf(vf,'\\\\ \n');
end
fclose(vf);

sf = fopen(nsfile,'w');
for i = 1:size(v,1)
	fprintf(sf,'%6.2f &',s(i,:));
	fprintf(sf,'\\\\ \n');
end
fclose(sf);

ss = diag(s);

ssvf2 = fopen([dmfn '/singvalue2.txt'],'w');
fprintf(ssvf2,'%4.2f, followed by %4.2f and %4.2f.\n',ss(1),ss(2),ss(3));
fclose(ssvf2);

% calculate entropy

sssum = 0;
for i = 1:m
	sssum = sssum + ss(i).*ss(i);
end

for i = 1:m
	f(i) = ss(i).*ss(i)/sssum;
end

entrsum = 0;
for i = 1:m
	entrsum = entrsum + f(i) * log(f(i));
end
entropy = -entrsum/(log(m))

ef = fopen([dmfn '/entropy.txt'],'w');
fprintf(ef,'%5.3f, %5.3f, %5.3f, %5.3f, %5.3f, %5.3f, %7.5f, and %9.7f.',f);
fprintf(ef,' The entropy for this dataset is %5.3f,', entropy);
fclose(ef);

% labelled objects (normalized)
figure;

plot3(u(:,1),u(:,2),u(:,3),'r.','MarkerSize',12)
for i = 1:size(u,1)
   text(u(i,1), u(i,2), u(i,3), [' ' int2str(i)],'FontSize',11)
end
view([-150,30])
axis('auto')
xlabel('U1')
ylabel('U2')
zlabel('U3')

print('-deps2', [dmfn '/nu.eps']);

% labelled attributes (normalized)
figure

hold on;
plot3(v(:,1),v(:,2),v(:,3),'r.','MarkerSize',12);
for i=1:size(v,1)
	text(v(i,1),v(i,2),v(i,3),[' ' int2str(i)],'FontSize',11);
end
view([-150,30])
axis('auto')
xlabel('V1')
ylabel('V2')
zlabel('V3')

print('-deps2', [dmfn '/nv.eps']);

% singular values

figure;

plot(ss,'-k+','MarkerSize',10)
ylabel('s')

print('-deps2', [dmfn '/ns.eps']);

% matrix with extra rows

aa = [a; 1 1 1 1 1 1 1 1; 9 9 9 9 9 9 9 9];

[u,s,v] = svd(aa,0);

aaufile = [dmfn '/' mfn '.aau'];
aavfile = [dmfn '/' mfn '.aav'];
aasfile = [dmfn '/' mfn '.aas'];

aauf = fopen(aaufile,'w');
for i = 1:size(u,1)
	fprintf(aauf,'%6.2f &',u(i,:));
	fprintf(aauf,'\\\\ \n');
end
fclose(aauf);

aavf = fopen(aavfile,'w');
for i = 1:size(v,1)
	fprintf(aavf,'%6.2f &',u(i,:));
	fprintf(aavf,'\\\\ \n');
end
fclose(aavf);

aasf = fopen(aasfile,'w');
for i = 1:size(v,1)
	fprintf(aasf,'%6.2f &',s(i,:));
	fprintf(aasf,'\\\\ \n');
end
fclose(aasf);

% labelled objects
figure;

plot3(u(:,1),u(:,2),u(:,3),'r.','MarkerSize',12)
for i = 1:size(u,1)
   text(u(i,1), u(i,2), u(i,3), [' ' int2str(i)],'FontSize',11)
end
view([-160,30])
axis('auto')
xlabel('U1')
ylabel('U2')
zlabel('U3')

print('-deps2', [dmfn '/orient.eps']);

% matrix with extra rows 2

ab = [a; 1 1 1 1 9 9 9 9; 9 9 9 9 1 1 1 1];

[u,s,v] = svd(ab,0);

abufile = [dmfn '/' mfn '.abu'];
abvfile = [dmfn '/' mfn '.abv'];
absfile = [dmfn '/' mfn '.abs'];
abuf = fopen(abufile,'w');
for i = 1:size(u,1)
	fprintf(abuf,'%6.2f &',u(i,:));
	fprintf(abuf,'\\\\ \n');
end
fclose(abuf);

abvf = fopen(abvfile,'w');
for i = 1:size(v,1)
	fprintf(abvf,'%6.2f &',v(i,:));
	fprintf(abvf,'\\\\ \n');
end
fclose(abvf);

absf = fopen(absfile,'w');
for i = 1:size(v,1)
	fprintf(absf,'%6.2f &',s(i,:));
	fprintf(absf,'\\\\ \n');
end
fclose(absf);

% labelled objects
figure;

plot3(u(:,1),u(:,2),u(:,3),'r.','MarkerSize',12)
for i = 1:size(u,1)
   text(u(i,1), u(i,2), u(i,3), [' ' int2str(i)],'FontSize',11)
end
view([-100,30])
axis('auto')
xlabel('U1')
ylabel('U2')
zlabel('U3')

print('-deps2', [dmfn '/nextorient.eps']);

% output A1 A2

[u,s,v] = svd(na,0);

a1 = u(:,1) * s(1,1) * v(:,1)';
a2 = u(:,2) * s(2,2) * v(:,2)';
a1file = [dmfn '/a1.txt'];
a2file = [dmfn '/a2.txt'];
a3file = [dmfn '/a3.txt'];
autoafile = [dmfn '/autoa.txt'];

a1f = fopen(a1file,'w');
for i = 1:size(u,1)
	fprintf(a1f,'%6.2f &',a1(i,:));
	fprintf(a1f,'\\\\ \n');
end
fclose(a1f);

a2f = fopen(a2file,'w');
for i = 1:size(u,1)
	fprintf(a2f,'%6.2f &',a2(i,:));
	fprintf(a2f,'\\\\ \n');
end
fclose(a2f);

asum = a1 + a2;

a3f = fopen(a3file,'w');
for i = 1:size(u,1)
	fprintf(a3f,'%6.2f &',asum(i,:));
	fprintf(a3f,'\\\\ \n');
end
fclose(a3f);

autoa = asum * asum';

autoaf = fopen(autoafile,'w');
for i = 1:size(u,1)
	fprintf(autoaf,'%6.2f &',autoa(i,:));
	fprintf(autoaf,'\\\\ \n');
end
fclose(autoaf);

% end of svd section

k = 8;

[d,x,y] = sdd(na,k);

yfile = [dmfn '/' mfn '.y'];
dfile = [dmfn '/' mfn '.d'];
xfile = [dmfn '/' mfn '.x'];

xf = fopen(xfile,'w');
for i = 1:size(x,1)
	fprintf(xf,'%6.0f &',x(i,:));
	fprintf(xf,'\\\\ \n');
end
fclose(xf);

yf = fopen(yfile,'w');
for i = 1:size(y,1)
	fprintf(yf,'%6.0f &',y(i,:));
	fprintf(yf,'\\\\ \n');
end
fclose(yf);

df = fopen(dfile,'w');
for i = 1:size(y,1)
	fprintf(df,'%6.2f &',d(i,:));
	fprintf(df,'\\\\ \n');
end
fclose(df);

op = x(:,2) * y(:,2)'

opf = fopen([dmfn '/op1.txt'],'w');
for i = 1:size(x,1)
	fprintf(opf,'%4.0f &',op(i,:));
	fprintf(opf,'\\\\ \n');
end
fclose(opf);


% sdd with largest volume components moved to the front

[d,x,y] = smsdd(na,k);

yfile = [dmfn '/' mfn '.sy'];
dfile = [dmfn '/' mfn '.sd'];
xfile = [dmfn '/' mfn '.sx'];

xf = fopen(xfile,'w');
for i = 1:size(x,1)
	fprintf(xf,'%6.0f &',x(i,:));
	fprintf(xf,'\\\\ \n');
end
fclose(xf);

yf = fopen(yfile,'w');
for i = 1:size(y,1)
	fprintf(yf,'%6.0f &',y(i,:));
	fprintf(yf,'\\\\ \n');
end
fclose(yf);

df = fopen(dfile,'w');
for i = 1:size(y,1)
	fprintf(df,'%6.2f &',d(i,:));
	fprintf(df,'\\\\ \n');
end
fclose(df);

op = x(:,2) * y(:,2)'

opf = fopen([dmfn '/op2.txt'],'w');
for i = 1:size(x,1)
	fprintf(opf,'%4.0f &',op(i,:));
	fprintf(opf,'\\\\ \n');
end
fclose(opf);

n = size(na,1);
m = size(na,2);

% plots of regions

figure;
for r = 1:k
	peak = d(r) * x(:,r) * y(:,r)';
	nn = norm(peak,'fro');
	bar3(peak,1.0,'detached','y');
	title(['Bump at level ' int2str(r)]);
	ylabel('Objects');
	xlabel('Attributes');
	zlabel('Bump direction');
	print('-deps2',[dmfn '/peak' int2str(r)]);
	print('-depsc2',[dmfn '/cpeak' int2str(r)]);
	clf;
end

% 3 level sdd for objects

figure;
view([-150,30])
axis('auto')
xlabel('U1')
ylabel('U2')
zlabel('U3')
legend;
sz = n;

xdis;

print('-deps2',[dmfn '/sso'] );
print('-depsc2',[dmfn '/csso'] );

% 3 level sdd for attributes

figure;

view([-150,30])
axis('auto')
xlabel('V1')
ylabel('V2')
zlabel('V3')
sz = m;

ydis;

print('-deps2',[dmfn '/ssa'] );
print('-depsc2',[dmfn '/cssa'] );

%  nnmf using Seung and Lee code

% transpose because algorithm expects attributes as rows
A = a'; 

[n m] = size(A);
r = 8; % choose rank for the factorization
maxiter = 300; % choose the maximum number of iterations

W = rand(n,r); % randomly initialize basis
W = W./(ones(n,1)*sum(W)); % normalize column sums
H = rand(r,m); % randomly initialize encodings
eps = 1e-9; % set your own tolerance

for iter=1:maxiter
	H = H.*(W'*((A+eps)./(W*H+eps)));
	W = W.*(((A+eps)./(W*H+eps))*H');
	W = W./(ones(n,1)*sum(W));
end

C = H';
F = W';

cf = fopen([dmfn '/c.txt'],'w');
for i = 1:size(C,1)
	fprintf(cf,'%6.2f &',C(i,:));
	fprintf(cf,'\\\\ \n');
end
fclose(cf);

ff = fopen([dmfn '/f.txt'],'w');
for i = 1:size(F,1)
	fprintf(ff,'%6.2f &',F(i,:));
	fprintf(ff,'\\\\ \n');
end
fclose(ff);
