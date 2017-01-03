function demoLDSsubspace
%DEMOLDSSUBSPACE demo of training a linear dynamical system using a subspace method
V=2; % Number visible variables
H=3; % Number of hidden variables
L=2*H; % Order for the Hankel matrix
T=150+L-1; 
% generate some data from a model
sigmaV=0*0.000005; sigmaH=0*0.000001;
tmp=randn(H,H); 
Atrue=expm(0.1*(tmp-tmp')); % a random orthogonal matrix
htrue(:,1)=randn(H,1);
Btrue=randn(V,H);
vclean(:,1)=Btrue*htrue(:,1);
v(:,1)=vclean(:,1)+sigmaV*randn(V,1);
for t=2:T
	htrue(:,t)=Atrue*htrue(:,t-1)+sigmaH*randn(H,1);
	vclean(:,t)=Btrue*htrue(:,t);
	v(:,t)=vclean(:,t)+sigmaV*randn(V,1);
end
[A,B,diagS,hest,vest]=LDSsubspace(v,H,L); 
ms=5;
for i=1:V
	subplot(V,1,i); hold on; set(gca,'box','on');
	plot(vclean(i,1:T-L+1)','ro-','markersize',ms);
	plot(vest(i,1:T-L+1)','b+-','markersize',ms);
	plot(v(i,1:T-L+1)','m.','markersize',ms);
end; 
figure
for i=1:H
	subplot(H,1,i); hold on; set(gca,'box','on')
	plot(hest(i,1:T-L+1)','b+-','markersize',ms);
end;   
figure
for i=1:H
	subplot(H,1,i); hold on; set(gca,'box','on')
	plot(htrue(i,1:T-L+1)','ro-','markersize',ms);
end;
figure
plot(diagS);