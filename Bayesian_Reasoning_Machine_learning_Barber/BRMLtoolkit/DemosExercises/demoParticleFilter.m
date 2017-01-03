function demoParticleFilter
%DEMOPARTICLEFILTER demo of Particle Filering
Gx=40; Gy=40; % grid dimensions
% make a Face:
F = [1 1 1 0 1 1 1;
	0 0 0 0 0 0 0;
	0 1 0 0 0 1 0;
	0 0 0 0 0 0 0;
	0 0 0 1 0 0 0;
	1 0 0 0 0 0 1;
	1 1 1 1 1 1 1;];
T=50; % number of timesteps
sigma=1; % noise in both the x and y directions
h(:,1)=round([Gx Gy]'/2); % place object in centre of grid
v(:,:,1) = placeobject(F,h(:,1),Gx,Gy);
% generate a sequence of observations:
for t=2:T
	h(:,t)=h(:,t-1)+sigma*randn(2,1); % Brownian motion
	vclean = placeobject(F,h(:,t),Gx,Gy); % clean Face
	flippixel = rand(Gy,Gx)>0.95; % flip some of the pixels
	vnoisy=vclean; vnoisy(flippixel)=1-vnoisy(flippixel);
	v(:,:,t)=vnoisy;
end
figure
% Forward-Sampling-Resampling method:
L=50; % number of particles
hpart_old=repmat(h(:,1),1,L); % start with all particles in the correct position
w_old=condp(1:L);
for t=1:T
	subplot(1,2,1); imagesc(1-v(:,:,t)); colormap(bone); hold on;
	for l=1:L
		lstar=randgen(w_old); % sample a component
		hpart(:,l)=hpart_old(:,lstar)+sigma*randn(2,1);
		pvgh=compat(v(:,:,t),F,hpart(:,l),Gx,Gy);
		wtilde(l,1)=pvgh; % unnormalised weights
	end
	w=condp(wtilde); % normalise the weights
	lw=2;
	for l=1:L
		plot(hpart(2,l),hpart(1,l),'o','markersize',200*w(l));
		plot(h(2,t),h(1,t),'gx','markersize',20,'linewidth',lw);
	end
	[val ind]=max(w); % most likely particle
	plot(hpart(2,ind),hpart(1,ind),'r+','markersize',20,'linewidth',lw);
	hmean = sum(repmat(w',2,1).*hpart,2);
	plot(hmean(2),hmean(1),'mo','markersize',20,'linewidth',lw);
	subplot(1,2,2); bar(w); axis([1 L 0 val]);title('particle weights'); drawnow;
	hpart_old=hpart;w_old=w;
end