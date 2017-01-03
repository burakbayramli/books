function demoHMMburglar
%DEMOHMMBURGLAR demo of HMM in the burlgar scenario
figure;
Gx = 5; Gy = 5;  % two dimensional grid size
H = Gx*Gy; % number of states on grid
st = reshape(1:H,Gx,Gy); % assign each grid point a state

% make a deterministic transition matrix on a 2D grid:
phgh = zeros(H,H);
for x = 1:Gx
	for y = 1:Gy
		if validgridposition(x+1,y,Gx,Gy)
			phgh(st(x+1,y),st(x,y))=1;
		end
		if validgridposition(x-1,y,Gx,Gy)
			phgh(st(x-1,y),st(x,y))=1;
		end
		if validgridposition(x,y+1,Gx,Gy)
			phgh(st(x,y+1),st(x,y))=1;
		end
		if validgridposition(x,y-1,Gx,Gy)
			phgh(st(x,y-1),st(x,y))=1;
		end
	end
end
phghm = condp(phgh); ph1=condp(ones(H,1));
pgvh=zeros(4,H);

pv1gh= 0.01*ones(1,H); r=randperm(H); pv1gh(r(1:10))=0.9; % Creaks in 10 randomly chosen cells
pv2gh= 0.01*ones(1,H); r=randperm(H); pv2gh(r(1:10))=0.9; % Bumps in 10 randomly chosen cells
subplot(1,2,1); imagesc(reshape(pv1gh,Gx,Gy),[-1 2]); axis off; colormap bone
subplot(1,2,2); imagesc(reshape(pv2gh,Gx,Gy),[-1 2]); axis off; colormap bone

% Form the joint p(v|h)=p(v1|h)p(v2|h):
for i=1:4
	pvgh(1,:) =  pv1gh.*pv2gh; vv(1,:)=[1 1];       % p(v1=1|h)*p(v2=1|h)
	pvgh(2,:) =  pv1gh.*(1-pv2gh); vv(2,:)=[1 2];   % p(v1=1|h)*p(v2=1|h)
	pvgh(3,:) =  (1-pv1gh).*pv2gh; vv(3,:)=[2 1];   % p(v1=1|h)*p(v2=1|h)
	pvgh(4,:) =  (1-pv1gh).*(1-pv2gh);vv(4,:)=[2 2];% p(v1=1|h)*p(v2=1|h)
end

% draw some random samples:
T=10; h(1)=randgen(ph1); v(1)=randgen(pvgh(:,h(1)));
for t=2:T
	h(t)=randgen(phghm(:,h(t-1))); v(t)=randgen(pvgh(:,h(t)));
end

% Perform inference based on the observed v:
[logalpha,loglik]=HMMforward(v,phghm,ph1,pvgh); % filtering
phtgV1t = condexp(logalpha); % find the filtered distribution based on log(p(h(t)|v(1:t)))
phtgV1T=HMMgamma(logalpha,phghm); % Smoothed Burglar distribution
maxstate=HMMviterbi(v,phghm,ph1,pvgh); % Most likely Burglar path

figure
for t=1:T
	subplot(5,T,t); imagesc(vv(v(t),:),[0 3]); axis off;
	subplot(5,T,T+t); imagesc(reshape(phtgV1t(:,t),Gx,Gy)); axis off;
	subplot(5,T,2*T+t); imagesc(reshape(phtgV1T(:,t),Gx,Gy)); axis off;
	z=zeros(H,1); z(maxstate(t))=1;
	subplot(5,T,3*T+t); imagesc(reshape(z,Gx,Gy)); colormap(bone); axis off;
	z=zeros(H,1); z(h(t))=1;
	subplot(5,T,4*T+t); imagesc(reshape(z,Gx,Gy),[0 1]); colormap(bone); axis off;
end