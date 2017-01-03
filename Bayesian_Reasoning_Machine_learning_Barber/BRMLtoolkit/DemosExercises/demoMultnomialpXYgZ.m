function demoMultnomialpXYgZ
%DEMOMULTNOMIALPXYGZ demo of training a multinomial distribution p(x,y|z)p(z)
X = 4; Y=5; Z = 3; % number of states 

figure
pxgz = condp(rand(X,Z).^2); % power 2 makes the tables sparser
pygz = condp(rand(Y,Z).^2);
pz= condp(rand(Z,1));
pxy = pxgz*(pygz.*repmat(pz',Y,1))';

% generate some samples from p(x,y,z)
N=200;
for n=1:N
    zdata(n) = randgen(pz);
    xdata(n) = randgen(pxgz(:,zdata(n)));
    ydata(n) = randgen(pygz(:,zdata(n)));
end

x=1; y=2; z=3;
pot(x).variables=[x z]; pot(x).table=myzeros([X Z]);
pot(y).variables=[y z]; pot(y).table=myzeros([Y Z]);
pot(z).variables =z; pot(z).table=myzeros(Z);

data = vertcat(xdata,ydata,repmat(nan,1,N)); % z is missing

pars.tol=0.0001; pars.maxiterations=20; pars.plotprogress=1;
[pot loglik]=EMbeliefnet(pot,data,pars);

jointpot=multpots(pot);
tpxy = table(sumpot(jointpot,z));
subplot(1,2,1); hinton(pxy); title('true');
subplot(1,2,2); hinton(tpxy); title('learned');