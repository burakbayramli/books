% Generation of random data with a desired PDF
% Using MCMC
% Metropolis algorithm

% example of desired PDF
x=0:(pi/100):pi; 
dpf=0.5*sin(x); % desired PDF

% example of proposal PDF (normal)
xp=-4:(pi/100):4; 
sigma=0.6; %deviation
q=normpdf(xp,0,sigma);

% generation of random data
N=5000; %number of data
z=zeros(1,N); %space for data to be generated

x0=pi/2; %initial value
for nn=1:N, 
    inr=0;
    while inr==0, %new value proposal (Markovian transition)  
       x1=x0+(sigma*randn(1)); %normal distribution (symmetric)
       if (x1<pi) & (x1>0),
          inr=1; %x1 is valid (is inside dpf domain)
       end;
    end;   
    f1=0.5*sin(x1);
    f0=0.5*sin(x0);
    alpha=f1/f0;
    if alpha>=1
       z(nn)=x1; %accept
    else
       aux=rand(1);
       if aux<alpha,
          z(nn)=x1; %accept
       else
          z(nn)=x0;
       end;
    end;
    x0=x1;
 end;   
       
nz=z(1000:5000); %eliminate initial data
 
figure(1)
plot(x,dpf,'k'); hold on;
plot(xp,q,'r');
axis([-2 4, 0 0.8]);
xlabel('x'); title('desired PDF and proposal PDF');

figure(2)
hist(nz,30); colrmap('cool');
xlabel('x');title('histogram of the generated data');

