% Generation of random data with a desired PDF
% Using rejection method

% example of desired PDF
x=0:(pi/100):pi; 
dpf=0.5*sin(x); % desired PDF

% example of proposal PDF
xp=0:(pi/100):pi+3; 
ppf=raylpdf(xp,1.5);

%factor
c=1.5;

% generation of random data
N=2000; %number of data
z=zeros(1,N); %space for data to be generated

for nn=1:N,
   
   accept=0;  
   while accept==0,
   	v=raylrnd(1.5,1,1); %Rayleigh distribution
		u=rand(1,1); %uniform distribution
   	if v<=pi,  %v must be inside dpf domain  
			P=u*c*raylpdf(v,1.5);
			L=0.5*sin(v);
			if P<L, z(nn)=v; accept=1; end; %accept
      end;
   end;
end;

figure(1)
plot(x,dpf,'k'); hold on;
plot(xp,c*ppf,'r');
xlabel('x'); title('desired PDF and proposal PDF');

figure(2)
hist(z,30); colormap('cool');
xlabel('x');title('histogram of the generated data');

