% Backpropagation example
% Three clusters of Gaussian data

% the data (3 clusters)---------------------------
N=100;
%cluster 1
mux1=1; muy1=3; sigmax1=0.2; sigmay1=0.2;
x1=normrnd(mux1,sigmax1,1,N);
y1=normrnd(muy1,sigmay1,1,N);
%cluster 2
mux2=2; muy2=1; sigmax2=0.3; sigmay2=0.3;
x2=normrnd(mux2,sigmax2,1,N);
y2=normrnd(muy2,sigmay2,1,N);
%cluster 3
mux3=3; muy3=3; sigmax3=0.2; sigmay3=0.2;
x3=normrnd(mux3,sigmax3,1,N);
y3=normrnd(muy3,sigmay3,1,N);

% complete data set
D=zeros(2,3*N);
D(1,:)=[x1,x2,x3];
D(2,:)=[y1,y2,y3];

% neural network init------------------------------
% (w: weights; c: outputs; delta: error)
% 2 input neurons
ci=zeros(2,1);
% 2 hidden neurons
wh=0.05*ones(2,2); bh=0.05*ones(2,1); ch=zeros(2,1); deltah=zeros(2,1);
% 3 output neurons
wo=0.05*ones(2,3); bo=0.05*ones(3,1); co=zeros(3,1); deltao=zeros(3,1);

%training constant
eta=0.001;

Nte=120; %number of training epochs
Er=zeros(Nte,1); %error record

% neural network training------------------------------

%training iterations
Ef=1000; %for info after search

for it=1:Nte,   
   for in=1:300, 
      %---------------------------------------
      
      %neuron inputs
      nn=in;
      x=D(1,nn); y=D(2,nn);
      %neuron outputs
      ci(1)=x; ci(2)=y; ch=tanh(bh+(wh'*ci)); co=tanh(bo+(wo'*ch));
      %errors (output):
      deltao=(1-(co.^2)); 
      if in>200,
         %data with label co1=0, co2=0, co3=1
         deltao(1)=deltao(1)*(-co(1)); 
         deltao(2)=deltao(2)*(-co(2));
         deltao(3)=deltao(3)*(1-co(3));
         %training error
         Er(it)=Er(it)+((co(1))^2)+((co(2))^2)+((co(3)-1)^2);
      elseif in>100,         
         %data with label co1=0, co2=1, co3=0
         deltao(1)=deltao(1)*(-co(1)); 
      	deltao(2)=deltao(2)*(1-co(2));
         deltao(3)=deltao(3)*(-co(3));
         %training error
         Er(it)=Er(it)+((co(1))^2)+((co(2)-1)^2)+((co(3))^2);
      else
         %data with label co1=1, co2=0, co3=0
			deltao(1)=deltao(1)*(1-co(1)); 
         deltao(2)=deltao(2)*(-co(2));
         deltao(3)=deltao(3)*(-co(3));
         %training error
         Er(it)=Er(it)+((co(1)-1)^2)+((co(2))^2)+((co(3))^2);
      end;           
      %change of weights wo
      aux=eta*deltao*ch'; wo=wo+aux'; bo=bo+(eta*deltao);
      %errors (hidden)
      aux=deltao'*wo'; deltah=(1-(ch.^2)); deltah=deltah.*aux';
      %change of weights wh
      aux=eta*deltah*ci'; wh=wh+aux; bh=bh+(eta*deltah); 
  end;
  Ef=Er(it);
end;
Ef

% display-------------------------------------
figure(1)
plot(Er,'k');
title('Training error evolution');
xlabel('epoch')

% 3 data clusters
figure(2)
plot(x1,y1,'ro'); hold on;
plot(x2,y2,'gx');
plot(x3,y3,'bd');
title('Neural network example (3 clusters)'); 
xlabel('x'); ylabel('y');

% test of the trained network-----------------------------------
% test points:
figure(3)
zon=zeros(80,80,3); %space for RGB image
x=0; y=0;
%make zones using test points
for nl=1:80,
   y=0.05*nl;
   for nc=1:80,
      x=0.05*nc;
      %neuron outputs
      ci(1)=x; ci(2)=y; ch=tanh(bh+(wh'*ci)); co=tanh(bo+(wo'*ch));
      aux=co'; colr=0.5+(aux/2); %force into 0..1 range
      zon(nl,nc,:)=colr;
    end;
 end;  
 imshow(zon);
 axis xy;
 title('classification zones')
 xlabel('x'); ylabel('y');
 
