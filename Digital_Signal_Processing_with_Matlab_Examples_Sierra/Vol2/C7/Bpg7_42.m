% Perceptron example
% Scatterplot of IRIS data
% x=sepal width, y=petal length

%read IRIS data
D=dlmread('iris.data')';

%Perceptron weights 
w1=1; w2=0; %for data
w3=0.5; %for line threshold
%training constant
eta=0.01;
%number of epochs
N=10;

%failures counter
nfail=zeros(1,N);

%training iterations
for it=1:N,   
   for nn=1:50, %data with label -1
      x=D(2,nn); y=D(3,nn); 
      c=(w1*x)+(w2*y)+w3; 
      if(c>0), nfail(it)=nfail(it)+1; end; %failure count
      delta=-1-c;
      mf=eta*delta;
      w1=w1+(mf*x); w2=w2+(mf*y); w3=w3+mf; %change of weights
   end;
   for nn=51:150, %data with label 1
      x=D(2,nn); y=D(3,nn); 
      c=(w1*x)+(w2*y)+w3; 
      if(c<0), nfail(it)=nfail(it)+1; end; %failure count
      delta=1-c;
      mf=eta*delta;
      w1=w1+(mf*x); w2=w2+(mf*y); w3=w3+mf; %change of weights
   end;
end;

%result: line parameters
m=-w1/w2; b=-w3/w2;

%display
figure(1)
scatter(D(2,1:50),D(3,1:50),32,'k'); hold on;
scatter(D(2,51:150),D(3,51:150),32,'r');
axis([0 6 0 8]);
len=7; %arbitrary value
plot([0 len],[b (m*len)+b],'k');
title('Perceptron example (IRIS data)'); 
xlabel('sepal width'); ylabel('petal length');

figure(2)
plot(nfail,'k');
title('classification fails along training');
xlabel('epoch')
