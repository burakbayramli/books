%this m-file uses the acceptance/rejection 
%method to sample from the triangular and 
%truncnated normal densities. 
clear;
clc;
rand('seed',sum(100*clock));
nkeep = 25000;

%--------------------------------
%triangular density
%--------------------------------
M=1;
a=-1;
b=1;

triang_keep = zeros(nkeep,1);
counter =0;
    while counter < nkeep
        U = rand(2,1);
        U1 = U(1); U2 = U(2);
        if M*U2 < 1 - abs( a + (b-a)*U1)
            counter = counter+1;
            x = a + (b-a)*U1;
            triang_keep(counter,1) = x;
        end;
    end;
    
        
%--------------------------------
%Truncated Normal TN_[0,4] (1,1) density
%--------------------------------
a = 0;
b = 4;
normcons = normcdf( (4-1)/1 ) - normcdf( (0-1)/1 ) ;
M = normpdf(1,1,1)/ normcons;

tn_keep = zeros(nkeep,1);
counter = 0;
    while counter < nkeep
        U = rand(2,1);
        U1 = U(1); U2 = U(2);
        if M*U2 < (normpdf(a + (b-a)*U1,1,1)/normcons)
            counter = counter+1;
            x = a + (b-a)*U1;
            tn_keep(counter,1) = x;
        end;
    end;
    
%plotting the actual Triangular and TN_{0,4](1,1) density
xgrid = linspace(-1,1,75);
density = 1 - abs(xgrid);

mu = 1;
sig = 1;
xgrid2 = linspace(0,4,75);
densitya = normpdf(xgrid2,mu,sig);
density2 = densitya/normcons;


%plot the results
[dom ran] = epanech2(triang_keep);
[dom2 ran2] = epanech2(tn_keep);
subplot(121),
plot(dom,ran,'k');
xlabel('X');
ylabel('Triangular Density');
hold on;
plot(xgrid,density,'r.');
subplot(122),
plot(dom2,ran2,'k');
xlabel('X');
ylabel('Truncated Normal Density');
hold on;
plot(xgrid2,density2,'r.');