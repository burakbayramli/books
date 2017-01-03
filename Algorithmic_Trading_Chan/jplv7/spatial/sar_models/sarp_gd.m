% Demo based on:
% LeSage, James, R. Kelley Pace, Nina Lamm, Richard Campanella and
% Xingjian Liu, New Orleans business recovery in the aftermath of 
% Hurricane Katrina

clear all;

% ========== read data for 3 streets

load probit_demo.data;
% col1  y=open, 1 = store open, 0 = store closed            
% col2  constant          
% col3  flood depth       
% col4  log(median income)
% col5  small firm size   
% col6  large firm size   
% col7  low social status 
% col8  high social status
% col9  sole prop         
% col10 national chain    
% col11 long              
% col12 latt         

y = probit_demo(:,1);

long = probit_demo(:,11);
latt = probit_demo(:,12);

W = make_neighborsw(latt,long,15);

x = probit_demo(:,2:10);


vnames = strvcat('y=open','constant','flood depth','log(median income)', ...
    'small size','large size','low ses','high ses', ...
    'ownerdum1','ownerdum2');



ndraw = 1200;
nomit = 200;
prior.nsample=5;

results = sarp_g(y,x,W,ndraw,nomit,prior);
prt(results,vnames);

total = results.total_obs;

% plot total effects versus latt-long

plot3(latt,long,total(:,1),'.');
xlabel('latt');
ylabel('long');
zlabel('flood depth');
pause;

plot3(latt,long,total(:,2),'.');
xlabel('latt');
ylabel('long');
zlabel('income');
pause;

plot3(latt,long,total(:,3),'.');
xlabel('latt');
ylabel('long');
zlabel('small firm size');
pause;

plot3(latt,long,total(:,4),'.');
xlabel('latt');
ylabel('long');
zlabel('large firm size');
pause;

plot3(latt,long,total(:,5),'.');
xlabel('latt');
ylabel('long');
zlabel('low social economic status');
pause;

plot3(latt,long,total(:,6),'.');
xlabel('latt');
ylabel('long');
zlabel('high social economic status');
pause;

plot3(latt,long,total(:,7),'.');
xlabel('latt');
ylabel('long');
zlabel('sole prop');
pause;

plot3(latt,long,total(:,8),'.');
xlabel('latt');
ylabel('long');
zlabel('national chain');



