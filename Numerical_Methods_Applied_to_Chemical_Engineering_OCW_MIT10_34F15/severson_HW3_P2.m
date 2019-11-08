function lee_HW4_P1()
% Written by Mark Molaro, Fall 2013
% Modified by Liza Lee, Oct. 13, 2014

close all
%clc

% Set the rates
Nspecies = 5;
Rate.A = zeros(Nspecies,1);
Rate.Ea_R = zeros(Nspecies,1);

% calculate rate constants
T1 = 298; % K
T2 = 310; % K
k1 = [0.01,0.01,0.001,0.001,0.001]; % k at T1
k2 = [0.02,0.02,0.005,0.005,0.005];  % k at T2
x1 = 1/T1;
x2 = 1/T2;
for i=1:Nspecies
   y1=log(k1(i));
   y2=log(k2(i));
   Rate.Ea_R(i) = -1*(y2-y1)/(x2-x1);
   Rate.A(i) = k1(i)*exp(Rate.Ea_R(i)*x1);
end

% Initialize variables

% upper and lower bounds
% order: alpha, V, T
lb = [0.0000001;100;298]; % lower bound
ub = [100;10000;335]; % upper bound
options = optimoptions('fmincon','algorithm','interior-point','TolFun',10e-10,'TolX',10e-10);

%% Optimize Cc
Ngrid = 5;
param0 = initial_guess(@concobjective,lb,ub,Rate,Ngrid); % calculate best initial guess
[optimal,fval] = fmincon(@(x)(-1)*concobjective(x,Rate),param0,[],[],[],...
    [],lb,ub,[],options);
format long
fprintf('Concentration of C maximized (alpha, V, T)\n')
disp(optimal)
fprintf('Cc = %6.4e\n',-1*fval)

% Make contour plot for maximizing Cc
Ngrid = 25;
[Aplot,Vplot]=ndgrid(linspace(0.01,5,Ngrid),linspace(100,10000,Ngrid));
obj1 = zeros(size(Aplot));
T = optimal(3);
for i = 1:Ngrid
    for j = 1:Ngrid
        obj1(i,j)=concobjective([Aplot(i,j);Vplot(i,j);T],Rate);
    end
end

figure(1);
hold on
contourf(Aplot,Vplot,obj1,25);
title('Concentration in Exit Stream Contour Plot');
xlabel('\alpha value');
ylabel('Volume');
h=plot(optimal(1),optimal(2),'gs','MarkerSize',10);
set(h, 'MarkerFaceColor', get(h, 'Color'));
t = colorbar('peer',gca); 
set(get(t,'ylabel'),'String', 'C_C');
hold off 

%% Optimize conversion
Ngrid = 5;
param1 = initial_guess(@convobjective1,lb,ub,Rate,Ngrid); % calculate best initial guess
[optimal,fval] = fmincon(@(x)(-1)*convobjective1(x,Rate),param1,[],[],[],...
    [],lb,ub,[],options);
format long
fprintf('Conversion of A maximized (alpha, V, T)\n')
disp(optimal)
fprintf('Conversion of A = %6.4e\n',-1*fval)

% Make contour plot for maximizing Cc/Ca_in
Ngrid=25;
[Aplot,Vplot]=ndgrid(linspace(0.0000001,0.01,Ngrid),linspace(100,10000,Ngrid));
obj2 = zeros(size(Aplot));
T = optimal(3);
for i = 1:Ngrid
    for j = 1:Ngrid
        obj2(i,j)=convobjective1([Aplot(i,j);Vplot(i,j);T],Rate);
    end
end

figure(2);
hold on
contourf(Aplot,Vplot,obj2,25);
title('Conversion (C/A) in Exit Stream Contour Plot');
xlabel('\alpha value');
ylabel('Volume');
h=plot(optimal(1),optimal(2),'gs','MarkerSize',10);
set(h, 'MarkerFaceColor', get(h, 'Color'));
t = colorbar('peer',gca); 
set(get(t,'ylabel'),'String', 'C_C/C_{A,in}');
hold off

%% Optimize Cc/(Ca_in-Ca)
Ngrid = 5;
param2 = initial_guess(@yiedlobjective2,lb,ub,Rate,Ngrid); % calculate best initial guess
[optimal,fval] = fmincon(@(x)(-1)*yiedlobjective2(x,Rate),param2,[],[],[],...
    [],lb,ub,[],options);
format long
fprintf('Cc/(Ca_in-Ca) maximized (alpha, V, T)\n')
disp(optimal)
fprintf('Cc/(Ca_in-Ca) = %6.4e\n',-1*fval)

% Make contour plot for maximizing Cc/(Ca_in-Ca)
Ngrid=25;
[Aplot,Vplot]=ndgrid(linspace(5,100,Ngrid),linspace(100,10000,Ngrid));
obj3 = zeros(size(Aplot));
T = optimal(3);
for i = 1:Ngrid
    for j = 1:Ngrid
        obj3(i,j)=yiedlobjective2([Aplot(i,j);Vplot(i,j);T],Rate);
    end
end

figure(3);
hold on
contourf(Aplot,Vplot,obj3,25);
title('Conversion (C/(Ain-Aout)) in Exit Stream Contour Plot');
xlabel('\alpha value');
ylabel('Volume');
h=plot(optimal(1),optimal(2),'gs','MarkerSize',10);
set(h, 'MarkerFaceColor', get(h, 'Color'));
t = colorbar('peer',gca); 
set(get(t,'ylabel'),'String', 'C_C/(C_{A,in}-C_A)');
hold off 
% 
% param0
% param1
% param2
% keyboard
% 
end

function best_guess = initial_guess(func,lb,ub,Rate,Ngrid)
% Grid the parameters 

alphas = linspace(lb(1),ub(1),Ngrid);
Vs = linspace(lb(2),ub(2),Ngrid);
Ts = linspace(lb(2),ub(2),Ngrid);

[X1,X2,X3] = ndgrid(alphas,Vs,Ts);

objconcc = zeros(size(X1));
%objconca = zeros(size(X1));
for i = 1:Ngrid
    for j = 1:Ngrid
        for k = 1:Ngrid
            objconcc(i,j,k) = func([X1(i,j,k);X2(i,j,k);X3(i,j,k)],Rate);
          %  objconca(i,j,k) = concobjective([X1(i,j,k);X2(i,j,k);X3(i,j,k)],Rate);
        end
    end
end

% Concentration Objective
best = max(max(max(objconcc)));
idx=find(objconcc == best);
alpha0 = X1(idx(1));
V0 = X2(idx(1));
T0 = X3(idx(1));
best_guess = [alpha0,V0,T0];

end

function C_ss = steadystatecstr(alpha,V,T,Rate)

Bin = 0.1/(1+alpha);
Ain = alpha*Bin;
Q = 10;
%x0 = [0.04 0.01 0.02 0.02]';
x0 = [0.1 0.1 0.1 0.1]';

options = optimoptions('fsolve','Display','none','TolFun',10e-20,'TolX',10e-10);
[C_ss,fval,exitflag,output,Jac]=fsolve(@(x)mycstr(x,Q,Rate,Ain,Bin,V,T,false),x0,options);

if any(eig(Jac)>0)
    C_ss
    error('Unstable');
end

end

% 8.28.15 removed from new problem statement
function val = concobjective(inparams,Rate)
alpha = inparams(1);
V = inparams(2);
T = inparams(3);

C_ss = steadystatecstr(alpha,V,T,Rate);
val = C_ss(3);
end

function val = convobjective1(inparams,Rate)
alpha = inparams(1);
V = inparams(2);
T = inparams(3);

C_ss = steadystatecstr(alpha,V,T,Rate);

val = 1 - C_ss(1)/(alpha*0.1/(alpha+1));
end

function val = yiedlobjective2(inparams,Rate)
alpha = inparams(1);
V = inparams(2);
T = inparams(3);

C_ss = steadystatecstr(alpha,V,T,Rate);
val = C_ss(3)/((alpha*0.1/(alpha+1))-C_ss(1));
end


function f = mycstr(x,Q,Rate,cA_in,cB_in,V,T,eigprint)


% f should be nearly 0 at steady state

if min(x)<0 %dirty hack to avoid concentration ever being negative
    
     f =[10;10;10;10];
else

     c = x(1:4);
     f = zeros(size(c)); 

     % extract concentrations from state vector 
     cA = c(1); cB = c(2); cC = c(3); cD = c(4); 

     % calculate reactor rates 
     rxn_r = zeros(5,1); 

     k_rxn = zeros(5,1);
     for irxn = 1:5 
        k_rxn(irxn) = Rate.A(irxn) * exp(-Rate.Ea_R(irxn)/T); 
     end
     mean_RT = V/Q;

     rxn_r(1) = k_rxn(1)*cA*cB; 
     rxn_r(2) = k_rxn(2)*cC*cB; 
     rxn_r(3) = k_rxn(3)*cA*cD; 
     rxn_r(4) = k_rxn(4)*cA*cB; 
     rxn_r(5) = k_rxn(5)*cC*cB; 

     rxn_r = abs(rxn_r);
     % A balance 
     f(1) = (cA_in - cA)/mean_RT - rxn_r(1) - rxn_r(3) - rxn_r(4); 

     % B balance 
     f(2) = (cB_in - cB)/mean_RT - rxn_r(1) - rxn_r(2) - rxn_r(4) - rxn_r(5); 

     % C balance 
     f(3) = -cC/mean_RT + rxn_r(1) - rxn_r(2) - rxn_r(5); 

     % D balance 
     f(4) = -cD/mean_RT + rxn_r(1) + rxn_r(2) - rxn_r(3); 
    if eigprint == true
         k1 = k_rxn(1);
         k2 = k_rxn(2);
         k3 = k_rxn(3);
         k4 = k_rxn(4);
         k5 = k_rxn(5);
         Jac = [ - cB*k1 - cB*k4 - cD*k3 - 1/mean_RT,- cA*k1 - cA*k4,0,-cA*k3; ...
        - cB*k1 - cB*k4, - cA*k1 - cA*k4 - cC*k2 - cC*k5 - 1/mean_RT,- cB*k2 - cB*k5,0; ...
        cB*k1,cA*k1 - cC*k2 - cC*k5, - cB*k2 - cB*k5 - 1/mean_RT,0; ...
        cB*k1 - cD*k3,cA*k1 + cC*k2,cB*k2, - cA*k3 - 1/mean_RT];
        theeigs =eig(Jac);
        disp('Eigenval')
        disp(theeigs);
    end
end

end