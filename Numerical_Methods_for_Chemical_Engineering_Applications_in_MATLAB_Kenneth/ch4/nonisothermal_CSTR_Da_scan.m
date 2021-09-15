% nonisothermal_CSTR_Da_scan.m
% This MATLAB program performs arc-length
% continuation to plot phi_A and theta
% vs. Da at fixed values of the other parameters.
% Actually, the system steps in log space of the Damkoehler
% number.
% K.J. Beers. MIT ChE. 9/24/03
function iflag_main = nonisothermal_CSTR_Da_scan();
iflag_main = 0;

% First, input the system parameters
disp('nonisothermal_CSTR_Da_scan.m:');
disp('Enter parameters ...');
disp(' ');
disp('Da is Damkoehler #, inportance of rxn to convection.');
Da_0 = input('Enter lowest Da number : '); Da_0_log = log10(Da_0);
Da_1 = input('Enter highest Da number : '); Da_1_log = log10(Da_1);
disp(' '); disp('beta is dimensionless heat of reaction');
beta = input('Enter fixed beta value : ');
disp(' '); disp('chi is dimensionless cooling heat transfer strength');
chi = input('Enter fixed chi value : ');
disp(' '); disp('gamma is dimensionless activation energy');
gamma = input('Enter fixed gamma value : ');
disp(' '); disp('theta_c is dimensionless temp of coolant');
theta_c = input('Enter fixed theta_c value : ');

ArcLenParam.atol = 1e-6;
    
% generate linear path in parameter space by changing
% Damkoehler number logarithmically
Param_0 = [Da_0_log; beta; chi; gamma; theta_c];
Param_1 = [Da_1_log; beta; chi; gamma; theta_c];
    
% set initial guess for solution at Param_0
x0 = [1; 1];
    
% call arc-length continuation routine to generate curve
fun_name = 'nonisothermal_CSTR_calc_f';
[x,Param,lambda,f_norm,stab] = ...
    arclength_continuation(fun_name,Param_0,Param_1,x0,ArcLenParam);

% extract Da numbers along graph
Da = 10.^Param(1,:);

% Next, identify where stability changes along curve
% record values for first point
istab_point(1) = 1;
count = 1;
for k=2:length(stab)
    if(stab(k) ~= stab(k-1))  % if change in stability
        count = count + 1;
        istab_point(count) = k;
    end
end
% record last point
istab_point(count+1) = length(stab);
% Now, go along curves and plot each section, using
% a solid line for stable sections and a dashed line
% for unstable section
figure;
for isection=1:count
    istart = istab_point(isection);
    iend = istab_point(isection+1);
    if(stab(istart))  % if stable
        subplot(2,1,1);
        semilogx(Da(istart:iend),x(1,istart:iend),'-');
        hold on;
        subplot(2,1,2);
        semilogx(Da(istart:iend),x(2,istart:iend),'-');
        hold on;
    else  % if unstable
        subplot(2,1,1);
        semilogx(Da(istart:iend),x(1,istart:iend),'-.');
        hold on;
        subplot(2,1,2);
        semilogx(Da(istart:iend),x(2,istart:iend),'-.');
        hold on;
    end
    % for intermediate points, demark points at which
    % stability changes by a circle
    if(istart ~= 1)
        subplot(2,1,1);
        semilogx(Da(istart),x(1,istart),'o');
        subplot(2,1,2);
        semilogx(Da(istart),x(2,istart),'o');
    end
end
% add labels and title
subplot(2,1,1);
xlabel('Da');  ylabel('\phi_A');
title_phrase = ['\phi_A, \theta vs. Da: ', ...
        '\beta = ', num2str(beta), ...
        ', \chi = ', num2str(chi), ...
        ', \gamma = ', num2str(gamma), ...
        ', \theta_c = ', num2str(theta_c)];
title(title_phrase);
subplot(2,1,2)
xlabel('Da');  ylabel('\theta');

% now make separate plot of infinity norm of f vs. lambda as
% check that the arclength continuation worked correctly.
figure;  semilogy(lambda,f_norm);
xlabel('lambda');  ylabel('||f||_{infinity}');
title('Infinity norm of f along curve');
axis tight;

iflag_main = 1;
return;
