function [sol,histout]=hc_driver_parallel
v=zeros(12,1);
v(1) = 150; v(2) = 750; v(3) = 400; v(4) = 750;
v(5) = 250; v(6) = 650; v(7) = 250; v(8) = 450;
v(9) = 0.0064; v(10) = 0.0064; v(11) = -0.0064; v(12) = -0.0064;
minrate=1.d-6;
working_directory='hc_tmp';
hc_clean(working_directory);
budget=500;
fscale=9.6d4;
options=imfil_optset('fscale',fscale,'parallel',1,'stencil_wins',1);
bounds=zeros(12,2);
%
% Bounds on well locations. Keep them away from the boundary
%
for i=1:8
    bounds(i,1)=10.0;
    bounds(i,2)=990.0;
end
%
% Constraints on pumping rates.
%
for i=9:12
    bounds(i,1)=-.0064;
    bounds(i,2)=.0064;
end
%
% Make the optimization landscape at the initial iterate.
% Pick two coordinates to vary.
%
n1=1;
n2=10;
%
% Make an nl x nl grid for the landscape plot
%
nl=21;
xlab='Q(1)';
ylab='x(1)';
figure(1)
plot_landscape(v, nl, n1, n2, xlab, ylab, bounds, @hcevalp, working_directory);
%
% Solve the problem
%
[sol, histout]=imfil(v, @hcevalp, budget, bounds,options,working_directory);
%
% Make a nice history plot.
%
figure(2)
p1=subplot(1,1,1);
set(p1,'FontSize',14);
plot(histout(:,1),histout(:,2),'LineWidth',1,'Color','black');
xlabel('Calls to Modflow','FontSize',14);
ylabel('Cost','FontSize',14);
%
% Make the landscape near the final result.
%
n1=6;
n2=5;
nl=41;
xlab='x(3)';
ylab='y(3)';
bounds(6,1)=sol(6)*.9;
bounds(6,2)=sol(6)*1.1;
bounds(5,1)=sol(5)*.9;
bounds(5,2)=sol(5)*1.1;
bounds(5,:)=[240, 270]';
bounds(6,:)=[600, 660]';
figure(3)
plot_landscape(sol,nl, n1, n2, xlab, ylab, bounds, @hcevalp, working_directory);

function hc_clean(working_directory)
%
% Start things off right by clearing the old files from the
% working directory.
%
mkdir=['mkdir ',working_directory];
headfiles=['rm -f ',working_directory,'/HC.hed*'];
wellfiles=['rm -f ',working_directory,'/HC.wel*'];
configfiles=['rm -f ',working_directory,'/Config*'];
[ss,rr]=system(mkdir);
[ss,rr]=system(headfiles);
[ss,rr]=system(wellfiles);
[ss,rr]=system(configfiles);

