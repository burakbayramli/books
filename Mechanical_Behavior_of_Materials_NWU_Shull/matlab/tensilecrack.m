close all
% first set a few defaults so the plots look pretty
set(0','defaultfigurepapersize',[12 5])
set(0,'defaultfigurepaperposition',[0 0 12 5])
set(0,'defaultaxesfontsize',16)

% now we define the different stress components
sigxx=@(theta) cosd(theta/2).*(1-sind(theta/2).*sind(1.5*theta));
sigyy=@(theta) cosd(theta/2).*(1+sind(theta/2).*sind(1.5*theta));
sigxy=@(theta) cosd(theta/2).*sind(theta/2).*cosd(1.5*theta);
sigma=@(theta) [sigxx(theta), sigxy(theta); sigxy(theta), sigyy(theta)];

% now plot the stress components
thetavals=linspace(-180,180,60);
subplot(1,2,1)
plot(thetavals, sigxx(thetavals), 'b-o', thetavals, sigyy(thetavals), 'r-+', thetavals, sigxy(thetavals), 'r--')
set(gca,'XTick', linspace(-180,180,7));
xlim([-180 180])
xlabel('\theta (deg)')
ylabel('\sigma (2\pi d)^{1/2}/K_I')
legend('\sigma_{xx}', '\sigma_{yy}', '\sigma_{xy}', 'location', 'northeast')

% now calculate the normal stress.  We want sigma_yy in a coordinate system
% that is rotated by theta.

sigmanorm=[]; %  start with an empty matrx i
for theta=linspace(-180, 180, 60);  
    thetamatrix=[theta,90-theta;90+theta,theta]; % just use the 2d matrix
    Q=cosd(thetamatrix);
    QT=transpose(Q);
    sigp=Q*sigma(theta)*QT;
    sigmanorm=[sigmanorm sigp(2,2)];
end

subplot(1,2,2)
plot(thetavals, sigmanorm,'-+b')
xlim([-180 180])
set(gca,'XTick', linspace(-180,180,7));
xlabel('\theta (deg)')
ylabel('\sigma (2\pi d)^{1/2}/K_I')
legend('\sigma_{N}','location', 'northeast')
print(gcf, '../figures/crackstresses.svg', '-dsvg')
