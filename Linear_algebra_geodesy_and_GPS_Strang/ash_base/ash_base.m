%ASH_BASE Estimation of ambiguities by sequential least squares.
%  	    Goad's "60-77 Algorithm" determines the final ambiguities.
%         Finally, estimation of the baseline vector follows.
%	       During estimation ionospheric delay is set to zero.

%Kai Borre 04-03-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/23 $

%THIS SAMPLE CODE DOES NOT ACCOUNT FOR CYCLE SLIPS
global Ionoplot PHI1 PHI2
tic
ash_dd     % that script arranges and formats observations adequately

% Definition of F matrix
F = [ones(4,1) zeros(4,2)];
F(2,2) = lambda1;
F(4,3) = lambda2;

% ESTIMATION OF AMBIGUITIES
for k = 1:ms-1    % k runs over all used satellites, except ref. sat.
   start = (k-1)*noepochs+1;
   slut = k*noepochs;
   P1 = datar(start:slut,3)-datam(start:slut,3)-datarref(1:noepochs,3)...
                                           +datamref(1:noepochs,3);
   Phi1 = (datar(start:slut,4)-datam(start:slut,4)-datarref(1:noepochs,4)...
                                  +datamref(1:noepochs,4))*lambda1;
   P2 = datar(start:slut,5)-datam(start:slut,5)-datarref(1:noepochs,5)...
                                           +datamref(1:noepochs,5);
   Phi2 = (datar(start:slut,6)-datam(start:slut,6)-datarref(1:noepochs,6)...
                                  +datamref(1:noepochs,6))*lambda2;
   b = [P1 Phi1 P2 Phi2];
   sd = [0.3 0.005 0.3 0.005];% standard deviation of observations
   W = diag(1./sd.^2);	      % diagonal weight matrix
   N22 = zeros(2,2);
   RS21 = zeros(2,1);
   x_aug = [];             	% least squares sol. stored epoch-by-epoch
   b_aug = [];
   FW = F'*W;
   N = FW*F;
   for i = ef:el		% i runs over epochs
      RS = FW*b(i,:)';
      N22 = N22+N(2:3,2:3)-N(2:3,1)*N(2:3,1)'/N(1,1);
      RS21 = RS21+RS(2:3,1)-N(2:3,1)*RS(1,1)/N(1,1);
      x_aug = [x_aug inv(N22)*RS21];
      b_aug = [b_aug RS21];
   end
   x = inv(N22)*RS21;
   K1 = round(x(1)-x(2));
   K2 = round(60*b(i,2)'/lambda1-77*b(i,4)'/lambda2);
   trueN2 = round((60*K1-K2)/17)%;
   trueN1 = round(trueN2+K1)%;
   wlplot = x_aug(1,:)-x_aug(2,:)- (trueN1-trueN2);
   fprintf('Ambiguities between sats. %3.0f and %3.0f:\n',...
                                                  refsv, svs1(k));
   fprintf('N1: %10.1f N2: %10.1f Nw: %10.1f\n',...
                                           x(1), x(2), x(1)-x(2));
   wl = [wl trueN1-trueN2];
   n1 = [n1 trueN1];
   
   t = ef:el;
   figure(2*k-1);
   hold on
   pl = plot(t,wlplot,'o');
   xlabel('Epochs [epoch interval 20 s]')
   ylabel('Wide lane ambiguity [cycle]')
   title(['Double differenced P-code and phase obs. Sv ',...
                           num2str(refsv),' - Sv ',num2str(svs1(k))])
   set(pl,'Markersize',3);
   hold off
   print -deps ddamb1.eps
   
   numerat = 1-(f1/f2)^2;
   ionoplot = (b(ef:el,4)-b(ef:el,2)-lambda2*trueN2+lambda1*trueN1)/numerat;
   Ionoplot(t,k) = ionoplot;
   figure(2*k);
   hold on
   pl1 = plot(t,ionoplot*1.e3,'o');
   xlabel(['Epochs for Obs. Sv ',...
     num2str(refsv),' - Sv ',num2str(svs1(k)),'  [epoch interval 20 s]']) 
                               %,... 'FontSize',16)
   ylabel('Delay  {\itI}   [mm]')  %,'FontSize',16)
   set(pl1,'Markersize',3);
   hold off
   %set(gca,'FontSize',16)
   print -deps ddamb2.eps
end

% CALCULATION OF VECTOR

% Downloading of ephemeris data
Eph = get_eph('edata.dat');
% Preliminary computation of master and rover positions
pseudorange(1,1) = datamref(1,3);
for t = 1:ms-1
   pseudorange(t+1,1) = datam(1+(t-1)*noepochs,3);
end
pos = b_point(pseudorange,[refsv svs1'], datamref(1,1), 'edata.dat');
X_i = pos(1:3,1);
x = zeros(3,1);    % Preliminary vector components
X_j = X_i;
[phi_i,lambda_i,h_i] = ...
                 togeod(6378137,298.257223563,X_i(1),X_i(2),X_i(3));

n2 = n1-wl;
% Computation of weight matrix
D = [ones(ms-1,1) -eye(ms-1) -ones(ms-1,1) eye(ms-1)];
C = inv(D*D');

% We relate each satellite with a fixed ephemeris
time = datarref(1,1);
for t = 1:ms-1
   col_Eph(t) = find_eph(Eph,svs1(t),time);
end
% and the reference satellite
col_Eph_r = find_eph(Eph,refsv,time);

dx = 1;
iter = 0;
Var = 100;  % Initial value for variance of an epoch solution

% Iteration for vector estimation
while norm(dx) > 0.05  % Stop criterion: 0.05 m
   iter = iter+1;
   [phi_j,lambda_j,h_j] =  ...
                   togeod(6378137,298.257223563,X_j(1),X_j(2),X_j(3));
   Normal = zeros(3,3);
   RightSide = zeros(3,1);
   xx = [];
   dx_p = zeros(3,1);
   varsum = 0;
   t1 = 0;
   firstepoch = 4; % We skip the first three epochs due to cold start
   PHI1 = [];
   PHI2 = [];
   ressum = zeros((ms-1)*2,1);
   
   for p = firstepoch:noepochs
      [rhok_j,Xk_ECF] = get_rho(datarref(p,1), datarref(p,3), ...
                                             Eph(:,col_Eph_r), X_j);
      [rhok_i,Xk_ECF] = get_rho(datamref(p,1), datamref(p,3),...
                                             Eph(:,col_Eph_r), X_i);
      for t = 1:ms-1 % t runs over sat.s given in svs1;
         % ref.sat. is not incl.
         [rhol_j,Xl_ECF] = get_rho(datar(p+(t-1)*noepochs,1), ...
                     datar(p+(t-1)*noepochs,3), Eph(:,col_Eph(t)), X_j);
         [rhol_i,Xl_ECF] = get_rho(datam(p+(t-1)*noepochs,1), ...
                     datam(p+(t-1)*noepochs,3), Eph(:,col_Eph(t)), X_i);
         A(t,:) = [(Xk_ECF(1)-X_j(1))/rhok_j ...
                        - (Xl_ECF(1)-X_j(1))/rhol_j  ...
               (Xk_ECF(2)-X_j(2))/rhok_j - (Xl_ECF(2)-X_j(2))/rhol_j  ...
                  (Xk_ECF(3)-X_j(3))/rhok_j - (Xl_ECF(3)-X_j(3))/rhol_j];
         % Tropospheric correction of phases, standard met. parameters
         t_cor = -tropo(sin(datar(p+(t-1)*noepochs,7)*pi/180),...
                   h_j*1.e-3,1013,293,50,0,0,0)...
                +tropo(sin(datam(p+(t-1)*noepochs,7)*pi/180),....
                        h_i*1.e-3,1013,293,50,0,0,0)...
            +tropo(sin(datarref(p,7)*pi/180),h_j*1.e-3,1013,293,50,0,0,0)...
            -tropo(sin(datamref(p,7)*pi/180),h_i*1.e-3,1013,293,50,0,0,0);
         Phi1 = datar(p+(t-1)*noepochs,4)*lambda1...
            -datam(p+(t-1)*noepochs,4)*lambda1...
            -datarref(p,4)*lambda1+datamref(p,4)*lambda1 - t_cor;
         Phi2 = datar(p+(t-1)*noepochs,6)*lambda2...
            -datam(p+(t-1)*noepochs,6)*lambda2...
            -datarref(p,6)*lambda2+datamref(p,6)*lambda2 - t_cor;
         b1(t,:) = [Phi1-lambda1*n1(t)-rhok_i+rhok_j+rhol_i-rhol_j];
         b2(t,:) = [Phi2-lambda2*n2(t)-rhok_i+rhok_j+rhol_i-rhol_j];
         % Accumulation of data for primitive test of cycle slips
         phi1(t) = Phi1;
         phi2(t) = Phi2;
      end;
      
      PHI1 = [PHI1 phi1];
      PHI2 = [PHI2 phi2];
      
      b = [b1; b2];
      Aaug = [A; A];
      M = Aaug'*kron(eye(2),C);
      RS = M*b;
      dx_p = inv(M*Aaug)*RS;
      res_p = Aaug*dx_p-b;
      sigma_p = sqrt(res_p'*kron(eye(2),C)*res_p/(2*(ms-1)));
      % Simple test for outliers
      if (iter == 1) | (sigma_p < 1.5*sqrt(Var))  %  3*sigma
         xx = [xx dx_p];
         varsum = varsum+sigma_p^2;
         ressum = [ressum res_p];
         Normal = Normal + M*Aaug;
         RightSide = RightSide + M*b;
         t1 = t1+1;
      end;
   end;  % end p
   dx = inv(Normal)*RightSide;
   Var = varsum/t1;
   S = Var*inv(Normal);
   X_j = X_j+dx;
   sigma_X = sqrt(S(1,1));
   sigma_Y = sqrt(S(2,2));
   sigma_Z = sqrt(S(3,3));
   fprintf('\n');
   fprintf('Iteration # %2.0f\n', iter);
   fprintf(['Correction x, y, z: %8.3f'....
                          ' %8.3f %8.3f\n'], dx(1),dx(2),dx(3));
   fprintf(['Sigma x, y, z:      %8.3f'....
                    ' %8.3f %8.3f\n'], sigma_X,sigma_Y,sigma_Z);
   fprintf('Epochs rejected %2.0f\n', noepochs-firstepoch+1-t1);
   rho = corrcoef(S);
end; % end iter

% Correction for antenna heights
fids = fopen('sdata.dat');
[hmr] = fread(fids,2,'double');
hd = hmr(2)-hmr(1); % master - rover
cl1 = cos(lambda_i); sl1 = sin(lambda_i);
cb1 = cos(phi_i); sb1 = sin(phi_i);
up = [cb1*cl1; cb1*sl1; sb1]*hd;
vect = X_j-X_i - up;
fprintf(['\nFINAL VALUES FOR VECTOR\n deltaX: %10.3f'....
                   ' deltaY: %10.3f deltaZ: %10.3f\n'],...
                                  vect(1),vect(2),vect(3));
LENGTH = norm(vect);
figure(2*(ms-1)+1)
plot(xx(1,:)*1.e3)
ylabel('Residuals in x [mm]')
title('Vector Estimation');

figure(2*(ms-1)+2)
plot(xx(2,:)*1.e3)
ylabel('Residuals in y [mm]')
title('Vector Estimation');

figure(2*(ms-1)+3)
plot(xx(3,:)*1.e3)
ylabel('Residuals in z [mm]')
title('Vector Estimation');

figure(2*(ms-1)+4)
plot(ressum(1,:)*1.e3)
ylabel(['L1 Residuals, Sv ',...
                    num2str(refsv),' - Sv ',num2str(svs1(1)),' [mm]'])
title(['Vector Estimation Through All ',num2str(iter),' Iterations']);

figure(2*(ms-1)+5)
plot(ressum(ms,:)*1.e3)
ylabel(['L2 Residuals, Sv ',...
                    num2str(refsv),' - Sv ',num2str(svs1(1)),' [mm]'])
title(['Vector Estimation Through All ',num2str(iter),' Iterations']);
fprintf('\nElapsed time (sec): %3.2f\n', toc);

% For studying the residuals try calls like
%     plot(ressum(1,:))
%     plot(ressum(2,:))
%     .........
%     plot(ressum(2*(ms-1),:))
%%%%%%%%% end ash_base.m %%%%%%%%%
