%% Function prototype of the HLLE2d
% Coded by Manuel A. Diaz, NTU, 2015.11.08
clear; clc; close all;
global gamma; gamma = 1.4;

% Build a 2x2 mesh
dx=0.5; dy=0.5; [x,y]=meshgrid([0,1],[0,1]);
[r0,u0,v0,p0] = Euler_IC2d(x,y,03);
E0 = p0./(gamma-1)+0.5*r0.*(u0.^2+v0.^2); % Total Energy
c0 = sqrt(gamma*p0./r0);                  % Speed of sound
q0 = cat(3, r0, r0.*u0, r0.*v0, E0);      % initial state
 
% The corner data is obtained as
i=1; j=1;
qSW = squeeze( q0( i , j ,:) );
qSE = squeeze( q0( i ,j+1,:) );
qNW = squeeze( q0(i+1, j ,:) );
qNE = squeeze( q0(i+1,j+1,:) );


% West state
rSW = qSW(1);
uSW = qSW(2)/rSW;
vSW = qSW(3)/rSW;
pSW = (gamma-1)*( qSW(4) - rSW*(uSW^2+vSW^2)/2 );
aSW = sqrt(gamma*pSW/rSW);
HSW = ( qSW(4) + pSW ) / rSW;

% East state
rSE = qSE(1);
uSE = qSE(2)/rSE;
vSE = qSE(3)/rSE;
pSE = (gamma-1)*( qSE(4) - rSE*(uSE^2+vSE^2)/2 );
aSE = sqrt(gamma*pSE/rSE);
HSE = ( qSE(4) + pSE ) / rSE;

% South state
rNW = qNW(1);
uNW = qNW(2)/rNW;
vNW = qNW(3)/rNW;
pNW = (gamma-1)*( qNW(4) - rNW*(uNW^2+vNW^2)/2 );
aNW = sqrt(gamma*pNW/rNW);
HNW = ( qNW(4) + pNW ) / rNW;

% North state
rNE = qNE(1);
uNE = qNE(2)/rNE;
vNE = qNE(3)/rNE;
pNE = (gamma-1)*( qNE(4) - rNE*(uNE^2+vNE^2)/2 );
aNE = sqrt(gamma*pNE/rNE);
HNE = ( qNE(4) + pNE ) / rNE;




% Compute Roe Averages - SW to SE
rSroe = sqrt(rSE/rSW); 
uSroe = (uSW+rSroe*uSE)/(1+rSroe);
vSroe = (vSW+rSroe*vSE)/(1+rSroe);
HSroe = (HSW+rSroe*HSE)/(1+rSroe);
aSroe = sqrt( (gamma-1)*(HSroe-0.5*(uSroe^2+vSroe^2)) );

% Compute Roe Averages - NW to NE
rNroe = sqrt(rNE/rNW); 
uNroe = (uNW+rNroe*uNE)/(1+rNroe);
vNroe = (vNW+rNroe*vNE)/(1+rNroe);
HNroe = (HNW+rNroe*HNE)/(1+rNroe);
aNroe = sqrt( (gamma-1)*(HNroe-0.5*(uNroe^2+vNroe^2)) );

% Compute Roe Averages - SW to NW
rWroe = sqrt(rNW/rSW); 
uWroe = (uSW+rWroe*uNW)/(1+rWroe);
vWroe = (vSW+rWroe*vNW)/(1+rWroe);
HWroe = (HSW+rWroe*HNW)/(1+rWroe);
aWroe = sqrt( (gamma-1)*(HWroe-0.5*(uWroe^2+vWroe^2)) );

% Compute Roe Averages - SE to NE
rEroe = sqrt(rNE/rSE); 
uEroe = (uSE+rEroe*uNE)/(1+rEroe);
vEroe = (vSE+rEroe*vNE)/(1+rEroe);
HEroe = (HSE+rEroe*HNE)/(1+rEroe);
aEroe = sqrt( (gamma-1)*(HEroe-0.5*(uEroe^2+vEroe^2)) );




% Wave speed estimates in the S
sSW = min([ uSW-aSW, uSW+aSW, uSroe-aSroe, uSroe+aSroe ]); sSWplus=max(sSW,0);
sSE = max([ uSE-aSE, uSE+aSE, uSroe-aSroe, uSroe+aSroe ]); sSEplus=max(sSE,0);

% Wave speed estimates in the N
sNW = min([ uNW-aNW, uNW+aNW, uNroe-aNroe, uNroe+aNroe ]); sNWplus=max(sNW,0);
sNE = max([ uNE-aNE, uNE+aNE, uNroe-aNroe, uNroe+aNroe ]); sNEplus=max(sNE,0);

% Wave speed estimates in the W
sWS = min([ vSW-aSW, vSW+aSW, vWroe-aWroe, vWroe+aWroe ]); sWSplus=max(sWS,0);
sWN = max([ vNW-aNW, vNW+aNW, vWroe-aWroe, vWroe+aWroe ]); sWNplus=max(sWN,0);

% Wave speed estimates in the E
sES = min([ vSE-aSE, vSE+aSE, vEroe-aEroe, vEroe+aEroe ]); sESplus=max(sES,0);
sEN = max([ vNE-aNE, vNE+aNE, vEroe-aEroe, vEroe+aEroe ]); sENplus=max(sEN,0);

% The maximum wave speed delimit the interacting region to a square domain
sS  = min(sWS,sES); 
sN  = max(sWN,sEN); 
sW  = min(sSW,sNW); 
sE  = max(sSE,sNE); 

% Velocities at cells intersections
sW_hat = sSW-sWS*(sNW-sSW)/(sWN-sWS);
sE_hat = sNE-sEN*(sSE-sNE)/(sES-sEN);
sS_hat = sES-sSE*(sES-sWS)/(sSE-sSW);
sN_hat = sWN-sNW*(sWN-sEN)/(sNW-sNE);


% Verify, Verify, Verify!
[x,y] = meshgrid([-dx/2,0,dx/2],[-dy/2,0,dy/2]);
surf(x,y,zeros(3)); hold on; dt = 0.1;
xs = [sSW*dt,sNW*dt,sNE*dt,sSE*dt,0,0]';
ys = [sWS*dt,sWN*dt,sEN*dt,sES*dt,0,0]';
zs = [dt,dt,dt,dt,0,dt]';
DT = delaunayTriangulation(xs,ys,zs);
scatter3([sNE,sNW,sSE,sSW]*dt,[sEN,sWN,sES,sWS]*dt,[dt,dt,dt,dt]);
rectangle('Position',[sW*dt sS*dt (sE-sW)*dt (sN-sS)*dt]);
scatter3([0,0,sE*dt,sW*dt],[sN*dt,sS*dt,0,0],[dt,dt,dt,dt],...
    'MarkerEdgeColor','k','MarkerFaceColor',[0 .75 .75]);
scatter3([0,0,sE_hat*dt,sW_hat*dt],[sN_hat*dt,sS_hat*dt,0,0],[dt,dt,dt,dt],...
    'MarkerEdgeColor','k','MarkerFaceColor',[1 1 1]);
xlabel('x'); ylabel('y'); tetramesh(DT); hold off; view(0,90);



% Compute fluxes
fSW = [rSW*uSW; rSW*uSW*uSW + pSW; rSW*vSW*uSW; rSW*uSW*HSW];
fSE = [rSE*uSE; rSE*uSE*uSE + pSE; rSE*vSE*uSE; rSE*uSE*HSE];
fNW = [rNW*uNW; rNW*uNW*uNW + pNW; rNW*vNW*uNW; rNW*uNW*HNW];
fNE = [rNE*uNE; rNE*uNE*uNE + pNE; rNE*vNE*uNE; rNE*uNE*HNE];

gSW = [rSW*vSW; rSW*vSW*uSW; rSW*vSW*vSW + pSW; rSW*vSW*HSW];
gSE = [rSE*vSE; rSE*vSE*uSE; rSE*vSE*vSE + pSE; rSE*vSE*HSE];
gNW = [rNW*vNW; rNW*vNW*uNW; rNW*vNW*vNW + pNW; rNW*vNW*HNW];
gNE = [rNE*vNE; rNE*vNE*uNE; rNE*vNE*vNE + pNE; rNE*vNE*HNE];

% Compute the intermediate states
qSO = ( sSE*qSE - sSW*qSW + fSW-fSE )/(sSE-sSW); % verified
qNO = ( sNE*qNE - sNW*qNW + fNW-fNE )/(sNE-sNW); % verified
qOW = ( sWN*qNW - sWS*qSW + gSW-gNW )/(sWN-sWS); % verified
qOE = ( sEN*qNE - sES*qSE + gSE-gNE )/(sEN-sES); % verified

% Compute the intermediate states fluxes (normal HLLE 1d fluxes)
fSO = ( sSE*fSW - sSW*fSE + sSW*sSE*(qSE-qSW) )/(sSE-sSW); % verified
fNO = ( sNE*fNW - sNW*fNE + sNW*sNE*(qNE-qNW) )/(sNE-sNW); % verified
gOW = ( sWN*gSW - sWS*gNW + sWS*sWN*(qNW-qSW) )/(sWN-sWS); % verified
gOE = ( sEN*gSE - sES*gNE + sES*sEN*(qNE-qSE) )/(sEN-sES); % verified

% Compute the transverse intermediate fluxes (Balsara's solution)
fOW = [qOW(2);gOW(3)+(qOW(2)^2-qOW(3)^2)/qOW(1);qOW(3)*qOW(2)/qOW(1);qOW(2)*gOW(4)/(qOW(3)+eps)];
fOE = [qOE(2);gOE(3)+(qOE(2)^2-qOE(3)^2)/qOE(1);qOE(3)*qOE(2)/qOE(1);qOE(2)*gOE(4)/(qOE(3)+eps)];
gSO = [qSO(3);qSO(2)*qSO(3)/qSO(1);fSO(2)+(qSO(3)^2-qSO(2)^2)/qSO(1);qSO(3)*fSO(4)/(qSO(2)+eps)];
gNO = [qNO(3);qNO(2)*qNO(3)/qNO(1);fNO(2)+(qNO(3)^2-qNO(2)^2)/qNO(1);qNO(3)*fNO(4)/(qNO(2)+eps)];

% Verify with constant state
%disp([fNW,fNO,fNE;fOW,nan(4,1),fOE;fSW,fSO,fSE]) % everything is good ;)
%disp([gNW,gNO,gNE;gOW,nan(4,1),gOE;gSW,gSO,gSE]) % everything is good ;)

% Area of the main quadrilateral
aOO = (dt^2/2)*((sNE-sSW)*(sWN-sES)+(sEN-sWS)*(sSE-sNW)); %disp(aOO);
%a22 = polyarea([sNE,sNW,sSW,sSE]*dt,[sEN,sWN,sWS,sES]*dt); %disp(a22);
%disp(aOO==a22) % verified!

% Strongly Interacting state q**
qOO = 1/((sNE-sSW)*(sWN-sES)+(sEN-sWS)*(sSE-sNW)) * ( ...
  (sWN*sNE+sSE*sEN)*qNE - (sEN*sNW+sSW*sWN)*qNW + (sES*sSW+sNW*sWS)*qSW - (sWS*sSE+sNE*sES)*qSE ...
   - sWN*fNE + sEN*fNW - sES*fSW + sWS*fSE - (sEN-sES)*fOE + (sWN-sWS)*fOW ...
   - sSE*gNE + sSW*gNW - sNW*gSW + sNE*gSE - (sNE-sNW)*gNO + (sSE-sSW)*gSO );

% Verify with constant state
%disp([qNW,qNO,qNE;qOW,qOO,qOE;qSW,qSO,qSE]) % Everything is good ;D

%% Form 0: Compute fluxes of the strongly interacting state
% Precompute deltas
dq1 = sNW*sEN-sWN*sNE; df1 = sWN-sEN; dg1 = sNE-sNW;
dq2 = sSW*sWN-sWS*sNW; df2 = sWS-sWN; dg2 = sNW-sSW;
dq3 = sSE*sWS-sES*sSW; df3 = sES-sWS; dg3 = sSW-sSE;
dq4 = sNE*sES-sEN*sSE; df4 = sEN-sES; dg4 = sSE-sNE;

% Using LSQ
b1 = dq1*(qNO-qOO) + df1*fNO + dg1*gNO;
b2 = dq2*(qOW-qOO) + df2*fOW + dg2*gOW;
b3 = dq3*(qSO-qOO) + df3*fSO + dg3*gSO;
b4 = dq4*(qOE-qOO) + df4*fOE + dg4*gOE;

% k-weights
k11 = df1*(dg2^2+dg3^2+dg4^2) - dg1*(df2*dg2+df3*dg3+df4*dg4);
k12 = df2*(dg1^2+dg3^2+dg4^2) - dg2*(df1*dg1+df3*dg3+df4*dg4);
k13 = df3*(dg1^2+dg2^2+dg4^2) - dg3*(df1*dg1+df2*dg2+df4*dg4);
k14 = df4*(dg1^2+dg2^2+dg3^2) - dg4*(df1*dg1+df2*dg2+df3*dg3);
k21 = dg1*(df2^2+df3^2+df4^2) - df1*(df2*dg2+df3*dg3+df4*dg4);
k22 = dg2*(df1^2+df3^2+df4^2) - df2*(df1*dg1+df3*dg3+df4*dg4);
k23 = dg3*(df1^2+df2^2+df4^2) - df3*(df1*dg1+df2*dg2+df4*dg4);
k24 = dg4*(df1^2+df2^2+df3^2) - df4*(df1*dg1+df2*dg2+df3*dg3);

% Computing the determinant numerically
% A = [df1,dg1;df2,dg2;df3,dg3;df4,dg4]; M=A'*A; detM=det(M);
% b = [b1,b2,b3,b4]'; X = inv(M)*A'*b;

% Computing explicitly the determinant
detM = (df1*dg2-df2*dg1)^2 + (df1*dg3-df3*dg1)^2 + (df2*dg4-df4*dg2)^2 + ...
       (df3*dg2-df2*dg3)^2 + (df4*dg1-df1*dg4)^2 + (df4*dg3-df3*dg4)^2 ; % verified!

% compute fluxes of Strongly Interacting state f** and g** 
fOO = (k11*b1 + k12*b2 + k13*b3 + k14*b4)/detM;
gOO = (k21*b1 + k22*b2 + k23*b3 + k24*b4)/detM;

% Report q**, f** and g**
disp('LSQ : q**, f**, g**');
disp([qOO,fOO,gOO]);

%% Method I
A1=[sSE+sSW-sNE-sNW,sNW+sSW-sNE-sSE;...
    sES+sWS-sEN-sWN,sWN+sWS-sEN-sES];

f00=zeros(4,1); g00=zeros(4,1);
for i=1:4
    F00=-dt^2/(4*aOO)*A1*[b1(i)-b3(i);b4(i)-b2(i)]; 
    f00(i)=F00(1); g00(i)=F00(2);
end

% Report q**, f** and g**
disp('MthI : q**, f**, g**');
disp([qOO,f00,g00]);

%% Method II
% Precompute c1 and c2
c1 = dq1*dq3*(qNO-qSO) + df1*dq3*fNO - df3*dq1*fSO + dg1*dq3*gNO - dg3*dq1*gSO;
c2 = dq4*dq2*(qOE-qOW) + df4*dq2*fOE - df2*dq4*fOW + dg4*dq2*gOE - dg2*dq4*gOW;

% Precompute elements of inv(AII) = 1/(a*d-b*c)*[d,-b;-c,a]
a11 = df1*dq3-df3*dq1;    a12 = dg1*dq3-dg3*dq1;
a21 = df4*dq2-df2*dq4;    a22 = dg4*dq2-dg2*dq4;

% Numerical computation 
%A = [a11,a12;a21,a22]; C=[c1,c2]'; X=inv(A)*c;

% Explicit computation of the strongly Interacting state fluxes: f** and g**
foo=( a22*c1-a12*c2)/(a11*a22-a12*a21);
goo=(-a21*c1+a11*c2)/(a11*a22-a12*a21); % verified!

% Report f** and g**
disp('MthII : q**, f**, g**');
disp([nan(4,1),foo,goo]);

%% Balsara's HLLE 2-d

% 1-d HLLE solutions
i=1; j=1;
qL=q0(i  ,j,:); qR=q0( i ,j+1,:); fS=HLLE1Dflux(squeeze(qL),squeeze(qR),[1,0]);
qL=q0(i+1,j,:); qR=q0(i+1,j+1,:); fN=HLLE1Dflux(squeeze(qL),squeeze(qR),[1,0]);
qL=q0(i  ,j,:); qR=q0(i+1, j ,:); gW=HLLE1Dflux(squeeze(qL),squeeze(qR),[0,1]);
qL=q0(i,j+1,:); qR=q0(i+1,j+1,:); gE=HLLE1Dflux(squeeze(qL),squeeze(qR),[0,1]);

% Display HLLE approximations for verification
%disp('1-d HLLE individual solutions: fN - fS - gW - gE');
%disp([fN,fS,gW,gE]); % verified!

% Vector of coeficients
d1= sS*sW*qSW-sS*sE*qSE-sN*sW*qNW+sN*sE*qNE...
    + sS*(fSE-fSW)-sN*(fNE-fNW)+sE*(gSE-gNE)-sW*(gSW-gNW)...
    +0.5*(sWS*(fSW-fOW)-sES*(fSE-fOE)-sWN*(fNW-fOW)+sEN*(fNE-fOE))...
    +0.5*(sSW*(gSW-gSO)-sNE*(gSE-gSO)-sNW*(gNW-gNO)+sNE*(gNE-gNO));
d2= -sS*sE*qSE + sN*sE*qNE + 0.5*(sSWplus*gSW-sNWplus*gNW+sN*fN-sS*fS)...
    +0.5*((2*sE-sSEplus)*gSE-(2*sE-sNEplus)*gNE-(sNEplus-sNWplus)*gNO+(sSEplus-sSWplus)*gSO)...
    -0.5*((-2*sS+sES)*fSE+(2*sN-sEN)*fNE+(sEN-sES)*fOE);
d3= -sN*sW*qNW + sN*sE*qNE + 0.5*(sWSplus*fSW-sESplus*fSE+sE*gE-sW*gW)...
    +0.5*((2*sN-sWNplus)*fNW-(2*sN-sENplus)*fNE-(sENplus-sESplus)*fOE+(sWNplus-sWSplus)*fOW)...
    -0.5*((-2*sW+sNW)*gNW+(2*sE-sNE)*gNE+(sNE-sNW)*gNO);
d = [d1,d2,d3]';

% Define square region R'
Rprime = (sE-sW)*(sN-sS)*dt^2;
C = [(sE-sW)*(sN-sS),0,0; sE*(sN-sS),-0.5*(sN-sS),0; sN*(sE-sW),0,-0.5*(sE-sW)];
Cinv = 1/((sE-sW)*(sN-sS))*[1,0,0;2*sE,-2*(sE-sW),0;2*sN,0,-2*(sN-sS)]; % verified

% Solve 
X = Cinv*d;
disp('Balsara HLLE-2d : q**, f**, g**');
disp(X');


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%
% The HLLE-1d (for testing)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function HLLE = HLLE1Dflux(qL,qR,normal)
    % Compute HLLE flux
    global gamma

    % normal vectors
    nx = normal(1);
    ny = normal(2);
       
    % Left state
    rL = qL(1);
    uL = qL(2)/rL;
    vL = qL(3)/rL;
    vnL = uL*nx+vL*ny;
    pL = (gamma-1)*( qL(4) - rL*(uL^2+vL^2)/2 );
    aL = sqrt(gamma*pL/rL);
    HL = ( qL(4) + pL ) / rL;
    
    % Right state
    rR = qR(1);
    uR = qR(2)/rR;
    vR = qR(3)/rR;
    vnR = uR*nx+vR*ny;
    pR = (gamma-1)*( qR(4) - rR*(uR^2+vR^2)/2 );
    aR = sqrt(gamma*pR/rR);
    HR = ( qR(4) + pR ) / rR;
    
    % First compute the Roe Averages
    RT = sqrt(rR/rL); % r = RT*rL;
    u = (uL+RT*uR)/(1+RT);
    v = (vL+RT*vR)/(1+RT);
    H = (HL+RT*HR)/(1+RT);
    a = sqrt( (gamma-1)*(H-(u^2+v^2)/2) );
    vn = u*nx+v*ny;
    
    % Wave speed estimates
    SLm = min([ vnL-aL, vn-a, 0]);
    SRp = max([ vnR+aR, vn+a, 0]);
    
    % Left and Right fluxes
    FL=[rL*vnL; rL*vnL*uL + pL*nx; rL*vnL*vL + pL*ny; rL*vnL*HL];
    FR=[rR*vnR; rR*vnR*uR + pR*nx; rR*vnR*vR + pR*ny; rR*vnR*HR];
    
    % Compute the HLL flux.
    HLLE = ( SRp*FL - SLm*FR + SLm*SRp*(qR-qL) )/(SRp-SLm);
end
