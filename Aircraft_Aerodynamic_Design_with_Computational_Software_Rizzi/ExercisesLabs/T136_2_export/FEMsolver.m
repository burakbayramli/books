function [ output ] = FEMsolver( n,geo,lattice,state,ref,structure,mesh,results,wingno)
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here
N=sum(n(wingno,:));
Stiff =mesh.stiffness;                                                        %Total number of FEM elements
Stiff2=mesh.stiffness_clamped;
T=mesh.stiffness_rotation_matrix;
x=cumsum(mesh.element_length);
%JO1701
if geo.symetric(wingno)
    x = [-fliplr(x),x];
end
moment_axis=sum(structure.spars,2)/2;                                       %Take the moment between the spars
results=fStripforce5(geo,results,lattice,state,ref,moment_axis(wingno));    %Computing forces on each strip.

aero   =fReArrAero(N,geo,results,wingno);    %Maps the lattice aeroloads onto the FEM mesh.

% Load2 in global coord
Load2  =fLoadings2(N,geo,results,wingno,mesh,aero);
% figure(25)
% sig = -1;
% for kk = 1:6
%     subplot(3,2,kk)
%     ss = Load2(kk:6:end);
%     nn = length(ss);
%     plot(ss(1:nn/2),'.k')
%     hold on
%     sig = -1*sig;
%     plot(sig*ss(nn/2+1:end),'r')
% end
% Solving
deflections=Stiff2\Load2;
% figure(26)
% for kk = 1:6
%     subplot(3,2,kk)
%     ss = deflections(kk:6:end);
%     nn = length(ss);
%     plot(ss(1:nn/2),'.k')
%     hold on
%     sig = 1;
%     if kk==2 || kk == 4 || kk == 6
%         sig = -1;
%     end
%     plot(sig*ss(nn/2+1:end),'r')
% end

% deflections in global coord.

% Computing clamp load
deflections3=[0 0 0 0 0 0 deflections']';

% Loads3 in global coords
Loads3=Stiff*deflections3;
Clamp_load_global=Loads3(1:6);

% Loads4 in local system
%JO1701 - add symm flag
Loads4=fRload(T,Loads3,geo.symetric(wingno))'  ;

%Shear force and bending moment computations
beampts = mesh.GP_SB;
if geo.symetric(wingno)
    beampts = [beampts;mesh.GP_P(2:end,:)];
end
[Fi,Mi,translations]=beaminternal2(Loads4,beampts,deflections3);
%-----v Få den att funka 2015 05 11
%Checking stress levels, in beam system

[Stress Total MaxF]=stress(Fi,Mi,mesh.profile);

output.Mass     =sum(mesh.profile.mass);
output.Fuel_Vol =sum(mesh.profile.Vol(1:(structure.fueled_span*N)));
output.Fuel_Mass=output.Fuel_Vol*807.5;  %JET A-1
output.Fi     =Fi;
output.Mi     =Mi;
output.Total  =Total;
output.MaxF   =MaxF;
output.def    =deflections;
output.trans  =translations;
output.x      =x;
output.R_el   =mesh.R_el;
output.profile=mesh.profile;
output.Loads  =Loads3;

end


function[Load]=fLoadings2(n,geo,results,wingno,mesh,aero)
g=9.82;
rho_fuel=807.5;  %JET A-1

mass_on  =1;
engine_on=0;
fuel_on  =0;
gear_on  =0;
aero_on  =1;

profile=mesh.profile;

%Removing clamped rows rom RHS
Load=zeros(6*(n+1),1);  %initializing
%JO1701
if geo.symetric(wingno)
    Load=zeros(6*(2*n+1),1);
end
Load=Load(7:end);      %removing clampes node

% Computing indices
[a b]=size(Load);
n=a/6;              %Number of nodes
dx_index=1:6:(a-5);
dy_index=2:6:(a-4);
dz_index=3:6:(a-3);
dl_index=4:6:(a-2);
dm_index=5:6:(a-1);
dn_index=6:6:(a-0);
%
% Point load
%Load(dx_index(end))=Load(dx_index(end))-10000;
%Load(dz_index(end))=-1000;
%Load(dz_index(5))=0000;
%Load(dx_index(1:end))=1000/n;

% %%%%% Eigenmass load %%%%%%
if mass_on
    massload =-profile.mass.*g;
    if geo.symetric(wingno)
        massload = [massload,fliplr(massload)];
    end;
    Load(dz_index)=massload;        %Massforces in the AC z direction.
end

% %%%%% Fuel Load %%%%%
%warning off
%disp('floadings R38, warn off')
if fuel_on
    [a b]=size(profile.Vol');
    being_tank=zeros(a,b);
    being_tank(1:fspan(wingno)*a)=1;
    fuelload=-profile.Vol'.*being_tank.*rho_fuel.*g;
    Load(dz_index)=Load(dz_index)+fuelload;           %Massforces in the AC z direction.
end

% %%%%% Gear ground contact load %%%%%
if gear_on
    node=fix(gear.pos(2)/(span*cos(sweep)*cos(dihed))*n); %Node to hang the engine on
    node_pos=span*node/n* [sin(sweep)*cos(dihed)
        cos(sweep)*cos(dihed)
        sin(dihed)];
    % FORCES
    Load((node-1)*6+1)=Load((node-1)*6+1)...
        +gear.force(1);
    
    Load((node-1)*6+2)=Load((node-1)*6+2)...
        +gear.force(2);
    
    Load((node-1)*6+3)=Load((node-1)*6+3)...
        +gear.force(3);
    
    % Moments
    gear.pos(3)=gear.pos(3)+gear.length;
    h=gear.pos-node_pos';
    mom=cross(gear.force,h);
    % %mg_mom=cross(engine.mass(i)*g*[0 0 -1],h);
    % %mom=T_mom+mg_mom;
    
    Load((node-1)*6+4)=Load((node-1)*6+4)+mom(1);
    Load((node-1)*6+5)=Load((node-1)*6+5)+mom(2);
    Load((node-1)*6+6)=Load((node-1)*6+6)+mom(3);
    
end

if aero_on
    Load(dx_index)=Load(dx_index)+aero.F(:,1);
    Load(dy_index)=Load(dy_index)+aero.F(:,2);
    Load(dz_index)=Load(dz_index)+aero.F(:,3);
    
    Load(dl_index)=Load(dl_index)+aero.M(:,1);
    Load(dm_index)=Load(dm_index)+aero.M(:,2);
    Load(dn_index)=Load(dn_index)+aero.M(:,3);
end

% %%%%% Engine load %%%%%%
if engine_on
    if wingno>1 %engines on first wing only
        return
    end
    
    if engine.number==0
        %No engines on main wing
        return
    end
    
    [ne void]=size(engine.cgpos);
    ce       =sum(engine.cgpos(:,2)<0);
    %ne=ne-ce;% REmoving symmetric engines
    for ii=1:ne
        
        node=fix(engine.cgpos(ii,2)/(span*cos(sweep)*cos(dihed))*n); %Node to hang the engine on
        node_pos=span*node/n* [sin(sweep)*cos(dihed)
            cos(sweep)*cos(dihed)
            sin(dihed)];
        
        Load((node-1)*6+1)=Load((node-1)*6+1)...
            +engine.thrust(ii)*engine.throttle(ii)*engine.thrustv(ii,1);
        
        Load((node-1)*6+2)=Load((node-1)*6+2)...
            +engine.thrust(ii)*engine.throttle(ii)*engine.thrustv(ii,2);
        
        Load((node-1)*6+3)=Load((node-1)*6+3)...
            -engine.mass*g...
            +engine.thrust(ii)*engine.throttle(ii)*engine.thrustv(ii,3);
        
        % Moments
        h(1)=engine.cgpos(ii,1)-node_pos(1);
        h(3)=engine.cgpos(ii,3)-node_pos(3);
        
        T_mom=cross(engine.thrust(ii)*engine.throttle(ii)*engine.thrustv(ii,:),h);
        mg_mom=cross(engine.mass(ii)*g*[0 0 -1],h);
        mom=T_mom+mg_mom;
        
        Load((node-1)*6+4)=Load((node-1)*6+4)+mom(1);
        Load((node-1)*6+5)=Load((node-1)*6+5)+mom(2);
        Load((node-1)*6+6)=Load((node-1)*6+6)+mom(3);
        
    end
end
end %Function floadings


function[aero]=fReArrAero(n,geo,results,wingno)
N=sum(n);
%results.load.ypstation(:,wingno)
%JO1701
NI=length((results.load.ypstation(:,wingno)));
I = 1:NI;
x=results.load.ypstation(I,wingno);
y=results.load.F(I,wingno,:);
m=results.load.M(I,wingno,:);

xzspan=sum(geo.b(wingno,:));
XI=xzspan/N:xzspan/N:xzspan;
%JO1701
if geo.symetric(wingno)
    XI = [XI,-XI];
end

[a b c]=size(y);
%JO1701
skip  = 0;
if skip
    if geo.symetric(wingno)
        y=y(a/2+1:end,:,:);
        x=x(a/2+1:end);
        m=m(a/2+1:end,:,:);
    end
end
%JO1701
%x = x-(x(1))

YI(:,1)=interp1(x,y(:,1),XI,'spline');
YI(:,2)=interp1(x,y(:,2),XI,'spline');
YI(:,3)=interp1(x,y(:,3),XI,'spline');

MI(:,1)=interp1(x,m(:,1),XI,'spline');
MI(:,2)=interp1(x,m(:,2),XI,'spline');
MI(:,3)=interp1(x,m(:,3),XI,'spline');

aero.ystation=XI;
aero.F=YI*(xzspan/N);
aero.M=MI*(xzspan/N);
end


function[local_load]=fRload(T,Loads,symm)
%JO1701 - add symm
%Rotates the loads to the beam local coordinate system.
[a b]=size(Loads);
a=a/6; %Number of elements (?) nodes
ymir = ones(6,1);
ymir(2:3:end)=-1;
Ymir = diag(ymir);
ah   = (a+1)/2;
if symm
    for i=1:a
        if i==ah
            T2=T(1:6,1:6,i-1);
        else
            if i > ah
                T2=T(1:6,1:6,i-ah)*Ymir;
            else
                T2=T(1:6,1:6,i);
            end
        end
        local_load((6*(i-1)+1):6*i)=T2*Loads((6*(i-1)+1):6*i);
    end
else
    for i=1:a
        if i==a
            T2=T(1:6,1:6,i-1);
        else
            T2=T(1:6,1:6,i);
        end
        local_load((6*(i-1)+1):6*i)=T2*Loads((6*(i-1)+1):6*i);
    end
end

end


function [F2,M2,Ds]=beaminternal2(Loads,GP,def)
% Forces, moments, and deflections in global coordinates
[a b]=size(Loads);
dx_index=1:6:(a-5);
dy_index=2:6:(a-4);
dz_index=3:6:(a-3);
dl_index=4:6:(a-2);
dm_index=5:6:(a-1);
dn_index=6:6:(a-0);

F(:,1)=Loads(dx_index);
F(:,2)=Loads(dy_index);
F(:,3)=Loads(dz_index);

M(:,1)=Loads(dl_index);
M(:,2)=Loads(dm_index);
M(:,3)=Loads(dn_index);
% JO 1701 - comment
%F2=cumsum(F);
F2 = F;

dp=[diff(GP,1)];
dp=[[0 0 0]; dp];
%JO 1701 - comment
%Mf=cross(F2,dp);

[c d]=size(GP);
for i=1:c
    
    P(:,1)=GP(:,1)-GP(i,1);
    P(:,2)=GP(:,2)-GP(i,2);
    P(:,3)=GP(:,3)-GP(i,3);
    
    MFT(i,:)=sum(cross(F(i:end,:),P(i:end,:)));
end

%M2=cumsum(M+Mf);
M2=MFT;

%Deflections in matrix form
Ds(:,1)=def(dx_index);
Ds(:,2)=def(dy_index);
Ds(:,3)=def(dz_index);
end


function [S,total,maxforce]=stress(F,M,profile)
%JO1701
A  = profile.A;
Wp = profile.Wp;
Wy = profile.Wy;
Wz = profile.Wz;
nn = size(F,1);
np = size(A,2);
if nn == 2*np+1
    A  = [A ,fliplr(A)];
    Wp = [Wp,fliplr(Wp)];
    Wy = [Wy,fliplr(Wy)];
    Wz = [Wz,fliplr(Wz)];
% else
%     error('nn not 2np+1')
end

S(:,1)=F(1:end-1,1)./A';       %Normal x
S(:,2)=F(1:end-1,2)./A';       %Shear y
S(:,3)=F(1:end-1,3)./A';       %Shear z

S(:,4)=M(1:end-1,1)./Wp';     %Rotation shear xx
S(:,5)=M(1:end-1,2)./Wy';     %Bending yy
S(:,6)=M(1:end-1,3)./Wz';     %Bending zz

shear=sqrt(S(:,2).^2+S(:,3).^2+S(:,4).^2);
normal=abs(S(:,1))+abs(S(:,5))+abs(S(:,6));

total=sqrt(normal.^2+3*shear.^2);
maxforce=total./A';

end