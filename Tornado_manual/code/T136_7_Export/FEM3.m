function[output]=wingfem(geo,results,structure,wingno)
%%
%This function models wing number WINGNO as a stick beam and computes the
%deflections based on aeroloads from RESULTS and structural data from
%STRUCTURE.


a=geo.nelem(wingno);
total_weight_factor=1;

%% Constants
n=structure.nx;                                %Number of FEM elements per partition.
spars=structure.spars;                         %Spar position

%Looping over number of partitions:
[a b]=size(geo.b);


W=[];
H=[];
C=[];
THK=[];
T=[];
LENGTH=[];
GP=[0 0 0];
j=0;
N=sum(n);


stick_spar_pos=((structure.spars(wingno,1)+structure.spars(wingno,2))/2).*geo.c(wingno);
globalbeamoffset=[geo.startx(wingno)+(stick_spar_pos), geo.starty(wingno),  geo.startz(wingno) ];
geo.vCfraction(wingno)=(structure.spars(wingno,1)+structure.spars(wingno,2))/2;


part_c(1)=geo.c(wingno);
for i=2:b
    part_c(i)=part_c(i-1)*geo.T(wingno,i-1);
    
end


for i=1:b
  [w h chords]=fgeotransform2(n(i),geo,wingno,i,spars);  
  
  j=j+1;
  W=[W w];
  H=[H h];
  C=[C chords];
    

%boxstructure h,w & c distribution with airfoils and taper as in the
%geometry structure 
    
%% Data to transform local beam coordinate system into global system

Tt=geo.T(wingno,i);                                 %Partition taper
Cc=part_c(i);                                       %Partition root chord
Ss=geo.SW(wingno,i);                                %Partition sweep
bb=geo.b(wingno,i);                                 %Partition span

LES=atan((0.25*Cc*(1-Tt)+bb*tan(Ss))/bb);

JJ=-geo.vCfraction(wingno)*Cc*(1-Tt)+bb*tan(LES);   %Help variable
Sweep=atan(JJ/bb);                                  %Beam sweep

Length=sum(geo.b(wingno,i)./(cos(Sweep)));          %Partition beam length.
Dihedral=geo.dihed(wingno,i);
lx=Length/n(i):Length/n(i):Length;                  %Beam node local x coordinates
ly=zeros(size(lx));                                 %Beam node local y coordinates
lz=zeros(size(lx));                                 %Beam node local z coordinates
    
lp=[lx' ly' lz'];                                %local beam cooradinates
    
Roll(j)=Dihedral;                                %Euler angles to rotate local beam in global coordinates
Yaw(j)=+pi/2-Sweep;                              %Euler angles to rotate local beam in global coordinates
Pitch(j)=0;                                      %Euler angles to rotate local beam in global coordinates 

%%
L=Length/n(i);                                   %Length of each beam element
x=L:L:Length;                                    %Node distribution

[E,G,rho,R_el]=material_data(structure.material);          %Fetching material data
K=zeros(12+(n(i)-1)*6,12+(n(i)-1)*6,n(i));       %Initializing stiffnes matrix         
t(:,:)=fRmat(Roll(j),Pitch(j),Yaw(j));           %Straight beam rotation matrix

nt=t(1:3,1:3);  %Node rotation
for i2=1:n(j)
    t2(:,:,i2)=t;
end
T= cat(3,T,t2);


for i3=1:n(i)
   gp(i3,:)=(nt'*lp(i3,:)');
end


thk=ones(1,n(i)).*structure.skin_thick(wingno,i);
THK=[THK thk];
L2=ones(1,n(j)).*L;
LENGTH=[LENGTH L2];


gp(:,1)=gp(:,1)+GP(end,1);
gp(:,2)=gp(:,2)+GP(end,2);
gp(:,3)=gp(:,3)+GP(end,3);

GP=[GP;gp];                     %Global node coordinates
clear gp

end





N=sum(n);
%% Assemble stiffnes matrix
for k=1:N  
    i=(k*6-5):(k*6+6);  
    
    %Computing the profile properties 
    [Iy,Iz,Ip,A,Ai,Wy,Wz,Wp]=beamprofile('box',H(k),W(k),THK(k));
    
    profile.Iy(k)=Iy;
    profile.Iz(k)=Iz;
    profile.Ip(k)=Ip;
    profile.A(k)=A;
    profile.Wy(k)=Wy;
    profile.Wz(k)=Wz;
    profile.Wp(k)=Wp;
    profile.Vol(k)=Ai*L;
    profile.Ai(k)=Ai;
    
    profile.mass(k)=A*L*rho*total_weight_factor;       %3.33 to allow for primary and secondary structure
 
    K1=elem_K_mat(A,E,G,Iy,Iz,Ip,L);    %Local stiffnes
    K2=T(:,:,k)'*K1*T(:,:,k);           %Local stiffnes in global coords.   
    K(i,i,k)= K2;   
end
profile.t=THK;
profile.w=W;
profile.h=H;


%figure(10)
%hold on
%plot(profile.Ip)
Stiff=sum(K,3); %Collapsing stiffnes matrix.

%Clamp node 1, removing rows 1->6
Stiff2=Stiff(7:end,7:end);




%Rearranging aeroloads to fit wing
aero=fReArrAero(N,geo,results,wingno,GP);

Load2=fLoadings2(N,geo,results,wingno,profile,aero);

%% Solving
deflections=Stiff2\Load2;

%% Computing clamp load
deflections3=[0 0 0 0 0 0 deflections']';
Loads3=Stiff*deflections3;
Clamp_load_global=Loads3(1:6);

%% REM NEW VERSION%% Loads4=fRload(T,Loads3)';   
%Shear force and bending moment computations
x=cumsum(LENGTH);
[Fg,Mg,translations]=beaminternal2(Loads3,GP,deflections3);

[Fi, Mi]=glo2loc(Fg,Mg,T);

%Checking stress levels, in beam system
[Stress Total MaxF]=stress(Fi,Mi,profile);

%disp(strcat('MAx stress',num2str(max(Total))))


if wingno==1
    %Computing the spanwise stations for anti-buckling ribs.
    %buckling_rib_eta=buckling(profile,structure,Mi,Fi,Length,x);
    %Computing mass of the anti-buckling ribs.
    %rib_mass=fRibmass(geo,buckling_rib_eta,wingno,profile,structure);
end


         output.GP(:,1)=GP(:,1)+globalbeamoffset(:,1);
         output.GP(:,2)=GP(:,2)+globalbeamoffset(:,2);
         output.GP(:,3)=GP(:,3)+globalbeamoffset(:,3);


output.Mass=sum(profile.mass);
%disp('FEM R107, warn off')
output.Fuel_Vol=sum(profile.Vol);
output.Fuel_Mass=output.Fuel_Vol*807.5;  %JET A-1
output.Fi=Fi;
output.Mi=Mi;
output.Total=Total;
output.MaxF=MaxF;
output.def=deflections;
output.translations=translations;
%output.GP=GP;
output.x=x;
output.R_el=R_el;
output.profile=profile;
output.Loads=Loads3;
FEMresultplot2(output)

end %Function FEM



function[aero]=fReArrAero(n,geo,results,wingno,GP)
%This function resamples the aeroloads onto the stick beam nodes.   
   
   I=find(results.ystation(:,wingno));

   x=results.ystation(I,wingno);
   y=results.aeroload.F(I,wingno,:);
   m=results.aeroload.M(I,wingno,:);
    
   xzspan=sum(geo.b(wingno,:));
     % XI=xzspan/n:xzspan/n:xzspan;                 %<------   NEEDS
     % FIX....ok
   XI=sqrt( GP(2:end,2,:).^2+GP(2:end,3,:).^2);
   
   
   X2=sqrt( GP(:,2,:).^2+GP(:,3,:).^2);
   panelspan=diff(X2);
   
   
   [a b c]=size(y);
   if geo.symetric(wingno)
      y=y(a/2+1:end,:,:);
      x=x(a/2+1:end);
      m=m(a/2+1:end,:,:);
   end
   
   x=x-(x(1));
   

   %Check for symmetry
   YI(:,1)=interp1(x,y(:,1),XI,'spline');
   YI(:,2)=interp1(x,y(:,2),XI,'spline');
   YI(:,3)=interp1(x,y(:,3),XI,'spline');
   
   MI(:,1)=interp1(x,m(:,1),XI,'spline');
   MI(:,2)=interp1(x,m(:,2),XI,'spline');
   MI(:,3)=interp1(x,m(:,3),XI,'spline');
   
   aero.ystation=XI;
   aero.F(:,1)=YI(:,1).*(panelspan);
   aero.F(:,2)=YI(:,2).*(panelspan);
   aero.F(:,3)=YI(:,3).*(panelspan);
   
   
   aero.M(:,1)=MI(:,1).*(panelspan);
   aero.M(:,2)=MI(:,2).*(panelspan);
   aero.M(:,3)=MI(:,3).*(panelspan);
   
end %rearr aero







function[w,h,c]=fgeotransform2(n,geo,wingno,partition,spars)

[a b]=size(geo.b);

span=(geo.b(wingno,partition));
chords=geo.c(wingno)*[1 cumprod((geo.T(wingno,:)))];
Taper=(geo.T(wingno,partition));
[A B]=fGetProfThick(geo.foil(wingno,partition,:),spars);
hi=((A(1)+A(2))./2); %Avg thickness btw f and r spar  - height inner
hu=((B(1)+B(2))./2); %to give rectangular box         - heigth outer


hi=[hi hu(end)];
BI=span/n:span/n:span;
HI=interp1([0 span],hi,BI);

ti=[1 Taper];
TI=interp1([0 span],ti,BI);

c=chords(partition)*TI;         
h=HI.*c;
w=((spars(wingno,2)-spars(wingno,1)).*c')';


end





function [out1,out2]=fGetProfThick(foils,sparpos)
%  Input: 2 airfoils per partition: inboard & outboard
%  INPUT: foils = {'name1', 'name2'};
%  Input: spar location where thickness of airfoil is required (%chord)
%  Output: t=thikness of airfoil at sparloc for airfoils (%chord)
%  aenmu

for k = 1:2
    foil=(foils(1,1,k));

    if isempty(str2num((cell2mat(foil))))==0
        TYPE=1;       %Naca xxxx profile, see case 1
    elseif isempty(str2num((cell2mat(foil))))
        TYPE=2;       %Airfoil from file, see case 2
    end

    switch TYPE

        case 1

            foil  = str2num(cell2mat(foil));
            m     = fix(foil/1000);	%gives first NACA-4 number      -> max camber
            lemma = foil-m*1000;
            p     = fix(lemma/100);	%gives second NACA-4 number     -> pos of max camber
            lemma = (foil-m*1000)-p*100;
            tk    = lemma/100;     %                                -> max thikness      
        
            for i = 1:max(size(sparpos))
                x = sparpos(i);
                Yt = 5*tk*(0.2969*x^0.5 - 0.126*x - 0.3516*x^2 + 0.2843*x^3 - 0.1015*x^4);
                %if sparpos(i) <= p
                %    Yc=m*(1/p^2)*(2*p*x - x ^2);
                %    tanteta = m*(1/p^2)*(2*p - 2*x);                
                %else
                %    Yc=m*(1/(1-p)^2)*(1-2*p+2*p*x - x^2);
                %    tanteta=m*(1/(1-p)^2)*(2*p - 2*x);
                %end
                %Yup  = Yc + Yt*cos(atan(tanteta));
                %Ylow = Yc - Yt*cos(atan(tanteta));
                %t(i) = Yup - Ylow;
                t(i)=2*Yt;
                
            end

          
        case 2
    
            %The airfoil is descriped as a coordinate file for upper and lower surfaces

            cd aircraft
            cd airfoil
                A=load(char(foil));
            cd ..
            cd ..


            % Take the number of data points in the data file
            L=A(1,1);
        
            %Upper surface
            Xu = A(2:L+1,1)/A(L+1,1); %% It is divided by A(L+1,1), which is the max absciss of the aifoil, in order to normalize the airfoil to a chord c=1
            Yu = A(2:L+1,2)/A(L+1,1);

            % Lower surface
            Xl = A(L+2:end,1)/A(L+1,1);
            Yl = A(L+2:end,2)/A(L+1,1);

            for i = 1:max(size(sparpos))
                t(i) = interp1(Xu,Yu, sparpos(i)) - interp1(Xl,Yl, sparpos(i));
            end
            
    end%switch
    
    if k==1
        out1 = t;  %inboard airfoil: spar thicknesses
    else
        out2 = t;  %outboard airfoil: spar thicknesses
    end
end;    %end of 'k' for loop


end%function




function [E,G,rho,R_el]=material_data(type)
%This funktion sets the material data according to the input varialble
%'type', which is a string. The function looks up in the table below

switch type
    case 'SS2511-03'
        %Stainless steel, anealed.
        E=210  *10^9;     %Pa      Elasticity modulus   
        G=80    *10^9;     %Pa      Shear modulus
        rho=7800;         %kg/m^3  Density
        R_el=440*10^6;    %Pa      Elasticity limit
    
    case 'titanium'
        %Titanium 6AL-4V
        E=114 *10^9;     %Pa      Elasticity modulus   
        G=44  *10^9;     %Pa      Shear modulus
        rho=4540;        %kg/m^3  Density
        R_el=1070*10^6;  %Pa      Elasticity limit
   
    case 'AA7050'
        %Aluminium, aerospace.
        E=72 *10^9;      %Pa      Elasticity modulus   
        G=28 *10^9;      %Pa      Shear modulus
        rho=2700;        %kg/m^3  Density
        R_el=455*10^6;    %Pa      Elasticity limit
        
        
    case 'AA2024'
        %Aluminium, aerospace.
        E=72 *10^9;      %Pa      Elasticity modulus   
        G=28 *10^9;      %Pa      Shear modulus
        rho=2780;        %kg/m^3  Density
        R_el=389*10^6;    %Pa      Elasticity limit
              
    case 'Fir'
        %Wood, best direction
        E=9*   10^9;     %Pa      Elasticity modulus   
        G=0.61*10^9;     %Pa      Shear modulus
        rho=520;         %kg/m^3  Density
        R_el=46*10^6;    %Pa      Elasticity limit
        
    case 'Lockalloy'
        %Aerospace grade beryllium/aluminium alloy "LOCKALLOY". 60%Be,40%Al
        E=      202*   10^9; %Pa      Elasticity modulus   
        G=      87*    10^9; %Pa      Shear modulus
        rho=    2100;        %kg/m^3  Density
        R_el=   276*   10^6; %Pa      Elasticity limit
        
    case 'CFRP'
        % Isotropic simplificatrion.
        % 30% Epoxy, 70% Carbon fibre
        
        E=      127*   10^9; %Pa      Elasticity modulus   
        G=      60*    10^9; %Pa      Shear modulus
        rho=    1600       ;          %kg/m^3  Density
        R_el=   1000*   10^6; %Pa      Elasticity limit
    
    
    case 'CarbonNT'
        %Carbon nanotubes
        E=      1000*   10^9; %Pa      Elasticity modulus   
        G=      500*    10^9; %Pa      Shear modulus
        rho=    1400;         %kg/m^3  Density
        R_el=   13*     10^9; %Pa      Elasticity limit
        
        
        
end
end %Materials



function [Iy,Iz,Ip,A,Ainternal,Wy,Wz,Wp]=beamprofile(type,height,width,thickness)
%Profile computes the properties of a specified beam profile

h=height;
b=width;
t=thickness;

switch type
    case ('box')
        %Rectangular box with equal thickness skin
        
        Iy=(h*b^3-((h-2*t)*(b-2*t)^3))/12;
        Iz=(b*h^3-((b-2*t)*(h-2*t)^3))/12;
        
        Ai=(b-t)*(h-t);
        Ip=4*Ai^2/(2*(h-t)/t+2*(b-t)/t);
        
        A=b*h-(b-2*t)*(h-2*t);          %Used to compute structural weight
        
        Ainternal=(b-2*t)*(h-2*t);      %Used to compute fuel volume.
        
        Wy=Iy/(b/2);
        Wz=Iz/(h/2);
        Wp=2*Ai*t;
        
end

end %FUNCTION



function[Load]=fLoadings2(n,geo,results,wingno,profile,aero)
g=9.82;
rho_fuel=807.5;  %JET A-1

mass_on=1;
engine_on=0;
fuel_on=0;
gear_on=0;
aero_on=1;



%Removing clamped rows rom RHS
Load=zeros(6*(n+1),1);  %initializing
Load=Load(7:end);      %removing clampes node

%% Computing indices
[a b]=size(Load);
n=a/6;              %Number of nodes
dx_index=1:6:(a-5);
dy_index=2:6:(a-4);
dz_index=3:6:(a-3);
dl_index=4:6:(a-2);
dm_index=5:6:(a-1);
dn_index=6:6:(a-0);
%%
%% Point load
%Load(dx_index(end))=Load(dx_index(end))-10000;
%Load(dz_index(end))=-1000;
%Load(dz_index(5))=0000;
%Load(dx_index(1:end))=1000/n;

%% %%%%% Eigenmass load %%%%%%
if mass_on
    massload=-profile.mass.*g;            
    Load(dz_index)=massload;        %Massforces in the AC z direction.
end

%% %%%%% Fuel Load %%%%%
%warning off
%disp('floadings R38, warn off')
if fuel_on
    [a b]=size(profile.Vol');
    being_tank=zeros(a,b);
    being_tank(1:fspan(wingno)*a)=1;
    fuelload=-profile.Vol'.*being_tank.*rho_fuel.*g; 
    Load(dz_index)=Load(dz_index)+fuelload;           %Massforces in the AC z direction.
end

%% %%%%% Gear ground contact load %%%%%
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
    
    %% Moments
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

%% %%%%% Engine load %%%%%%


if engine_on
    
    if wingno>1 %engines on first wing only
    return
end

if engine.number==0
   %No enbgines on main wing
    return
end
    
    
  [ne void]=size(engine.cgpos);
  ce=sum(engine.cgpos(:,2)<0);
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
    
    %% Moments
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




function [F2,M2,Ds]=beaminternal2(Loads,GP,def)

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

F2=cumsum(F);



dp=[diff(GP,1)];
dp=[[0 0 0]; dp];

Mf=cross(F2,dp);

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

end %Function





function[Fl,Ml]=glo2loc(Fg,Mg,T)
%Transforms global forces to local coordintes

[a b]=size(Fg);

for i=1:a-1
    Fl(i,:)=T(1:3,1:3,i)*Fg(i,:)';
    Ml(i,:)=T(1:3,1:3,i)*Mg(i,:)';
end
end %Function


function [S,total,maxforce]=stress(F,M,profile)


S(:,1)=F(:,1)./[profile.A]';       %Normal x
S(:,2)=F(:,2)./[profile.A]';       %Shear y
S(:,3)=F(:,3)./[profile.A]';       %Shear z

S(:,4)=M(:,1)./[profile.Wp]';     %Rotation shear xx
S(:,5)=M(:,2)./[profile.Wy]';     %Bending yy
S(:,6)=M(:,3)./[profile.Wz]';     %Bending zz


shear=sqrt(S(:,2).^2+S(:,3).^2+S(:,4).^2);
normal=abs(S(:,1))+abs(S(:,5))+abs(S(:,6));

total=sqrt(normal.^2+3*shear.^2);
maxforce=total./[profile.A]';
end %Function





function[T]=fRmat(roll,pitch,yaw)
a=roll;
b=pitch;
c=yaw;

L1=[1       0       0
    0   cos(a)  sin(a)
    0   -sin(a) cos(a)];

L2=[cos(b) 0 -sin(b)
    0      1    0
    sin(b)  0   cos(b)];

L3=[cos(c) sin(c)   0
    -sin(c) cos(c)  0
    0           0   1];

L=L3*L2*L1;

Z=zeros(3);

T=[L Z Z Z
   Z L Z Z
   Z Z L Z
   Z Z Z L]; %Stiffness rotation matrix
end






function[K]=elem_K_mat(A,E,G,Iy,Iz,Ip,L)
K(1,1)=E*A/L;
K(2,1)=0;
K(3,1)=0;
K(4,1)=0;
K(5,1)=0;
K(6,1)=0;
K(7,1)=-E*A/L;
K(8,1)=0;
K(9,1)=0;
K(10,1)=0;
K(11,1)=0;
K(12,1)=0;

K(1,2)=0;
K(2,2)=12*E*Iz/L^3;
K(3,2)=0;
K(4,2)=0;
K(5,2)=0;
K(6,2)=6*E*Iz/L^2;
K(7,2)=0;
K(8,2)=-12*E*Iz/L^3;
K(9,2)=0;
K(10,2)=0;
K(11,2)=0;
K(12,2)=6*E*Iz/L^2;

K(1,3)=0;
K(2,3)=0;
K(3,3)=12*E*Iy/L^3;
K(4,3)=0;
K(5,3)=-6*E*Iy/L^2;
K(6,3)=0;
K(7,3)=0;
K(8,3)=0;
K(9,3)=-12*E*Iy/L^3;
K(10,3)=0;
K(11,3)=-6*E*Iy/L^2;
K(12,3)=0;

K(1,4)=0;
K(2,4)=0;
K(3,4)=0;
K(4,4)=G*Ip/L;
K(5,4)=0;
K(6,4)=0;
K(7,4)=0;
K(8,4)=0;
K(9,4)=0;
K(10,4)=-G*Ip/L;
K(11,4)=0;
K(12,4)=0;

K(1,5)=0;
K(2,5)=0;
K(3,5)=-6*E*Iy/L^2;
K(4,5)=0;
K(5,5)=4*E*Iy/L;
K(6,5)=0;
K(7,5)=0;
K(8,5)=0;
K(9,5)=6*E*Iy/L^2;
K(10,5)=0;
K(11,5)=2*E*Iy/L;
K(12,5)=0;

K(1,6)=0;
K(2,6)=6*E*Iz/L^2;
K(3,6)=0;
K(4,6)=0;
K(5,6)=0;
K(6,6)=4*E*Iz/L;
K(7,6)=0;
K(8,6)=-6*E*Iz/L^2;
K(9,6)=0;
K(10,6)=0;
K(11,6)=0;
K(12,6)=2*E*Iz/L;

K(1,7)=-E*A/L;
K(2,7)=0;
K(3,7)=0;
K(4,7)=0;
K(5,7)=0;
K(6,7)=0;
K(7,7)=E*A/L;
K(8,7)=0;
K(9,7)=0;
K(10,7)=0;
K(11,7)=0;
K(12,7)=0;

K(1,8)=0;
K(2,8)=-12*E*Iz/L^3;
K(3,8)=0;
K(4,8)=0;
K(5,8)=0;
K(6,8)=-6*E*Iz/L^2;
K(7,8)=0;
K(8,8)=12*E*Iz/L^3;
K(9,8)=0;
K(10,8)=0;
K(11,8)=0;
K(12,8)=-6*E*Iz/L^2;

K(1,9)=0;
K(2,9)=0;
K(3,9)=-12*E*Iy/L^3;
K(4,9)=0;
K(5,9)=6*E*Iy/L^2;
K(6,9)=0;
K(7,9)=0;
K(8,9)=0;
K(9,9)=12*E*Iy/L^3;
K(10,9)=0;
K(11,9)=6*E*Iy/L^2;
K(12,9)=0;

K(1,10)=0;
K(2,10)=0;
K(3,10)=0;
K(4,10)=-G*Ip/L;
K(5,10)=0;
K(6,10)=0;
K(7,10)=0;
K(8,10)=0;
K(9,10)=0;
K(10,10)=G*Ip/L;
K(11,10)=0;
K(12,10)=0;

K(1,11)=0;
K(2,11)=0;
K(3,11)=-6*E*Iy/L^2;
K(4,11)=0;
K(5,11)=2*E*Iy/L;
K(6,11)=0;
K(7,11)=0;
K(8,11)=0;
K(9,11)=6*E*Iy/L^2;
K(10,11)=0;
K(11,11)=4*E*Iy/L;
K(12,11)=0;

K(1,12)=0;
K(2,12)=6*E*Iz/L^2;
K(3,12)=0;
K(4,12)=0;
K(5,12)=0;
K(6,12)=2*E*Iz/L;
K(7,12)=0;
K(8,12)=-6*E*Iz/L^2;
K(9,12)=0;
K(10,12)=0;
K(11,12)=0;
K(12,12)=4*E*Iz/L;

end %Function















function[]=FEMresultplot2(input)


Fi=input.Fi;
Mi=input.Mi;
Total=input.Total;
def=input.def;
R_el=input.R_el;
LF=input.Loads;

%% Plotting results
%% Computing indices
[a b]=size(def);
dx_index=1:6:(a-5);
dy_index=2:6:(a-4);
dz_index=3:6:(a-3);
dl_index=4:6:(a-2);
dm_index=5:6:(a-1);
dn_index=6:6:(a-0);



x_pos=[input.x];



figure(100)

subplot(3,2,1);
plot(x_pos,def(dx_index))
ylabel('\Delta x, [m]')
xlabel('Beam station, L, [m]')
title('Deformation in the global coordinate system')

subplot(3,2,3);
plot(x_pos,def(dy_index))
ylabel('\Delta y, [m]')
xlabel('Beam station, L, [m]')

subplot(3,2,5);
plot(x_pos,def(dz_index))
ylabel('\Delta z, [m]')
xlabel('Beam station, L, [m]')

subplot(3,2,2);
plot(x_pos,def(dl_index)*180/pi)
ylabel('\Delta\theta_x, [deg]')
xlabel('Beam station, L, [m]')

subplot(3,2,4);
plot(x_pos,def(dm_index)*180/pi)
ylabel('\Delta\theta_y, [deg]')
xlabel('Beam station, L, [m]')

subplot(3,2,6);
plot(x_pos,def(dn_index)*180/pi)
ylabel('\Delta\theta_z, [deg]')
xlabel('Beam station, L, [m]')



figure(101)
%----

subplot(3,2,1)
plot([x_pos],Fi(:,1))
ylabel('Normal force, Fx, [N]')
xlabel('Beam station, L, [m]')
title('Node forces in the local beam coordinate system')
grid on

subplot(3,2,2)
plot([x_pos],Mi(:,1))
ylabel('Twisting moment, Mx, [Nm]')
xlabel('Beam station, L, [m]')
grid on
%-----

subplot(3,2,3)
plot([x_pos],Fi(:,2))
ylabel('Shear force, Fy, [N]')
xlabel('Beam station, L, [m]')
grid on

subplot(3,2,4)
plot([x_pos],Mi(:,2))
ylabel('Bending moment, My, [Nm]')
xlabel('Beam station, L, [m]')
grid on

%----
subplot(3,2,5)
plot([x_pos],Fi(:,3))
ylabel('Shear force, Fz, [N]')
xlabel('Beam station, L, [m]')
grid on

subplot(3,2,6)
plot([x_pos],Mi(:,3))
ylabel('Bending moment, Mz, [Nm]')
xlabel('Beam station, L, [m]')
grid on

figure(102)
plot([x_pos],Total./10^6)
ylabel('von Mises Stress, \sigma, [MPa]')
xlabel('Beam station, L, [m]')
hold on
plot([0 x_pos(end)],[R_el R_el]./10^6,'r')
plot([0 x_pos(end)],[R_el R_el]./10^6/1.5,'r--')
legend('Beam stress','Yield stress','FAR Safety limit \eta=1.5')





figure(103)
subplot(3,2,1);

plot(x_pos,LF(dx_index))
ylabel('Node forces, F_X, [N]')
xlabel('Beam station, L, [m]')
title('Node forces in the global coordinate system')
subplot(3,2,3);
plot(x_pos,LF(dy_index))
ylabel('Node forces, F_Y, [N]')
xlabel('Beam station, L, [m]')

subplot(3,2,5);
plot(x_pos,LF(dz_index))
ylabel('Node forces, F_Z, [N]')
xlabel('Beam station, L, [m]')

subplot(3,2,2);
plot(x_pos,LF(dl_index)*180/pi)
ylabel('Node moment, M_X, [N]')
xlabel('Beam station, L, [m]')

subplot(3,2,4);
plot(x_pos,LF(dm_index)*180/pi)
ylabel('Node moment, M_Y, [Nn]')
xlabel('Beam station, L, [m]')

subplot(3,2,6);
plot(x_pos,LF(dn_index)*180/pi)
ylabel('Node moment, M_Z, [Nn]')
xlabel('Beam station, L, [m]')

figure(104)
T=input.profile.t*1000;
x=input.x;
plot(x,T)
ylabel('Skin Thickness, t, [mm]')
xlabel('Beam station, L, [m]')
axis([0 x(end) 0 max(T)*1.1])
title('Wing box wall thickness.')

figure(105)
h=input.profile.h*1000;
w=input.profile.w*1000;
x=input.x;
plot(x,h);
hold on
plot(x,w,'r');
ylabel('Dimensions, (h,w), [mm]')
xlabel('Beam station, L, [m]')
axis([0 x(end) 0 max(w)*1.1])
title('Wing box height and width.')
legend('Box height','Box width')

figure(106)
h=input.profile.h*1000;
w=input.profile.w*1000;
x=input.x;
%plot(x,h);
hold on
plot(x,h./w,'r');
ylabel('Thickness, [-]')
xlabel('Beam station, L, [m]')
%axis([0 x(end) 0 max(h./w)*1.1])
title('Wing box tickness ratio.')
%legend('Box height','Box width')

figure(107)
figure(1)
h3=plot3(input.GP(:,1),input.GP(:,2),input.GP(:,3),'-o');
set(h3,'LineWidth',2)

hold on

JJ=input.GP+input.translations;
h2=plot3(JJ(:,1),JJ(:,2),JJ(:,3),'r-d');
set(h2,'LineWidth',2)


end


















%%Rearranging aeroloads to fit wing
%aero=fReArrAero(n,geo,results,wingno);