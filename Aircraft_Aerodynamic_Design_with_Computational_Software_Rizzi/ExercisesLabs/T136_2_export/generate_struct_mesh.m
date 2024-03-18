function [output] = generate_struct_mesh( geo,lattice,structure,wingno)
%UNTITLED4 Summary of this function goes here
%   Detailed explanation goes here

%
%Initialize
W=[];                            %wing box width vector
H=[];                            %Wing box height vector
C=[];                            %Wing chord vector
T  = [];                         % element rotation matrices, starboard
T2 = [];                         % for port wing if symmetric
THK= [];                         %Wing box skin thickness vector
LENGTH=[];                       %Wing FEM element length vector
GP=[0 0 0];
j=0;
% Constants
total_weight_factor=3.33;        %Weight fudge factor allowing for secondary structure.

n=round(geo.b(wingno,:)./sum(geo.b(wingno,:))*structure.nx(wingno));  %Number of FEM elements per partition.
% Length=sum(geo.b(wingno,:)./(cos(geo.SW(wingno,:))));        %Total beam length.
% el_Length=(geo.b(wingno,:)./(cos(geo.SW(wingno,:))))./n      %number of FEMelements per partition.

[a b]=size(geo.b(wingno,:));

spars=structure.spars;                                                                         %Spar position
stick_spar_pos=((structure.spars(wingno,1)+structure.spars(wingno,2))/2).*geo.c(wingno);                 %chord position of stick spar
globalbeamoffset=[geo.startx(wingno)+(stick_spar_pos), geo.starty(wingno),  geo.startz(wingno) ];        %start coordinate of stickspar
geo.vCfraction(wingno)=(structure.spars(wingno,1)+structure.spars(wingno,2))/2;                         %Chord fraction of stick spar.

part_c(1)=geo.c(wingno);
for i=2:b
    part_c(i)=part_c(i-1)*geo.T(wingno,i-1);            %Root chord per partition.
end

for i=1:b                                               %Looping over Partitions to get beam node coordinates.
    [w h chords]=fgeotransform2(n(i),geo,wingno,i,spars);
    
    j=j+1;
    
    C=[C chords];
    %boxstructure h,w & c distribution with airfoils and taper as in the
    %geometry structure
    
    % Data to transform local beam coordinate system into global system
    
    Tt=geo.T(wingno,i);                                 %Partition taper
    Cc=part_c(i);                                       %Partition root chord
    Ss=geo.SW(wingno,i);                                %Partition sweep
    bb=geo.b(wingno,i);                                 %Partition span
    
    LES  =atan((0.25*Cc*(1-Tt)+bb*tan(Ss))/bb);
    JJ   =-geo.vCfraction(wingno)*Cc*(1-Tt)+bb*tan(LES); %Help variable
    Sweep=atan(JJ/bb);                                   %Beam sweep
    % JO 1701 update list of beam W
    W       = [W cos(Sweep)*w];
    Length  = geo.b(wingno,i)/cos(Sweep);   %Partition beam length.
    Dihedral=geo.dihed(wingno,i);
    % JO 1701 update list of beam H
    H = [H h*cos(Dihedral)];
    L = Length/n(i);                        %Length of each beam element
    lx=(1:n(i))*L;                          %Beam node local x coordinates
    ly=zeros(size(lx));                     %Beam node local y coordinates
    lz=zeros(size(lx));                     %Beam node local z coordinates
    
    lp=[lx' ly' lz'];                       %local beam cooradinates
    
    %     Roll(i) =Dihedral;                %Euler angles to rotate local beam in global coordinates
    %     Yaw(i)  =+pi/2-Sweep;             %Euler angles to rotate local beam in global coordinates
    %     Pitch(i)=0;                       %Euler angles to rotate local beam in global coordinates
    %
    t=fRmat(Dihedral,0,+pi/2-Sweep);        %Straight beam rotation matrix
    %Node rotation
    nt = t(1:3,1:3);
    %JO1701 - fix bug
    t2 = zeros(12,12,n(i));
    for i2=1:n(i)
        t2(:,:,i2)=t;
    end
    T = cat(3,T,t2);
    
    if geo.symetric(wingno)
        t=fRmat(-Dihedral,0,-pi/2+Sweep);  %Straight beam rotation matrix
        t2= zeros(12,12,n(i));
        for i2=1:n(i)
            t2(:,:,i2)=t;
        end
        T2 = cat(3,T2,t2);
    end
    
    gp = lp*nt;
    
    L2 = ones(1,n(i))*L;
    LENGTH=[LENGTH L2];
    
    gp(:,1)=gp(:,1)+GP(end,1);
    gp(:,2)=gp(:,2)+GP(end,2);
    gp(:,3)=gp(:,3)+GP(end,3);
    
    GP=[GP;gp];                     %Global node coordinates
    clear gp thk
end
tmp = [-1e-5 cumsum(LENGTH)];
output.beameta_SB = tmp/(tmp(end)-1e-5);
output.stiffness_rotation_matrix=T;
if geo.symetric(wingno)
    output.stiffness_rotation_matrix=T2;
end

%thickness_help_vector
span_station_length=diff(structure.sp(wingno));  %How long is each span station segment for
nooffemelements    =sum(structure.nx(wingno));

if isempty(span_station_length)
    THK=ones(nooffemelements,1).*structure.skin_thick(wingno);
else
    nofsectors            =size(diff(structure.st),2);
    elements_per_sector   =span_station_length*nooffemelements;
    skin_delta_per_sector =diff(structure.st);
    skin_delta_per_element=skin_delta_per_sector./elements_per_sector;
    counter=1;
    thk2(1)=1;
    for i=1:nofsectors
        lemma1=[structure.st(i):(span_station_length(i)/elements_per_sector(i)):structure.st(i+1)]    ;
        for j=1:elements_per_sector(i)
            counter      =counter+1;
            thk2(counter)=thk2(counter-1)+skin_delta_per_element(i);
        end
    end
    
    if size(thk2)>nooffemelements
        THK=thk2(1:end-1).*structure.skin_thick(wingno);
    else
        THK=thk2.*structure.skin_thick(wingno);
    end
end

output.element_length=LENGTH;

GP_SB(:,1)=GP(:,1)+globalbeamoffset(1);
GP_SB(:,2)=GP(:,2)+globalbeamoffset(2);
GP_SB(:,3)=GP(:,3)+globalbeamoffset(3);
output.GP_SB=GP_SB;                         %OUTPUT NODE COORDINATES STARBOARD

if geo.symetric(wingno)
    GP_P(:,1)= GP(:,1)+globalbeamoffset(1);
    GP_P(:,2)=-GP(:,2)-globalbeamoffset(2);
    GP_P(:,3)= GP(:,3)+globalbeamoffset(3);
    output.beameta_P = -output.beameta_SB;
    output.GP_P=GP_P;                       %OUTPUT NODE COORDINATES PORT
end

% Computing Stiffness matrix.
[E,G,rho,R_el]=material_data(structure.material(wingno,:));
N=sum(n);

% Assemble stiffnes matrix
% JO
Stiff = zeros(N*6+6,N*6+6);
if geo.symetric(wingno)
    Stiff = zeros(2*N*6+6,2*N*6+6);
end
%size(Stiff)
% node numbering:
% SB: 1(root) -- N+1 (tip)
%  els      1       2                     N
%  node    1,2     2,3                   N ,N+1
%  dof  1:6,7:12   7:12,13:18           (N-1)*6+1:6N , 6N+1:6N+6
% P : N+2 (root+1) -- 2*N+1
% els     N+1                                N+2                    2*N
% node   1,N+2                             N+2,N+3 ...            2*N,2*N+1
% dof  1:6,(N+1)*6+1:(N+2)*6 (N+1)*6+1:(N+2)*6,(N+2)*6+1:(N+3)*6

for k=1:N % loop over only SB
    i=(k*6-5):(k*6+6);
    if geo.symetric(wingno)
        if k == 1
            ip = [1:6 (6*(N+1)+1):(6*(N+1)+6)];
        else
            ip = i + 6*N;
        end
    end
    
    % profile properties
    [Iy,Iz,Ip,A,Ai,Wy,Wz,Wp]=beamprofile('box',H(k),W(k),THK(k));
    
    profile.Iy(k) =Iy;
    profile.Iz(k) =Iz;
    profile.Ip(k) =Ip;
    profile.A(k)  =A;
    profile.Wy(k) =Wy;
    profile.Wz(k) =Wz;
    profile.Wp(k) =Wp;
    profile.Vol(k)=Ai*LENGTH(k);
    profile.Ai(k) =Ai;
    profile.mass(k)=A*LENGTH(k)*rho*total_weight_factor;       %3.33 to allow for primary and secondary structure
    
    K1=elem_K_mat(A,E,G,Iy,Iz,Ip,LENGTH(k));    %Local stiffnes
    K2=T(:,:,k)'*K1*T(:,:,k);                   %Local stiffnes in global coords.
    %K(i,i,k)= K2;
    %JO1702
    Stiff(i,i) = Stiff(i,i)+K2;
    if geo.symetric(wingno)
        Stiff(ip,ip) = Stiff(ip,ip)+T2(:,:,k)'*K1*T2(:,:,k);
    end
end
profile.t=THK;
profile.w=W;
profile.h=H;

% JO
%Stiff=sum(K,3); %Collapsing stiffnes matrix.

%Clamp node 1, removing rows 1->6
Stiff2=Stiff(7:end,7:end);

output.Mass             =sum(profile.mass);
output.Fuel_Vol         =sum(profile.Vol);
output.Fuel_Mass        =output.Fuel_Vol*807.5;  %JET A-1
output.R_el             =R_el;
output.profile          =profile;
output.stiffness        =Stiff;
output.stiffness_clamped=Stiff2;
end


function[w,h,c]=fgeotransform2(n,geo,wingno,partition,spars)
%This function computes the width and height of the local box beam.
%Edit this function to give other shape than rectangular box

[a b]=size(geo.b);

span  =(geo.b(wingno,partition));
chords=geo.c(wingno)*[1 cumprod((geo.T(wingno,:)))];
Taper =(geo.T(wingno,partition));
[A B] =fGetProfThick(geo.foil(wingno,partition,:),spars)
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
%  Output: t=thickness of airfoil at sparloc for airfoils (%chord)
%  aenmu

for k = 1:2
    foil=(foils(1,1,k));
    TYPE = 1; % naca xxxx or xxxxx?
    if isempty(str2num((cell2mat(foil))))
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
            
            %The airfoil is described as a coordinate file for upper and lower surfaces
            
            cd aircraft
            cd airfoil
            A=load(char(foil));
            cd ..
            cd ..
            % can be of two types: lednicer or uiuc
            if A(1,1) < 2 % uiuc, translate into lednices
                atmp = A;
                npa  = size(atmp,1);
                A    = zeros(npa+2,2);
                xmin = min(atmp(:,1));
                iu   =find(atmp(:,1)==xmin);
                A(2:iu+1,:) = flipud(atmp(1:iu,:));
                A(iu+2:end,:) = atmp(iu:end,:);
                A(1,1)= iu-1; A(1,2) = npa-A(1,1);
                
            end
            % Take the number of data points in the data file
            L=A(1,1);
            
            %Upper surface
            Xu = A(2:L+1,1)/A(L+1,1); %% It is divided by A(L+1,1), which is the max absciss of the aifoil, in order to normalize the airfoil to a chord c=1
            Yu = A(2:L+1,2)/A(L+1,1);
            
            % Lower surface
            Xl = A(L+2:end,1)/A(L+1,1);
            Yl = A(L+2:end,2)/A(L+1,1);
            
            for i = 1:max(size(sparpos))
                t(i) = abs(interp1(Xu,Yu, sparpos(i)) - interp1(Xl,Yl, sparpos(i)));
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
%'type', which is a string.

cd aircraft
cd material
A=load(strcat(type,'.dat'));
E=A(1);G=A(2);rho=A(3);R_el=A(4);
cd ..
cd ..

end

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

function[K]=elem_K_mat(A,E,G,Iy,Iz,Ip,L)
K = zeros(12,12);
K(1,1)=E*A/L;
% K(2,1)=0;
% K(3,1)=0;
% K(4,1)=0;
% K(5,1)=0;
% K(6,1)=0;
K(7,1)=-E*A/L;
% K(8,1)=0;
% K(9,1)=0;
% K(10,1)=0;
% K(11,1)=0;
% K(12,1)=0;

% K(1,2)=0;
K(2,2)=12*E*Iz/L^3;
% K(3,2)=0;
% K(4,2)=0;
% K(5,2)=0;
K(6,2)=6*E*Iz/L^2;
%K(7,2)=0;
K(8,2)=-12*E*Iz/L^3;
% K(9,2)=0;
% K(10,2)=0;
% K(11,2)=0;
K(12,2)=6*E*Iz/L^2;

% K(1,3)=0;
% K(2,3)=0;
K(3,3)=12*E*Iy/L^3;
%K(4,3)=0;
K(5,3)=-6*E*Iy/L^2;
% K(6,3)=0;
% K(7,3)=0;
% K(8,3)=0;
K(9,3)=-12*E*Iy/L^3;
%K(10,3)=0;
K(11,3)=-6*E*Iy/L^2;
%K(12,3)=0;

% K(1,4)=0;
% K(2,4)=0;
% K(3,4)=0;
K(4,4)=G*Ip/L;
% K(5,4)=0;
% K(6,4)=0;
% K(7,4)=0;
% K(8,4)=0;
% K(9,4)=0;
K(10,4)=-G*Ip/L;
% K(11,4)=0;
% K(12,4)=0;
%
% K(1,5)=0;
% K(2,5)=0;
K(3,5)=-6*E*Iy/L^2;
%K(4,5)=0;
K(5,5)=4*E*Iy/L;
% K(6,5)=0;
% K(7,5)=0;
% K(8,5)=0;
K(9,5)=6*E*Iy/L^2;
%K(10,5)=0;
K(11,5)=2*E*Iy/L;
% K(12,5)=0;
%
% K(1,6)=0;
K(2,6)=6*E*Iz/L^2;
% K(3,6)=0;
% K(4,6)=0;
% K(5,6)=0;
K(6,6)=4*E*Iz/L;
%K(7,6)=0;
K(8,6)=-6*E*Iz/L^2;
% K(9,6)=0;
% K(10,6)=0;
% K(11,6)=0;
K(12,6)=2*E*Iz/L;

K(1,7)=-E*A/L;
% K(2,7)=0;
% K(3,7)=0;
% K(4,7)=0;
% K(5,7)=0;
% K(6,7)=0;
K(7,7)=E*A/L;
% K(8,7)=0;
% K(9,7)=0;
% K(10,7)=0;
% K(11,7)=0;
% K(12,7)=0;
%
% K(1,8)=0;
K(2,8)=-12*E*Iz/L^3;
% K(3,8)=0;
% K(4,8)=0;
% K(5,8)=0;
K(6,8)=-6*E*Iz/L^2;
%K(7,8)=0;
K(8,8)=12*E*Iz/L^3;
% K(9,8)=0;
% K(10,8)=0;
% K(11,8)=0;
K(12,8)=-6*E*Iz/L^2;

% K(1,9)=0;
% K(2,9)=0;
K(3,9)=-12*E*Iy/L^3;
%K(4,9)=0;
K(5,9)=6*E*Iy/L^2;
% K(6,9)=0;
% K(7,9)=0;
% K(8,9)=0;
K(9,9)=12*E*Iy/L^3;
%K(10,9)=0;
K(11,9)=6*E*Iy/L^2;
% K(12,9)=0;
%
% K(1,10)=0;
% K(2,10)=0;
% K(3,10)=0;
K(4,10)=-G*Ip/L;
% K(5,10)=0;
% K(6,10)=0;
% K(7,10)=0;
% K(8,10)=0;
% K(9,10)=0;
K(10,10)=G*Ip/L;
% K(11,10)=0;
% K(12,10)=0;
%
% K(1,11)=0;
% K(2,11)=0;
K(3,11)=-6*E*Iy/L^2;
%K(4,11)=0;
K(5,11)=2*E*Iy/L;
% K(6,11)=0;
% K(7,11)=0;
% K(8,11)=0;
K(9,11)=6*E*Iy/L^2;
%K(10,11)=0;
K(11,11)=4*E*Iy/L;
% K(12,11)=0;
%
% K(1,12)=0;
K(2,12)=6*E*Iz/L^2;
% K(3,12)=0;
% K(4,12)=0;
% K(5,12)=0;
K(6,12)=2*E*Iz/L;
%K(7,12)=0;
K(8,12)=-6*E*Iz/L^2;
% K(9,12)=0;
% K(10,12)=0;
% K(11,12)=0;
K(12,12)=4*E*Iz/L;

end %Function


