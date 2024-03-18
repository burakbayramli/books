function [ output_args ] = fplotspar(geo,results,structure,wingno)
%This function generates and plts the FEM spar geometry for validation

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


stick_spar_pos=((structure.spars(wingno,1)+structure.spars(wingno,2))/2).*geo.c(wingno);                %chord position of stick spar
globalbeamoffset=[geo.startx(wingno)+(stick_spar_pos), geo.starty(wingno),  geo.startz(wingno) ];       %start coordinate of stickspar
geo.vCfraction(wingno)=(structure.spars(wingno,1)+structure.spars(wingno,2))/2;                         %Chord fraction of stick spar.


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










end

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
