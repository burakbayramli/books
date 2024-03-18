function [results]=coeff_create3(results,lattice,state,ref,geo)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Coefficient creator: Essential function for TORNADO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Computes aerodynamic coefficients
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Author:	Tomas Melin, KTH, Department of Aeronautical
%                               and Vehicle Engineering
%			copyright 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONTEXT:	Subsidary function for TORNADO
% Called by:	solverloop
% Calls:			MATLAB standard functions
% Loads:
% Saves:
% Input:
% Output: forces moments coefficients
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delta=0.01;
q=0.5*state.rho*state.AS^2;			    %calculating dynamic pressure
%for coefficient calculation
[a b void]=size(results.F);

npan=cumsum(sum((geo.nx+geo.fnx).*geo.ny,2).*(geo.symetric+1)'); %Number of panels per wing
normal_force = zeros(a,1);
for s=1:a
    normal_force(s)=squeeze(results.F(s,1,:))'*lattice.N(s,:)';
end

panel_area=tarea(lattice.XYZ);

stat_press=normal_force./panel_area;	%Delta pressure, top/bottom
results.cp=((stat_press)./(q))';

CX=results.FORCE(:,:,1)/(q*ref.S_ref);
CY=results.FORCE(:,:,2)/(q*ref.S_ref);
CZ=results.FORCE(:,:,3)/(q*ref.S_ref);

B2WTransform=[cos(state.betha)*cos(state.alpha),        -sin(state.betha),          cos(state.betha)*sin(state.alpha) ;...
    cos(state.alpha)*sin(state.betha),         cos(state.betha),          sin(state.betha)*sin(state.alpha) ;...
    -sin(state.alpha),                        0,                           cos(state.alpha)];
for i=1:b
    lemma(i,:)=B2WTransform*squeeze(results.FORCE(:,i,:));
end

D=lemma(:,1)';
C=lemma(:,2)';
L=lemma(:,3)';

CL=L/(q*ref.S_ref);
CD=D/(q*ref.S_ref);
CC=C/(q*ref.S_ref);

Cl=results.MOMENTS(1,:,1)/(q*ref.S_ref*ref.b_ref);
Cm=results.MOMENTS(1,:,2)/(q*ref.S_ref*ref.C_mac);
Cn=results.MOMENTS(1,:,3)/(q*ref.S_ref*ref.b_ref);

% ------------ CL per wing computation
npan=cumsum(sum((geo.nx+geo.fnx).*geo.ny,2).*(geo.symetric+1)'); %Number of panels per wing

index1=1;

for i=1:geo.nwing
    index2=npan(i);
    
    lemma2=B2WTransform*(sum(squeeze(results.F(index1:index2,1,:))))';
    
    results.CLwing(i)=lemma2(3)/(q*ref.S_ref);
    results.CDwing(i)=lemma2(1)/(q*ref.S_ref);
    results.CYwing(i)=lemma2(2)/(q*ref.S_ref);
    
    index1=npan(i)+1;
end
% ----------

%%Setting output
results.L=L(1);
results.D=D(1);
results.C=C(1);

results.CX=CX(:,1);
results.CY=CY(:,1);
results.CZ=CZ(:,1);
results.CL=CL(:,1);
results.CD=CD(:,1);
results.CC=CC(:,1);
results.Cl=Cl(:,1);
results.Cm=Cm(:,1);
results.Cn=Cn(:,1);

results.F=squeeze(results.F(:,1,:));
results.M=squeeze(results.M(:,1,:));

results.FORCE=squeeze(results.FORCE(:,1,:));
results.MOMENTS=squeeze(results.MOMENTS(:,1,:));

delta=config('delta');
fac1=ref.b_ref /(2*state.AS);
fac2=ref.C_mac /(2*state.AS);


%%Differentiating
dCX=(CX-CX(:,1))./delta;
dCY=(CY-CY(:,1))./delta;
dCZ=(CZ-CZ(:,1))./delta;

dCL=(CL-CL(:,1))./delta;
dCD=(CD-CD(:,1))./delta;
dCC=(CC-CC(:,1))./delta;

dCl=(Cl-Cl(:,1))./delta;
dCm=(Cm-Cm(:,1))./delta;
dCn=(Cn-Cn(:,1))./delta;

results.CL_a=dCL(2);
results.CD_a=dCD(2);
results.CC_a=dCC(2);
results.CX_a=dCX(2);
results.CY_a=dCY(2);
results.CZ_a=dCZ(2);
results.Cl_a=dCl(2);
results.Cm_a=dCm(2);
results.Cn_a=dCn(2);

results.CL_b=dCL(3);
results.CD_b=dCD(3);
results.CC_b=dCC(3);
results.CX_b=dCX(3);
results.CY_b=dCY(3);
results.CZ_b=dCZ(3);
results.Cl_b=dCl(3);
results.Cm_b=dCm(3);
results.Cn_b=dCn(3);

results.CL_P=dCL(4)/fac1;
results.CD_P=dCD(4)/fac1;
results.CC_P=dCC(4)/fac1;
results.CX_P=dCX(4)/fac1;
results.CY_P=dCY(4)/fac1;
results.CZ_P=dCZ(4)/fac1;
results.Cl_P=dCl(4)/fac1;
results.Cm_P=dCm(4)/fac1;
results.Cn_P=dCn(4)/fac1;

results.CL_Q=dCL(5)/fac2;
results.CD_Q=dCD(5)/fac2;
results.CC_Q=dCC(5)/fac2;
results.CX_Q=dCX(5)/fac2;
results.CY_Q=dCY(5)/fac2;
results.CZ_Q=dCZ(5)/fac2;
results.Cl_Q=dCl(5)/fac2;
results.Cm_Q=dCm(5)/fac2;
results.Cn_Q=dCn(5)/fac2;

results.CL_R=dCL(6)/fac1;
results.CD_R=dCD(6)/fac1;
results.CC_R=dCC(6)/fac1;
results.CX_R=dCX(6)/fac1;
results.CY_R=dCY(6)/fac1;
results.CZ_R=dCZ(6)/fac1;
results.Cl_R=dCl(6)/fac1;
results.Cm_R=dCm(6)/fac1;
results.Cn_R=dCn(6)/fac1;

try
    results.CL_d=dCL(7:end);
    results.CD_d=dCD(7:end);
    results.CC_d=dCC(7:end);
    results.CX_d=dCX(7:end);
    results.CY_d=dCY(7:end);
    results.CZ_d=dCZ(7:end);
    results.Cl_d=dCl(7:end);
    results.Cm_d=dCm(7:end);
    results.Cn_d=dCn(7:end);
end

[results]=fStripforce(geo,results,lattice,state,ref);

end%function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function [results]=fStripforce(geo,results,lattice,state,ref)
%This lemma function computes the aerodynamic force on each strip.

dynp=0.5*state.rho*state.AS^2;
S=ref.S_ref;

try
    vCfraction=geo.vCfraction;      %catch cases with no spar position
catch
    vCfraction=0.25*ones(size(geo.c));   %defaulting to put spar shearcenter at c/4
end

B2WTransform=[cos(state.betha)*cos(state.alpha),        -sin(state.betha),          cos(state.betha)*sin(state.alpha) ;...
    cos(state.alpha)*sin(state.betha),         cos(state.betha),          sin(state.betha)*sin(state.alpha) ;...
    -sin(state.alpha),                        0,                           cos(state.alpha)];

F=results.F;                            %Reassigning to save some space
%% Vortex points
[s1 s2 s3]=size(lattice.VORTEX);
if s2==8
    pV1=squeeze(lattice.VORTEX(:,4,:));
    pV2=squeeze(lattice.VORTEX(:,5,:));
elseif s2==4
    pV1=squeeze(lattice.VORTEX(:,2,:));
    pV2=squeeze(lattice.VORTEX(:,3,:));
end
pV=(pV1+pV2)/2;
%%

[ai bi]=size(geo.nx);           %number of wings and panels
cnx=geo.nx+geo.fnx;             %corrected number of xpanels

cnx2=[];                        %mucking about with index variables
for i=1:ai
    for j=1:bi
        if cnx(i,j)>0
            cnx2=[cnx2 cnx(i,j)];
        end
    end
end

for i=1:geo.nwing;
    cny(i,:)=geo.ny(i,:).*(geo.symetric(i)+1); %corrected number of ypanels
end

stripsperwing=sum(cny,2);

%% Compute force action point and strip moment axis
m=0;
index2=0;
q=0;
for i=1:ai          %loop per wing
    for j=1:bi
        if cny(i,j)>=1
            q=q+1;%loop per partition
        end
        for k=1:cny(i,j)
            %per strip loop
            index1=index2+1;
            index2=index2+cnx2(q);
            m=m+1;
            
            %% Compute force action point and  strip moment axis
            cornerp=squeeze([lattice.XYZ(index1,1,:);
                lattice.XYZ(index1,2,:);
                lattice.XYZ(index2,3,:);
                lattice.XYZ(index2,4,:)]);
            
            localC1=[(cornerp(1,:)+cornerp(2,:))/2];
            localC2=[(cornerp(3,:)+cornerp(4,:))/2];
            Mpoint=(1-vCfraction(i))*localC1+(vCfraction(i))*localC2;
            yprimestation(m)=sign(Mpoint(2))*sqrt(Mpoint(2)^2+Mpoint(3)^2);
            
            %Local chord
            lemma1=localC1-localC2;
            lc=sqrt(sum(lemma1.^2));
            
            %local span
            lemma1=(-cornerp(1,:)+cornerp(2,:));
            lemma2=lemma1.*[0 1 1];%Disregarding x component
            ls(m)=sqrt(sum(lemma2.^2));
            
            %Strip Area
            la=ls(m)*lc;
            
            %%
            %Forces
            F0(m)=sum(sqrt(F(index1:index2,2).^2+F(index1:index2,3).^2)); %Only Z and Y component
            
            h(:,1)=Mpoint(1)-pV(index1:index2,1);
            h(:,2)=Mpoint(2)-pV(index1:index2,2);
            h(:,3)=Mpoint(3)-pV(index1:index2,3);
            
            F3(m,:)=sum(F(index1:index2,:),1);
            M3(m,:)=sum(cross(F(index1:index2,:),h),1);
            
            F_w(m,:)=(B2WTransform*F3(m,:)')';              %strip forces in wind system
            Cx_w(m,:)=2*(F_w(m,:))./(state.rho*state.AS^2*la);   %strip coefficients in wind system.
            
            clear h
            %% Coefficients
            CZprime(m)=F0(m)/(dynp*la);
            
        end
    end
    
    [yps or]=sort(yprimestation);
    [a b]=size(yps);
    
    results.ystation(1:b,i)=(yps);
    results.stripforce(1:b,i)=F0(or);    %Strip force in *BEAM* z direction.
    results.aeroload.CZprime(1:b,i)=CZprime(or);
    results.ForcePerMeter(1:b,i)=F0(or)./ls(or);   %Beam coordinate z-force per meter.
    results.panelspan=ls(or);
    
    results.CL_local(1:b,i)=Cx_w(or,3);             %Local lift coefficient, in global wind direction.
    results.CD_local(1:b,i)=Cx_w(or,1);             %Local Drag coefficient, in global wind direction.
    results.CY_local(1:b,i)=Cx_w(or,2);             %Local sideforce coefficient, in global wind direction.
    
    results.aeroload.F(1:b,i,1)=F3(or,1)./ls(or)';  %Force per meter
    results.aeroload.F(1:b,i,2)=F3(or,2)./ls(or)';  %Force per meter
    results.aeroload.F(1:b,i,3)=F3(or,3)./ls(or)';  %Force per meter
    results.aeroload.M(1:b,i,1)=M3(or,1)./ls(or)';  %Moment per meter
    results.aeroload.M(1:b,i,2)=M3(or,2)./ls(or)';  %Moment per meter
    results.aeroload.M(1:b,i,3)=M3(or,3)./ls(or)';  %Moment per meter
    
    
    % Cumulative load, wingtip to wingtip 
    
    if geo.symetric(i)==1   %symmetric case
        for k=1:b/2
            shear(k,i)=sum(results.stripforce(1:k,i));
        end
        
        for k=b:-1:(b/2+1)
            shear(k,i)=-sum(results.stripforce(k:b,i));
        end
        
    end
    
    if geo.symetric(i)==0   %asymmetric case
        for k=1:b
            shear(k,i)=sum(results.stripforce(1:k,i));
        end
    end
   
    %%%%%%%%%%%%%%%
    %% Moments
    
    arm=ls(or);
    if geo.symetric(i)==1   %symmetric case
        for k=1:b/2
            bend(k,i)=-sum(shear(1:k,i).*arm(1:k)');
            twist(k,i)=-sum(results.aeroload.M(1:k,i,3).*results.panelspan(1:k)');
        end
        
        for k=b:-1:(b/2+1)
            bend(k,i)=sum(shear(k:b,i).*arm(k:b)') ;
            twist(k,i)=sum(results.aeroload.M(k:b,i,3).*results.panelspan(k:b)');
        end
              
    end
    
    if geo.symetric(i)==0   %symmetric case
        for k=1:b
            bend(k,i)=-sum(shear(1:k,i).*arm(1:k)') ;
            twist(k,i)=sum(results.aeroload.M(1:k,i,3).*results.panelspan(1:k)');
        end
    end  
    
    clear yprimestation F3 M3 h F0 ls m Cx_w cl
    m=0;
end

results.bend=bend;
results.shear=shear;
results.twist=twist;

return


%% REST OF CODE NOT ACTIVE


out.shear=results.load.F(:,3);
out.Bend=results.load.M(:,1);
out.Twist=results.load.M(:,2);


%%%%%%%%%%%%%%%%5
%Stuff below is experimental
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%return
H=figure(1);
hold on
set(H,'Position',[10 10 0.8*3*210 0.8*3*297])
%Changing variables to plot only partition outline
g2=geo;
g2.nx=double(g2.nx>0);
g2.ny=double(g2.ny>0);
g2.fnx=double(g2.fnx>0);
s2.AS=1;
s2.alpha=0;
s2.betha=0;
s2.P=0;
s2.Q=0;
s2.R=0;
s2.ALT=0;
s2.rho=1;
s2.pgcorr=0;

[l2,ref]=fLattice_setup2(g2,s2,1);

subplot(4,1,1,'position',[0.1 0.8 0.8 0.17]); hold on
%axes('position',[0.1 0.8 0.8 0.17])
g=fill3(l2.XYZ(:,:,1)',l2.XYZ(:,:,2)',l2.XYZ(:,:,3)','w');
set(g,'LineWidth',2);
view([90,90]);
%axis equal
hold on
%xlabel('Aircraft body x-coordinate')
%ylabel('Aircraft body y-coordinate')
%zlabel('Aircraft body z-coordinate')
title('Wing aerodynamic loading')
axis off


subplot(4,1,2)
%axes()
h1=plot(results.ypstation,results.shear);
hold on
set(h1,'LineWidth',2)
h2=gca;
%set(h2,'XTickLabel',[],'position',[0.1 0.6 0.8 0.17])
grid on
ylabel('Shear force, F_z, [N]')

subplot(4,1,3)
%axes();
h1=plot(out.ypstation,out.Bend);
hold on
set(h1,'LineWidth',2)
h2=gca;
%set(h2,'XTickLabel',[],'position',[0.1 0.4 0.8 0.17])


grid on
ylabel('Bend moment, M_x, [Nm]')

subplot(4,1,4)
%axes('position',[0.1 0.2 0.8 0.17]);
h1=plot(out.ypstation,out.Twist);
hold on
set(h1,'LineWidth',2)
%h2=gca;
%set(h5,'OuterPosition',[0 0.0 1 0.2])
grid on
ylabel('Twist moment, M_y, [Nm]')
xlabel('Span station, y, [m]')
h2=gca;
%set(h2,'position',[0.1 0.2 0.8 0.17])


end %function stripforce


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [results]=spanload6(results,geo,lattice,state)
%
%
%   ORPHAN FUNCTION, NOT USED AFTER V135.
%THIS SUBFUNCTION IS NOT TO BE CALLED
%to be removed in next version
%

disp('HOW DID YOU GET HERE?')
return
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	CONFIG: Basic computation function   	%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Computes the spanload (force/meter) for
%  all wings
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Author: Tomas Melin, KTH, Department of%
%	Aeronautics, copyright 2002				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Context: Auxillary function for TORNADO%
%	Called by: TORNADO SOlverloop          %
%	Calls:	None							%
%	Loads:	None							%
%	Generates:	force per meter array
%     			(ystations X wings)
%					Ystation array
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Revision history post alfa 1.0			%
%  2007-02-14  rho moved to state
%  2002-05-02
%   input var T (taper) added to get local
%	 chords.
% input var AS (airspeed) added
%   local chord computation function call added
%

%rho=config('rho');	                            %set density
lemma=size(geo.b);								%number of partitions and wings

B2WTransform=[cos(state.betha)*cos(state.alpha),        -sin(state.betha),          cos(state.betha)*sin(state.alpha) ;...
    cos(state.alpha)*sin(state.betha),         cos(state.betha),          sin(state.betha)*sin(state.alpha) ;...
    -sin(state.alpha),                        0,                           cos(state.alpha)];

noofpanels=sum(((geo.nx+geo.fnx).*geo.ny),2).*(geo.symetric'+1); %number of panels in total (symmetry disregarded)
lemma=size(results.F);

corrx=[];
corry=[];
corrz=[];

lemma2=size(geo.b);
for i=1:lemma2(1)
    corry=[corry;ones(noofpanels(i),1)*geo.starty(i)];
    corrz=[corrz;ones(noofpanels(i),1)*geo.startz(i)];
end

for i=1:lemma(1)
    forceMagn(i)=-results.F(i,:)*lattice.N(i,:)'; %Force magnitude (3Dvector -> scalar)
    %Aligned with panel normals
    
    lemma4(i,:)=B2WTransform*results.F(i,:)';
    forceLift(i)=lemma4(i,3);                     %Lift on each panel, this is outdata for the
    %viscous correction.
end

A1=((lattice.XYZ(:,1,:)-lattice.XYZ(:,2,:)));
p_span=sqrt(A1(:,:,2).^2+A1(:,:,3).^2); %span of each panel

p_mid=(lattice.XYZ(:,1,:)+lattice.XYZ(:,2,:))/2;                  	%midpoint of each panel
p_mid_r=sqrt(((p_mid(:,:,2)-corry).^2+(p_mid(:,:,3)-corrz).^2));	%Radius from centerline to midpoint

FPM=forceMagn'./p_span;					%Force per meter on each panel.
LPM=forceLift'./p_span;					%Lift per meter on each panel.

knx=geo.nx+geo.fnx;						    %corrected number of panel in x-direction
lemma2=((knx).*geo.ny);						%number of panels in total (symmetry disregarded)

p=[];
p2=[];

lemma=size(geo.b);

for i=1:lemma(1)
    for j=1:lemma(2)
        
        a=[knx(i,j).*ones(geo.ny(i,j),1)];%computing # x-stations to add to each
        %y-station
        c=ones(geo.ny(i,j),1);				%sign vector for y station. !!TROUBLE HERE!!
        if geo.symetric(i);
            a=[a;a];							%Doubling if wing is symmetric.
            c=[c;-c];
        end
        p=[p;a];
        p2=[p2;c];
    end
end

lemma3=size(p);						%Total number of ystations for all wings;

for i=1:lemma3
    SF(i)=sum(FPM(1:p(i)));		    %Moving beginning of FPM into SF
    FPM=FPM(p(i)+1:end);		    %Removing beginning of FPM
    
    LF(i)=sum(LPM(1:p(i)));		    %Moving beginning of LPM into LF
    LPM=LPM(p(i)+1:end);			%Removing beginning of LPM
    
    R(i)=sum(p_mid_r(1:p(i)))/p(i);   %Moving p_mid_r into R
    p_mid_r=p_mid_r(p(i)+1:end);	  %Removing beginning of p_mid_r
      
    strip_span(i)=p_span(1);
    p_span=p_span(p(i)+1:end);
    
end
ystation=R.*p2';							%Fixing signs on negative symmetric half.
ForcePerMeter=SF;							%Renaming

%A=[ForcePerMeter;ystation];			%Output matrix, spanloads with spanstation.
%y_entries=sum(ny,2).*(1+symetric');	%numer of entries in A that corresponds
%to each wing
%local chord computation
lc=fLocal_chord2(geo,lattice);

%%%%%%%
%Sorting algorithm to couple force per meter (fpm)
%value with corresponding y-station
%

SF3=2*LF./(state.rho*state.AS^2*lc);

kny=sum(geo.ny,2).*(geo.symetric+1)'; %corrected number of spanwise strips per wing
for i=1:geo.nwing
    SF2=(SF(1:kny(i)))';
    SF4=(SF3(1:kny(i)))'; 
    
    SF=SF(kny(i)+1:end);         %removing beginning
    SF3=SF3(kny(i)+1:end);       %removing beginning
    
    [ystat Or]=sort((ystation(1:kny(i)))');
    ys(1:kny(i),i)=ystat;
    
    ystation=ystation((kny(i)+1):end);
    order(1:kny(i),i)=Or;
    
    fpm(1:kny(i),i)=SF2(Or);
    clpm(1:kny(i),i)=SF4(Or);
    
    strip_span2=(strip_span(1:kny(i)))';
    strip_span=strip_span(kny(i)+1:end);
    strip_span3(1:kny(i),i)=strip_span2(Or);
    
    lc2=(lc(1:kny(i)))';
    lc=lc(kny(i)+1:end);
    lc3(1:kny(i),i)=lc2(Or);
        
    %Shear force load calculation
    lps(1:kny(i),i)=fpm(1:kny(i),i).*strip_span3(1:kny(i),i);   %Load per strip, per wing
    load1=fpm.*strip_span3;
    
    if geo.symetric(i)==1
        %port side
        ys_p=ys(1:kny(i)/2,i);
        strip_span_p=strip_span3(1:kny(i)/2,i);
        shear_p=cumsum(load1(1:kny(i)/2,i));
        
        for j=1:(kny(i)/2);
            dist=(ys_p-ys_p(j));
            load2=load1(1:kny(i)/2,i).*dist;
            bend_p(j)=sum(load2(1:j));
        end
        
        %stb side
        ys_stb=flipud(ys((kny(i)/2+1):kny(i),i));
        strip_span_stb=flipud(strip_span3((kny(i)/2+1):kny(i),i));
        shear_stb=-flipud(cumsum(flipud(load1((kny(i)/2+1):kny(i),i))));
        
        for j=1:(kny(i)/2)
            dist=(ys_stb-ys_stb(j));
            load2=-(flipud(load1((kny(i)/2+1):kny(i),i)).*dist);
            bend_stb(j)=sum(load2(1:j));
        end
        bend_stb=fliplr(bend_stb);
        
        
        
        
    else %Assymetric case
        shear_p=[];
        bend_p=[];
        
        
        ys_stb=(ys(1:kny(i),i));
        strip_span_stb=(strip_span3(1:kny(i),i));
        shear_stb=-flipud(cumsum(flipud(load1(1:kny(i),i))));
        
        for j=1:(kny(i))
            dist=(ys_stb-ys_stb(j));
            load2=-((load1(1:kny(i),i).*dist));
            bend_stb(j)=sum(load2(1:j));
        end
        bend_stb=-fliplr(bend_stb);
        
    end %symmetry cases
    
    
    
    
    %combining
    sh=[shear_p;shear_stb];
    bm=[bend_p';bend_stb'];
    
    a=size(sh);
    
    shearforce(1:a(1),i)=sh;
    bendingmoment(1:a(1),i)=bm;
    
    clear bend_p bend_stb shear_p shear_stb dist load1 load2
        
end

results.ystation=sparse(ys);
results.ForcePerMeter=sparse(fpm);
results.ShearForce=shearforce;
results.BendingMoment=bendingmoment;
results.CL_local=sparse(clpm);

end%function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [panel_area]=tarea(XYZ)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tarea: Subsidary function for TORNADO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculates the area of each panel
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Author:	Tomas Melin, KTH, Department of Aeronautics	%
%				Copyright 2000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONTEXT:	Subsidaty function for TORNADO
% Called by:	coeff_create
%
% Calls:	MATLAB 5.2 std fcns
% Loads:	none
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%[a b c]=size(XYZ)
x = XYZ(:,:,1);
y = XYZ(:,:,2);
z = XYZ(:,:,3);

ax = x(:,2)-x(:,1);
ay = y(:,2)-y(:,1);
az = z(:,2)-z(:,1);

bx = x(:,4)-x(:,1);
by = y(:,4)-y(:,1);
bz = z(:,4)-z(:,1);

cx = x(:,2)-x(:,3);
cy = y(:,2)-y(:,3);
cz = z(:,2)-z(:,3);

dx = x(:,4)-x(:,3);
dy = y(:,4)-y(:,3);
dz = z(:,4)-z(:,3);

ar1 = [by.*az-bz.*ay,...
       bz.*ax-bx.*az,...
       bx.*ay-by.*ax];
ar2 = [cy.*dz-cz.*dy,...
       cz.*dx-cx.*dz,...
       cx.*dy-cy.*dx];
ar1 = sqrt(sum(ar1.^2,2));
ar2 = sqrt(sum(ar2.^2,2));
panel_area = (ar1+ar2)/2;
%size(panel_are)
% for i=1:a
%     p1=[XYZ(i,1,1) XYZ(i,1,2) XYZ(i,1,3)];	%sets up the vectors
%     p2=[XYZ(i,2,1) XYZ(i,2,2) XYZ(i,2,3)];	%to the corners of the
%     p3=[XYZ(i,3,1) XYZ(i,3,2) XYZ(i,3,3)];	%panel.
%     p4=[XYZ(i,4,1) XYZ(i,4,2) XYZ(i,4,3)];
%     
%     a=p2-p1;	%sets up the edge vectors
%     b=p4-p1;
%     c=p2-p3;
%     d=p4-p3;
%     
%     ar1=norm(cross(b,a))/2;	%calculates the ctoss product of
%     ar2=norm(cross(c,d))/2;	%two diagonal corners
%     
%     panel_area(i)=ar1+ar2;	%Sums up the product to make the
% end						    %Area
% disp('tarea');
% norm(panel_are - panel_area')
end% function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function[lc]=fLocal_chord2(geo,lattice)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Geometry function 						 	%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Computes the Local chord at each collocation
%  point row.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Author: Tomas Melin, KTH, Department of%
%	Aeronautics, copyright 2002				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Context: Auxillary function for TORNADO%
%	Called by: TORNADO spanload            %
%	Calls:	None									%
%	Loads:	None									%
%	Generates:	Local chord vector lc, same
%  order as colloc, N, and the others
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[indx1 indx2]=size(geo.b);

for s=1:indx1;	   		%Looping over wings
    CHORDS(s,1)=geo.c(s);		%calculating chords of first element
end

for s=1:indx1				%Looping over wings
    for t=1:indx2			%Looping over partitions
        %Chord loop, generating chords for wing partitions
        CHORDS(s,t+1)=CHORDS(s,t)*geo.T(s,t);	%calculating
        %element root-chord
    end
end

lc=[];	%Local chord vector.

panelchords1=sqrt(sum((lattice.XYZ(:,1,:)-lattice.XYZ(:,4,:)).^2,3)); %inboard
panelchords2=sqrt(sum((lattice.XYZ(:,2,:)-lattice.XYZ(:,3,:)).^2,3)); %outboard
panelchords3=(panelchords1+panelchords2)/2; %Chord of each panel, CAUTION
%this is really camber line
%length, so not really chord
%for very cambered profiles

for i=1:indx1;			%Wing
    for j=1:indx2;		%Partition
        lemma=[]; %local chord lemma vector.
        chordwisepanels=geo.nx(i,j)+geo.fnx(i,j); %number of panels chordwise on
        %this partition
        for k=1:geo.ny(i,j)                       %loop over panel strips.
            if geo.ny(i,j)~=0
                lemma=[lemma sum(panelchords3(1:chordwisepanels))];
                panelchords3=panelchords3((chordwisepanels+1):end);
                %size(panelchords3);
            end
        end
        if geo.symetric(i)==1	%symmetric wings got two sides
            lc=[lc lemma lemma];
            panelchords3=panelchords3((chordwisepanels*geo.ny(i,j)+1):end);
        else
            lc=[lc lemma];
        end
        
    end
end
end%function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

