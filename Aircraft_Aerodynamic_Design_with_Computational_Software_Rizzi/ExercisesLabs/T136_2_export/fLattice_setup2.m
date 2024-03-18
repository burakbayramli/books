
function [lattice,ref]=fLattice_setup2(geo,state,latticetype)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (C) 1999, 2007 Tomas Melin
%
% This file is part of Tornado
%
% Tornado is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public
% License as published by the Free Software Foundation;
% either version 2, or (at your option) any later version.
%
% Tornado is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied
% warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
% PURPOSE.  See the GNU General Public License for more
% details.
%
% You should have received a copy of the GNU General Public
% License along with Tornado; see the file GNU GENERAL
% PUBLIC LICENSE.TXT.  If not, write to the Free Software
% Foundation, 59 Temple Place -Suite 330, Boston, MA
% 02111-1307, USA.
%
% usage: [LATTICE,REF]=fLattice_setup(GEO,STATE,latticetype);
%
%   Generates the LATTICE and REF structs from the GEO and STATE structs.
%   First, the function invokes geosetup to yield the data correlation to
%   the geometry, then it appends wake data (i.e. the deflecting wake) in
%   wakesetup, and lastly it rotates any control surface points in the
%   lattice struct.
%
%   If latticetype=1, the lattice produced will be a standard vortex
%   lattice -lattice, i.e. only horseshoes, not slings.
%
% Example:
%
%  [lattice,ref]=fLattice_setup(geo,state);
%
% Calls:
%       geosetup15      Assembles the lattice and ref structured from geo
%       wakesetup2      Adds the wake to the lattice struct.
%       setrudder3      Deflects any flap, aileron, elevator or ruder.
%                       All three functions above are inline as only this
%                       function calls them.

%       terror          Displays various Error messages.
%
% Author: Tomas Melin <melin@kth.se>
% Keywords: lattice generation
%
% Revision History:
%   Bristol, 2007-06-27:  Addition of new header. TM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
try
    latticetype;
catch
    latticetype=0;  %If latticetype is unspecified, go with vortexslings.
end
%warning('off')
[lattice,ref]=geosetup15(geo);

%Aircraft lattice done

[dim1 dim2 dim3]=size(lattice.VORTEX);
if dim2==8						%checking if vortex has wake
    lattice.VORTEX=lattice.VORTEX(:,2:7,:); %discarding far wake points in wake.
end

if state.AS~=0   %appending wake lattice points (farpoints)
    lattice=wakesetup2(lattice,state,ref); %setting up wake legs.
else
    terror(13)
end

[n,m]=find(geo.flapped');

if ~isempty(m)        %Any flap with deflection
    noof_flaps=sum(sum(geo.flapped));
    for k=1:noof_flaps %Loop all flaps and set them according to setting vector
        flap_no=k;
        deflection=(geo.flap_vector(m(k),n(k)));
        [lattice]=setrudder3(flap_no,deflection,lattice,geo);
    end
end

% Allmoving surface setting here
try
    [n,m]=find(geo.allmove');
    if ~isempty(m)        %Is any wing an allmoving surface?
        noof_allmoves=(sum(geo.allmove));
        for k=1:noof_allmoves %Loop all allmoving wings and set them according to deflection vector
            deflection=(geo.allmove_def(n(k)));
            [lattice]=wingrotation2(n(k),geo,lattice,...
           geo.allmove_axis(n(k),:),...
           geo.allmove_origin(n(k),:),...
          deflection);
        end
    end
catch
    %  disp(' no all-moving')
end
% End allmoving surface
[dim1 dim2 dim3]=size(lattice.VORTEX);
%STANDARD VLM, REPLACING SLINGS WITH HORSESHOES.
if latticetype==1
    %tdisp('Standard VLM')
    if dim2==8
        temporary=lattice.VORTEX(:,[1 4 5 8],:); %changing lattice to a standard VLM
    else
        temporary=lattice.VORTEX(:,[1 3 4 6],:); %if there is no wake
    end
    temporary(:,1,3)=temporary(:,2,3); %Flattening wake z
    temporary(:,4,3)=temporary(:,3,3); %Flattening wake z
    %JO 1905
    temporary(:,1,2)=temporary(:,2,2); %Flattening wake y
    temporary(:,4,2)=temporary(:,3,2); %Flattening wake y
    lattice.VORTEX=temporary;
end

end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Inline function geosetup
function [lattice,ref]=geosetup15(geo)
void=0;
try % has meshing type been defined?
    geo.meshtype;
catch % if not, set to equidistant
    geo.meshtype=ones(size(geo.T));
end

%npan=sum(sum((geo.nx+geo.fnx).*geo.ny,2).*(geo.symetric+1)');%number of panels

lattice.COLLOC= [];%zeros(npan,3);       %initializing
lattice.VORTEX= [];%zeros(npan,3);
lattice.N     = [];%zeros(npan,8,3);
lattice.XYZ   = [];%zeros(npan,5,3);
lattice.Cpar  = [];
lattice.Vorpar= [];
lattice.Ppar  = [];

X=[];
Y=[];
Z=[];
S=1;
Cmac=0;

loopsperwing=geo.nelem;
noofloops=loopsperwing;
temp=0;
noofwings=size(loopsperwing',1);
SX = zeros(noofwings,max(noofloops)+1);
CHORDS = SX;
SY = SX;
SZ = SX;

for s=1:noofwings			%Intermediate variable setuploop
    CHORDS(s,1)=geo.c(s);   %calculating chords of first element
    SX(s,1)=geo.startx(s);	%Element apex calculation
    SY(s,1)=geo.starty(s);	% Same-o
    SZ(s,1)=geo.startz(s);  % Same-o
end

for s=1:noofwings
    for t=1:(noofloops(s))
        %Chord loop, generating chords for wing sections.
        %And startingpoints for partition-quads
        % jeeez ... so continuous chord ! no dogtooth
        CHORDS(s,t+1)=CHORDS(s,t)*geo.T(s,t);	%calculating
        %element root-chord
        
        SX(s,t+1)=0.25*CHORDS(s,t)+geo.b(s,t)*(tan(geo.SW(s,t)))...
            -0.25*CHORDS(s,t+1)+SX(s,t) ;
        SY(s,t+1)=geo.b(s,t)*cos(geo.dihed(s,t))+SY(s,t);
        SZ(s,t+1)=geo.b(s,t)*sin(geo.dihed(s,t))+SZ(s,t);
    end
end


%MAIN GEOMETRY SETUP LOOP, CREATES Partition QUAD PANELS, VORTICES AND COLL-POINTS
for s=1:noofwings
    bb = [0 cumsum(geo.b(s,:))];
    for t=1:noofloops(s) %setuploop
        [C V N2 P Cpar, Vorpar, Ppar]=geometry19(geo.fnx(s,t),geo.ny(s,t),geo.nx(s,t),...
            geo.fsym(s,t),geo.fc(s,t),geo.flapped(s,t),geo.TW(s,t,:),geo.foil(s,t,:),...
            geo.T(s,t),geo.SW(s,t),CHORDS(s,t),geo.dihed(s,t),geo.b(s,t),...
            geo.symetric(s),SX(s,t),SY(s,t),SZ(s,t),geo.meshtype(s,t));
        % eta-pars: eta = (bb(t) + etaloc*b(t))/bb(end)
        ii = find(Cpar(:,2)<0);
        i0 = find(Cpar(:,2) > 0);
        Cpar(i0,2)     =(bb(t) +     Cpar(i0,2)*geo.b(s,t))/bb(end);
        Vorpar(i0,:,2) =(bb(t) + Vorpar(i0,:,2)*geo.b(s,t))/bb(end);
        Ppar(i0,:,2)   =(bb(t) +   Ppar(i0,:,2)*geo.b(s,t))/bb(end);
        
        Cpar(ii,2)     =(-bb(t) +     Cpar(ii,2)*geo.b(s,t))/bb(end);
        Vorpar(ii,:,2) =(-bb(t) + Vorpar(ii,:,2)*geo.b(s,t))/bb(end);
        Ppar(ii,:,2)   =(-bb(t) +   Ppar(ii,:,2)*geo.b(s,t))/bb(end);
        
        lattice.COLLOC= [lattice.COLLOC;C];
        lattice.VORTEX= [lattice.VORTEX;V];
        lattice.N     = [lattice.N;N2];
        % JO1701
        lattice.Cpar   = [lattice.Cpar;Cpar];
        lattice.Vorpar = [lattice.Vorpar;Vorpar];
        lattice.Ppar   = [lattice.Ppar;Ppar];
        
        S(s,t)=geo.b(s,t)*CHORDS(s,t)*((1+geo.T(s,t)))/2;
        Cmgc(s,t)=S(s,t)/geo.b(s,t);
        
        if geo.symetric(s)==1
            S(s,t)=S(s,t)*2;
        end
        
        lattice.XYZ =[lattice.XYZ;P];
    end
end

ref.b_ref=config('b_ref');
if isempty(ref.b_ref)
    B=sum(geo.b,2);
    ref.b_ref=B(1)*(geo.symetric(1)+1); % span of first wing
end

ref.S_ref=config('S_ref');
if isempty(ref.S_ref)
    S_r=sum(S,2);
    ref.S_ref=S_r(1);	%reference area = area of first wing
end

C_m=sum(Cmgc.*S,2);
ref.C_mgc=C_m(1)/ref.S_ref;		%Mean Geometric Chord  Gross surface  Main (first)

ref.C_mac=config('C_mac');
if isempty(ref.C_mac)
    nnn = geo.nelem(1);
    [ref.C_mac void]=fCmac(CHORDS(1,1:nnn+1),geo.b(1,:),geo.SW(1,:),...
        SX(1,:),SY(1,:),SZ(1,:),geo.dihed(1,:),geo.symetric(1)); %Main (first) wing Mean aerodymaic chord calculation
end

ref.mac_pos=config('mac_pos');
if isempty(ref.mac_pos)
     nnn = geo.nelem(1);
    [void ref.mac_pos]=fCmac(CHORDS(1,1:nnn+1),geo.b(1,:),geo.SW(1,:),...
        SX(1,:),SY(1,:),SZ(1,:),geo.dihed(1,:),geo.symetric(1)); %Main (first) wing Mean aerodymaic chord calculation
    %mac_pos=-mac_pos
end

end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
function [lattice]=wakesetup2(lattice,state,ref)
infdist=config('infinity');
if isempty(infdist)
    infdist=600*ref.b_ref;
end

[a b c]=size(lattice.VORTEX);
V2=lattice.VORTEX;
c=[1 b];
JO = 1; % JO 2004 to avoid wake vortices close to collocation 
        % or bound vortices
% if JO , wake as if alpha = beta = 0
if JO
  infx= infdist;
  infy= 0;
  infz= 0;
else
  infx= infdist*cos(state.alpha)*cos(state.betha);
  infy=-infdist*sin(state.betha);
  infz= infdist*sin(state.alpha)*cos(state.betha);
end
if JO
  dx = zeros(a,2);
  dy = zeros(a,2);
  dz = zeros(a,2);
else
  for t=1:a
    for s=1:2
      x=infx+lattice.VORTEX(t,c(s),1);
      y=infy+lattice.VORTEX(t,c(s),2);
      z=infz+lattice.VORTEX(t,c(s),3);
        
      psi  =state.P/state.AS*x;
      theta=state.Q/state.AS*x;
      fi   =state.R/state.AS*x;
        
      dx(t,s)=-x*(2-cos(theta)-cos(fi));
      dy(t,s)=+sin(psi)*z-sin(fi)*x+(1-cos(psi))*y;
      dz(t,s)=sin(theta)*x-sin(psi)*y+(1-cos(psi))*z;   
    end
  end
end

INF1 = zeros(a,1,3);
INF2 = INF1;
for i=1:a
    INF1(i,1,1)=lattice.VORTEX(i,1,1)+infx+dx(i,1);
    INF1(i,1,2)=lattice.VORTEX(i,1,2)+infy+dy(i,1);
    INF1(i,1,3)=lattice.VORTEX(i,1,3)+infz+dz(i,1);
    
    INF2(i,1,1)=lattice.VORTEX(i,b,1)+infx+dx(i,2);
    INF2(i,1,2)=lattice.VORTEX(i,b,2)+infy+dy(i,2);
    INF2(i,1,3)=lattice.VORTEX(i,b,3)+infz+dz(i,2);
end

lattice.VORTEX=[INF1(:,1,:) V2(:,:,:) INF2(:,1,:)];
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [lattice]=setrudder3(rudder,deflection,lattice,geo)

[I K]=find(geo.flapped');
try
    wing=K(rudder);
    division=I(rudder);
catch
    terror(2)
    return
end

[q1 q2 q3]=size(lattice.VORTEX);

if q2==8
    tempV1=lattice.VORTEX(:,1,:);
    tempV2=lattice.VORTEX(:,8,:);
    lattice.VORTEX=lattice.VORTEX(:,2:7,:);
end

fsym=geo.fsym(wing,division);

mp=3;

t=1;
r=0;
[q6 q7]=size(geo.nx);
nr=((geo.nx+geo.fnx).*geo.ny).*((ones(q6,q7)+(geo.symetric'*ones(1,q7))));
[q4 q5]=size(nr);
for i=1:q4
    for j=1:q5
        if geo.flapped(i,j)==1
            r=r+1;
        end
        if r<rudder
            t=t+nr(i,j);
        end
    end
end

nx=geo.nx(wing,division);
ny=geo.ny(wing,division);
fnx=geo.fnx(wing,division);

a1=[lattice.XYZ(t+nx,1,1) lattice.XYZ(t+nx,1,2) lattice.XYZ(t+nx,1,3)];
b1=[lattice.XYZ(t+nx,2,1) lattice.XYZ(t+nx,2,2) lattice.XYZ(t+nx,2,3)];

a2=[lattice.XYZ(t+nx,2,1) -lattice.XYZ(t+nx,2,2) lattice.XYZ(t+nx,2,3)];
b2=[lattice.XYZ(t+nx,1,1) -lattice.XYZ(t+nx,1,2) lattice.XYZ(t+nx,1,3)];

h=b1-a1;				%defining hingeline SB-side
h1_hat=h./norm(h);      %normalizing hingeline

h2=b2-a2;				%defining hingeline P-side
h2_hat=h2./norm(h2);    %normalizing hingeline

s=nx+fnx;

for i=1:(nx+fnx)*ny*(1+geo.symetric(wing));
    %loop for trailing edge points
  rad2=t+i-1;
    
  if rad2 < t+(nx+fnx)*ny; %if wing is symmetric and
                           %panel is on the SB-side
    a=a1;
    b=b1;
    h_hat=h1_hat;
    def=deflection;
    else					      %if wing is on the P-side
      h_hat=h2_hat;
      a=a2;
      b=b2;
      if fsym==0;				%if flap deflection is anti-
                        %symmetric
        def=-deflection;
      else
        def=deflection;
      end
    end
    
    for col=1:5:6
      p1(1)=lattice.VORTEX(rad2,col,1);
      p1(2)=lattice.VORTEX(rad2,col,2);
      p1(3)=lattice.VORTEX(rad2,col,3);
      if col<=mp
        r=p1-a;
        p2=trot3(h_hat,r,def);
        lattice.VORTEX(rad2,col,1)=p2(1)+a(1);
        lattice.VORTEX(rad2,col,2)=p2(2)+a(2);
        lattice.VORTEX(rad2,col,3)=p2(3)+a(3);
      else
        r=p1-b;
        p2=trot3(h_hat,r,def);
            
        lattice.VORTEX(rad2,col,1)=p2(1)+b(1);
        lattice.VORTEX(rad2,col,2)=p2(2)+b(2);
        lattice.VORTEX(rad2,col,3)=p2(3)+b(3);
      end
    end
end

for i=s:s:s*ny*(1+geo.symetric(wing))
    %stepping through number of strips
  for j=0:fnx-1 %stepping through number of flappanels
    ii=i-fnx;
    rad1=(t+ii+j);
        
        if rad1 < t+(nx+fnx)*ny; %if wing is symmetric and
            %panel is on the SB-side
            a=a1;
            b=b1;
            h_hat=h1_hat;
            def=deflection;
        else					    % wing is on the P-side
            h_hat=h2_hat;
            a=a2;
            b=b2;
            if fsym==0;				%if flap deflection is anti-
                %symmetric
                def=-deflection;
            else
                def=deflection;
            end
            
        end
        
        for k=0:3 %Vortex loop
            col=(k+mp-1);
            p1(1)=lattice.VORTEX(rad1,col,1);
            p1(2)=lattice.VORTEX(rad1,col,2);
            p1(3)=lattice.VORTEX(rad1,col,3);
            if col<=mp
                r=p1-a;
                p2=trot3(h_hat,r,def);
                
                lattice.VORTEX(rad1,col,1)=p2(1)+a(1);
                lattice.VORTEX(rad1,col,2)=p2(2)+a(2);
                lattice.VORTEX(rad1,col,3)=p2(3)+a(3);
            else
                r=p1-b;
                p2=trot3(h_hat,r,def);
                
                lattice.VORTEX(rad1,col,1)=p2(1)+b(1);
                lattice.VORTEX(rad1,col,2)=p2(2)+b(2);
                lattice.VORTEX(rad1,col,3)=p2(3)+b(3);
            end
            
        end
        
        %collocation point rotation
%       p1t = reshape(lattice.COLLOC(rad1,:),3,1);
        p1(1)=lattice.COLLOC(rad1,1);
        p1(2)=lattice.COLLOC(rad1,2);
        p1(3)=lattice.COLLOC(rad1,3);
%         disp('452')
%         norm(p1t-p1);
        c=(a+b)./2;
        r=p1-c;
        p2=trot3(h_hat,r,def);
        
        lattice.COLLOC(rad1,1)=p2(1)+c(1);
        lattice.COLLOC(rad1,2)=p2(2)+c(2);
        lattice.COLLOC(rad1,3)=p2(3)+c(3);
        
        %Normals rotation
        
        p1(1)=lattice.N(rad1,1);
        p1(2)=lattice.N(rad1,2);
        p1(3)=lattice.N(rad1,3);
        
        c=(a+b)./2;
        r=p1;
        p2=trot3(h_hat,r,def);
        
        lattice.N(rad1,1)=p2(1);
        lattice.N(rad1,2)=p2(2);
        lattice.N(rad1,3)=p2(3);
        
        for k=0:4 %panelcoords
            col=(k+1);
            p1(1)=lattice.XYZ(rad1,col,1);
            p1(2)=lattice.XYZ(rad1,col,2);
            p1(3)=lattice.XYZ(rad1,col,3);
            %disp('************')
            if col<=1;
                r=p1-a;
                p2=trot3(h_hat,r,def);
                
                lattice.XYZ(rad1,col,1)=p2(1)+a(1);
                lattice.XYZ(rad1,col,2)=p2(2)+a(2);
                lattice.XYZ(rad1,col,3)=p2(3)+a(3);
                %plot3([a(1), p2(1)+a(1)],[a(2), p2(2)+a(2)],[a(3), p2(3)+a(3)],'.-m')
                
            elseif col<=3
                r=p1-b;
                p2=trot3(h_hat,r,def);
                
                lattice.XYZ(rad1,col,1)=p2(1)+b(1);
                lattice.XYZ(rad1,col,2)=p2(2)+b(2);
                lattice.XYZ(rad1,col,3)=p2(3)+b(3);
                %plot3([b(1), p2(1)+b(1)],[b(2), p2(2)+b(2)],[b(3), p2(3)+b(3)],'.-m')
            else
                r=p1-a;
                p2=trot3(h_hat,r,def);
                
                lattice.XYZ(rad1,col,1)=p2(1)+a(1);
                lattice.XYZ(rad1,col,2)=p2(2)+a(2);
                lattice.XYZ(rad1,col,3)=p2(3)+a(3);
                %plot3([a(1), p2(1)+a(1)],[a(2), p2(2)+a(2)],[a(3), p2(3)+a(3)],'.-m')
            end
            
        end
    end
end

if q2==8
    lattice.VORTEX=[tempV1 lattice.VORTEX tempV2];
end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [C_mac,mac_start_coo]=fCmac(C,b,SW,sx,sy,sz,dihed,sym)
[void noofpan]=size(C);								%Create counter
for i=1:noofpan-1;
    T(i)=C(i+1)/C(i);                 %partition taper
    
    Cb=C(i);									      	%Base chord
    Ct=C(i+1);										    %Tip chord
    
    b_mac(i)=b(i)*(2*Ct+Cb)/(3*(Ct+Cb));	  %Tp pos
    Cmac(i)=Cb-(Cb-Ct)/b(i)*b_mac(i);				%Chord at tp pos
    
    Cmac(find(isnan(Cmac)))=0;              %if the first wing has fewer partitions
    
    start(i,1)=0.25*Cb+b_mac(i)*tan(SW(i))-0.25*Cmac(i)+sx(i); %Mac start x position
    start(i,2)=cos(dihed(i))*b_mac(i)+sy(i);
    start(i,3)=sin(dihed(i))*b_mac(i)+sz(i);
end

if sym
    start(:,2)=0;
    
end

A=(1+T).*C(1:end-1).*b(1:noofpan-1)./2;							%Area of partitions

C_mac=sum(Cmac.*A)./sum(A);                         % Area weighing for multiple
mac_start_coo(1)=sum((start(:,1).*A')./sum(A));   	% partition wing
mac_start_coo(2)=sum((start(:,2).*A')./sum(A));
mac_start_coo(3)=sum((start(:,3).*A')./sum(A));   	% partition wing% partition wing
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [C,Vor,N,P,Cpar,Vorpar,Pppar]=...
    geometry19(fnx,ny,nx,fsym,fc,flapped,TW,foil...
    ,T,SW,c,dihed,b,sym,sx,sy,sz,meshtype)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GEOMETRY: Essential function for TORNADO				 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determines the position of vortex-collocation-normals	 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Author:	Tomas Melin, KTH, Division of Aeronautics	 %
%				copyright 2000							 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONTEXT:	Subsidary function for TORNADO				 %
% Called by:	setup									 %
% Calls:			MATLAB 5.2 std fcns, tmesh, drawhinge%
%					slope, normals						 %
% Loads:	none										 %
% Saves: none											 %
% Input: wing and division number						 %
% Output:coordinades for collocationpoints, vorticies and%
% 			Normals										 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

TEP=[];
TEP1=[];
TEP2=[];
INF=[];
INF1=[];
INF2=[];

ox=sx;
oy=sy;
oz=sz;
neqns=(nx+fnx)*ny;

dx=(c*(1-fc)/nx);
if flapped==1
    fdx=(c*fc/fnx);
else
    fdx=0;
end
a1=ones(nx,1)*dx;
a2=ones(fnx,1)*fdx;

dr=[a1' a2'];

%%%%%%%%%%%%%%%%%%%%%%%
%Calculates geometry, collocation points
%panels and vortices for a flat quad
%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
% Plotting planform
%%%%%%%%%%%%%%%%%%

lem(1)=0.25*c;
lem(2)=0.25*T*c;
lem(3)=-0.75*T*c;
lem(4)=-0.75*c;

DX=[(1-cos(TW(1,1,1)))*cos(SW) (1-cos(TW(1,1,2)))*cos(SW)...
    (1-cos(TW(1,1,2)))*cos(SW) (1-cos(TW(1,1,1)))*cos(SW)].*lem;

DY=-[sin(TW(1,1,1))*sin(dihed)*cos(SW) sin(TW(1,1,2))*sin(dihed)*cos(SW)...
    sin(TW(1,1,2))*sin(dihed)*cos(SW) sin(TW(1,1,1))*sin(dihed)*cos(SW)].*lem;

DZ=[sin(TW(1,1,1))*cos(dihed) sin(TW(1,1,2))*cos(dihed) sin(TW(1,1,2))*cos(dihed)...
    sin(TW(1,1,1))*cos(dihed)].*lem;

wingx=[0 0.25*c+b*tan(SW)-0.25*T*c 0.25*c+b*tan(SW)+0.75*T*c c]+ox+DX;
wingy=[0 b*cos(dihed) b*cos(dihed) 0]+oy+DY;
wingz=[0 b*sin(dihed) b*sin(dihed) 0]+oz+DZ;

%%%%%%%%%%%%%%%%%
%  create hinge %
%%%%%%%%%%%%%%%%%
if flapped==1
    [flapx flapy flapz]=drawhinge(wingx,wingy,wingz,fc);
end

%figure(2) %UNCOMMENT THESE ROWS TO DRAW WING OUTLINES
%	plot3(wingx,wingy,wingz);
%	hold on
%if sym==1
%plot3(wingx,-wingy,wingz);
%end
%hold on
%axis equal

if flapped==0
    [p,rpar]=tmesh2(wingx,wingy,wingz,nx,ny,meshtype);
    PX(:,:)=p(:,:,1);
    PY(:,:)=p(:,:,2);
    PZ(:,:)=p(:,:,3);
else
    % JO 1701
    c1 = (flapx(2)-wingx(2))/(wingx(3)-wingx(2));
    c2 = (wingx(3)-flapx(2))/(wingx(3)-wingx(2));
    tempx=wingx(3:4);
    tempy=wingy(3:4);
    tempz=wingz(3:4);
    
    wingx(3:4)=fliplr(flapx(1:2));
    wingy(3:4)=fliplr(flapy(1:2));
    wingz(3:4)=fliplr(flapz(1:2));
    
    flapx(3:4)=tempx;
    flapy(3:4)=tempy;
    flapz(3:4)=tempz;
    
    [p,pparw]=tmesh2(wingx,wingy,wingz,nx,ny,meshtype);
    [q,pparf]=tmesh2(flapx,flapy,flapz,fnx,ny,meshtype);
    
    pparw(:,:,1) = pparw(:,:,1)*c1;
    pparf(:,:,1) = c1 + c2*pparf(:,:,1);
    rpar = [];
    %---
    r=[];
    for i=1:ny
        count1=((1:nx)+(nx*(i-1)));
        count2=(1:fnx)+(fnx*(i-1));
        r     =[r;p(count1,:,:);q(count2,:,:)];
        % JO 1701
        rpar  =[rpar;pparw(count1,:,:);pparf(count2,:,:)];
        %---
    end
    PX(:,:)=r(:,:,1);
    PY(:,:)=r(:,:,2);
    PZ(:,:)=r(:,:,3);
end
nx=nx+fnx;

%%%%%%%%%%%%%%%%%%%
%Panel plot.
%Collocation point tensor generation & plot.
%Vortex tensor generation & plot.
%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Loop over all panels on quad. Determines panel corners, %
% vortex coo-rds, and collocation coords		          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[X_1_S,lemma_1_S_tot]=slope2(foil(1,1,1)); %element inboard camber slope

[X_2_S,lemma_2_S_tot]=slope2(foil(1,1,2)); %element outboard camber slope
% JO 1702: compute piecewise poly for later pointwise eval.
pp1 = interp1(X_1_S,lemma_1_S_tot,'pchip','pp');
pp2 = interp1(X_2_S,lemma_2_S_tot,'pchip','pp');
t=0;
for j=0:(ny-1);
    for i=0:(nx-1);
        t=t+1;
        
        px=PX(t,:);
        py=PY(t,:);
        pz=PZ(t,:);
        Ppar = squeeze(rpar(t,:,:));
        
        if i==(nx-fnx-1) %if the panel is the rearest chordwise on wing, forward of flap.
            for s=0:(nx-fnx-1);
                HP(t-s,1,:)=[px(4) py(4) pz(4)];
                HP(t-s,2,:)=[px(3) py(3) pz(3)];
                HPpar(t-s,1,:) = Ppar(4,:);
                HPpar(t-s,2,:) = Ppar(3,:);
                % TEP=Trailing edge points, Vortex points on the trailing edge
                if sym==1
                    %Port side points
                    HP(t-s+neqns,1,:)=[px(3) -py(3) pz(3)];
                    %JO1701 - bug,
                    %       HP(t-s+neqns,1,:)=[px(4) -py(4) pz(4)];
                    %                    ^
                    HP(t-s+neqns,2,:)=[px(4) -py(4) pz(4)];
                    % JO1701
                    
                    HPpar(t-s+neqns,1,:) = Ppar(3,:).*[1 -1];
                    HPpar(t-s+neqns,2,:) = Ppar(4,:).*[1 -1];
                end
            end
        end
        
        if i==(nx-1);		%if the panel is the rearest chordwise on both wing and flap
            for s=0:(nx-1);
                TEP1(t-s,1,:)=[px(4) py(4) pz(4)];
                TEP1(t-s,2,:)=[px(3) py(3) pz(3)];
                % JO1701
                TEP1par(t-s,1,:) = Ppar(4,:);
                TEP1par(t-s,2,:) = Ppar(3,:);
                % TEP=Trailing edge points, Vortex points on the trailing edge
                if sym==1
                    %Port side points
                    TEP1(t-s+neqns,1,:)=[px(3) -py(3) pz(3)];
                    TEP1(t-s+neqns,2,:)=[px(4) -py(4) pz(4)];
                    % JO1701
                    TEP1par(t-s+neqns,1,:) = Ppar(3,:).*[1 -1];
                    TEP1par(t-s+neqns,2,:) = Ppar(4,:).*[1 -1];
                end
                
                for u=0:(fnx-1)	%Hinge points for flap (equals trailing points)
                    HP(t-u,1,:)=[px(4) py(4) pz(4)];
                    HP(t-u,2,:)=[px(3) py(3) pz(3)];
                    % JO1701
                    HPpar(t-u,1,:) = Ppar(4,:);
                    HPpar(t-u,2,:) = Ppar(3,:);
                    if sym==1
                        %Port side points
                        HP(t-u+neqns,1,:)=[px(3) -py(3) pz(3)];
                        HP(t-u+neqns,2,:)=[px(4) -py(4) pz(4)];
                        % JO1701
                        HPpar(t-u+neqns,1,:) = Ppar(3,:).*[1 -1];
                        HPpar(t-u+neqns,2,:) = Ppar(4,:).*[1 -1];
                    end
                end
            end
        end
        
        %%%%%%%%%%%%%%%%%%%%%%%
        % Collocation point   %
        % tensor generation   %
        %%%%%%%%%%%%%%%%%%%%%%%
        
        pp = [px',py',pz'];
        C1(t,:)    = 0.125*(pp(1,:)+pp(2,:)) + 0.375*(pp(3,:)+pp(4,:)); % starboard coll pt coord
        C1par(t,:) = 0.125*(Ppar(1,:)+Ppar(2,:)) + 0.375*(Ppar(3,:)+Ppar(4,:));
        if sym==1
            C2(t,:) = C1(t,:).*[1 -1 1];       % port coll pt coord
            C2par(t,:) = C1par(t,:).*[1 -1];
        else
            C2=[];
            C2par = [];
        end
        
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %Vortex tensor generation and plot %
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
        V1(t,1,:)    = 0.75*pp(1,:)+0.25*pp(4,:);
        V1(t,2,:)    = 0.75*pp(2,:)+0.25*pp(3,:);
        V1par(t,1,:) = 0.75*Ppar(1,:)+0.25*Ppar(4,:);
        V1par(t,2,:) = 0.75*Ppar(2,:)+0.25*Ppar(3,:);
        if sym==1;
            V1(t+neqns,2,:)    =  V1(t,1,:);
            V1(t+neqns,2,2)    = -V1(t+neqns,2,2);
            V1par(t+neqns,2,1) =  V1par(t,1,1);
            V1par(t+neqns,2,2) = -V1par(t,1,2);
            V1(t+neqns,1,:)    =  V1(t,2,:);
            V1(t+neqns,1,2)    = -V1(t+neqns,1,2);
            V1par(t+neqns,1,1) =  V1par(t,2,1);
            V1par(t+neqns,1,2) = -V1par(t,2,2);
        end
        
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % Passus to compute camber slope at %
        % Station							%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
        a3=(sum(dr(1:i+1))-0.25*dr(1+i))/c; %determining percent of chord
        %Thanks, Domenico & Giancarlo
        %Tortora at University of Naples
        %for spotting an earlier error
        %here.
        
        %JO1702
        % must work for ny = 1
        %S(t) = (ppval(pp1,a3)*(ny-j-1)+ppval(pp2,a3)*j)/(ny-1);
        S(t) = (ppval(pp1,a3)*(ny-j-1/2)+ppval(pp2,a3)*(j+1/2))/ny;
        
        if sym==1
            S(t+neqns)=S(t);
        end
    end
end

C=[C1;C2];
Cpar = [C1par;C2par];
V=V1;
Vpar   = V1par;
Vor    = [TEP1(:,1,:) HP(:,1,:) V(:,:,:) HP(:,2,:) TEP1(:,2,:)];
Vorpar = [TEP1par(:,1,:) HPpar(:,1,:) Vpar(:,:,:) HPpar(:,2,:) TEP1par(:,2,:)];


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% calculating normals              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

N=normals4(C,V,S);

V=Vor;

Pp1 = squeeze(rpar(:,:,1));
Pp2 = squeeze(rpar(:,:,2));
if sym==1
    PX2(:,1)=PX(:,2);
    PX2(:,2)=PX(:,1);
    PX2(:,3)=PX(:,4);
    PX2(:,4)=PX(:,3);
    PX2(:,5)=PX2(:,1);
    
    PY2(:,1)=PY(:,2);
    PY2(:,2)=PY(:,1);
    PY2(:,3)=PY(:,4);
    PY2(:,4)=PY(:,3);
    PY2(:,5)=PY2(:,1);
    
    PZ2(:,1)=PZ(:,2);
    PZ2(:,2)=PZ(:,1);
    PZ2(:,3)=PZ(:,4);
    PZ2(:,4)=PZ(:,3);
    PZ2(:,5)=PZ2(:,1);
    
    P2par(:,1,1)= rpar(:,2,1);
    P2par(:,1,2)=-rpar(:,2,2);
    
    P2par(:,2,1)= rpar(:,1,1);
    P2par(:,2,2)=-rpar(:,1,2);
    
    P2par(:,3,1)= rpar(:,4,1);
    P2par(:,3,2)=-rpar(:,4,2);
    
    P2par(:,4,1)= rpar(:,3,1);
    P2par(:,4,2)=-rpar(:,3,2);
    
    P2par(:,5,1)= rpar(:,2,1);
    P2par(:,5,2)=-rpar(:,2,2);
    
    PX=[PX;PX2];
    PY=[PY;-PY2];
    PZ=[PZ;PZ2];
    Pp1 = [Pp1;squeeze(P2par(:,:,1))];
    Pp2 = [Pp2;squeeze(P2par(:,:,2))];
end
Pppar(:,:,1) = Pp1;
Pppar(:,:,2) = Pp2;
P(:,:,1)=PX;
P(:,:,2)=PY;
P(:,:,3)=PZ;
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [normal]=normals4(colloc,vortex,C_Slope)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NORMALS: Essential function for TORNADO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function calculates the normals to
% each panel. Two vectors in the plane, the ones between
% the vortex points and the collocation point, defines
% the panel plane. Together with the vortex orientation
% the orientation of the normal is defined.
% Output normals are normalized.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Author:	Tomas Melin, KTH, Department of Aeronautics
%				copyright 2000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONTEXT:	Essential function for TORNADO
% Called by:	setup
% Calls:			trot
%					MATLAB 5.2 std fcns
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
N=[];
step=size(colloc);
[d e f]=size(vortex);
a=e/2;
b=a+1;

for t=1:step	%Looping through panels
    alpha=C_Slope(t);
    
    for s=1:3						%Looping Through Dimensions.
        ra(s)=vortex(t,a,s);
        rb(s)=vortex(t,b,s);
        rc(s)=colloc(t,s);
    end
    r0=rb-ra;
    r0(1)=0;                    %fix to get normals to not point the right way
    r1=rc-ra;
    r2=rc-rb;
    %n=cross(r1,r2);				%Passus to determine normal
    n = [r1(2)*r2(3)-r1(3)*r2(2),...
        r1(3)*r2(1)-r1(1)*r2(3),...
        r1(1)*r2(2)-r1(2)*r2(1)];
    nl=sqrt(sum((n.^2),2));     %of panel at collocationpoint.
    R=n/nl;				        %Normalizing normal.
    % JOp 180301
    R2=trot3([0;sign(r0(2));0],R,-alpha);		%rotating wha trot
    N=[N;R2'];
end

normal=N;
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function[x,y,z]=drawhinge(wx,wy,wz,fc)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DRAWHINGE: subsidary function to TORNADO	%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Function that draws the hinge line on		%
% on a wing division. It also returns the 	%
% coordinates on the foremost flap corners	%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Called by: Geometry
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INPUT: WX,WY,WZ = wing cornerpoint coor-
%   		dinates.
%			fc is the percentage of total chord
%			built up by the flap
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OUTPUT: 	graph (in figure (2))
%				flap cornerpoint coor-
%				dinates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if isempty(fc);
    x=[];
    y=[];
    z=[];
    
else
    
    for i=0:1
        a=[wx(1+i) wy(1+i) wz(1+i)];
        b=[wx(4-i) wy(4-i) wz(4-i)];
        r = fc*a+(1-fc)*b;
        R1(i+1,:)=r;
    end
    %plot(R1(:,1),R1(:,2));
    
    x=R1(:,1)';
    y=R1(:,2)';
    z=R1(:,3)';
end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function[p2]=trot3(hinge,p,alpha)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TROT: Auxillary rotation function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rotates point p around hinge alpha rads.%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ref: 	R�de, Westergren, BETA 4th ed,
%			studentlitteratur, 1998
%			pp:107-108
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: 	Tomas Melin, KTH,Department of%
% 				aeronautics, Copyright 2000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Context:	Auxillary function for
%				TORNADO.
% Called by: setrudder, normals
% Calls:		norm (MATLAB std fcn)
%				sin			"
%				cos			"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELP:		Hinge=vector around rotation
%						takes place.
%				p=point to be rotated
%				alpha=radians of rotation
%				3D-workspace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cosa=cos(alpha);
sina=sin(alpha);
% JO1701 - make sure hinge is a column
hinge = hinge(:)/(norm(hinge)+0e-17);
Om = [0 -hinge(3) hinge(2); hinge(3) 0 -hinge(1); -hinge(2) hinge(1) 0];
P  = sina*Om + cosa*eye(3)+(1-cosa)*(hinge*hinge');
p2 = P*p';
% or p2 = sina*cross(hinge,p')+ cosa*p' + (1-cosa)*hinge*(hinge'*p')
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [xa,angle]=slope3(foil)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SLOPE: Essential function for TORNADO					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculates the angle of normalrotation due to camber	 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Author:Adrien Berard, KTH, Department of Aeronautics  %
%			              and
%         Tomas Melin, University of Bristol, Aero Dept.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONTEXT:	subsidiary function for TORNADO			     %
% Called by:	geometry								 %
% Calls:		MATLAB 5.2 std fcns						 %
%													     %
%
%  Load: the airfoil data points

% Check type, file or formula...   %TM20070206
disp('THIS MESSAGE SHOULD NOT SHOW. fLattice_setup2, r1032')
if isempty(str2num((cell2mat(foil))))==0
    TYPE=1;       %Naca xxxx profile, see case 1
elseif isempty(str2num((cell2mat(foil))))
    TYPE=2;       %Airfoil from file, see case 2
else
    disp('Foil error, flatplate assumed')
    xa    = [0 0 0];
    angle = [0 0 0];
    return
end

% First type
switch TYPE
    case 1
        %The Airfoil camber can be described as a function, NACA 4 digits
        foil=str2num(cell2mat(foil));
        m=fix(foil/1000);	%gives first NACA-4 number
        lemma=foil-m*1000;
        p=fix(lemma/100);	%gives second NACA-4 number
        
        p=p/10;
        m=m/100;
        xa=0:0.01:1;
        
        for i=1:101
            if xa(i)<p
                %a(i)=m/(p^2)*xa(i)*(2*p-xa(i));
                ap(i) = m/(p^2)*(2*p-2*xa(i));
            else
                %a(i)= m/((1-p)^2)* ((1-2*p)+2*p*xa(i)-xa(i)^2);
                ap(i) = m/((1-p)^2)*(2*p-2*xa(i));
            end
        end
        angle=atan(ap);
        return
        
        % Second Type
    case 2
        % Load the airfoil data points
        settings=config('startup');
        cd(settings.afdir)
        A=load(char(foil));
        cd(settings.hdir)
        
        % Take the number of data points in the data file
        Nu=A(1,1); % for the upper surface
        Nl=A(1,2);  % for the lower surface
        
        %Upper surface
        Xu = A(2:Nu+1,1)/A(Nu+1,1); %% It is divided by A(L+1,1), which is the max absciss of the aifoil, in order to normalize the airfoil to a chord c=1
        Yu = A(2:Nu+1,2)/A(Nu+1,1);
        
        % Lower surface
        Xl = A(Nu+2:end,1)/A(Nu+1,1);
        Yl = A(Nu+2:end,2)/A(Nu+1,1);
        
        Xc(1)=1/2*(Xu(1)+Xl(1));
        Yc(1)=1/2*(Yu(1)+Yl(1));
        
        N1=101;
        Lnorm(:,1)=zeros(Nl,1);
        Lnorm1(:,1)=zeros(N1,1);
        Index=2;
        
        for i=2:Nu-1;
            
            norm=double([Xu(i).*ones(Nl,1)-Xl    Yu(i).*ones(Nl,1)-Yl]);
            Nnorm=double(sqrt(sum(norm.^2,2)));
            if Nnorm==0
                Nnorm=1;
            end
            camber=double([1/2.*(Xu(i).*ones(1,Nl)+Xl') - Xc(i-1).*ones(1,Nl)   ;  1/2.*(Yu(i).*ones(1,Nl)+Yl') - Yc(i-1).*ones(1,Nl)]);
            Ncamber=double(sqrt(sum(camber.^2,1)));
            if Ncamber==0
                Ncamber=1;
            end
            
            perp=double((norm)*(camber./[Ncamber ; Ncamber]));
            
            D=diag(perp);
            
            while (1/2*(Xu(i)+Xl(Index))<=Xc(i-1))
                Index=Index+1;
            end
            
            [void,Ic]=min(Nnorm(Index:end));
            %     [void,Ic]=min(D(Index:end));
            
            
            if Index+Ic<=Nl
                for j=1:N1
                    xintl(j)=Xl(Index+Ic-2) + (j-1)/(N1-1)*(Xl(Index+Ic+0)-Xl(Index+Ic-2));
                end
            else
                for j=1:N1
                    xintl(j)=Xl(Index+Ic-2) + (j-1)/(N1-1)*(Xl(Index+Ic-1)-Xl(Index+Ic-2));
                end
            end
            
            yintl=interp1(Xl,Yl,xintl,'spline');
            
            
            norm1=[Xu(i).*ones(N1,1)-xintl'   Yu(i).*ones(N1,1)-yintl'];
            camber1=[1/2.*(Xu(i).*ones(1,N1)+xintl) - Xc(i-1).*ones(1,N1)   ;  1/2.*(Yu(i).*ones(1,N1)+yintl) - Yc(i-1).*ones(1,N1)];
            Ncamber1=sqrt(sum(camber1.^2,1));
            perp1=double((norm1)*(camber1./[Ncamber1 ; Ncamber1]));
            
            
            D1=diag(perp1);
            Index1 = 2;
            stop=0;
            while (1/2*(Xu(i)+xintl(Index1))<=Xc(i-1))
                Index1=Index1+1;
                if Index1>=N1
                    stop=1;
                end
            end
            norm1Prec=[Xu(i-1)-Xc(i-1)   Yu(i-1)-Yc(i-1)];
            Nnorm1Prec=sqrt(sum(norm1Prec.^2,2));
            if Nnorm1Prec==0
                Nnorm1Prec=1;
            end
            camber1Prec=camber1;
            Ncamber1Prec=sqrt(sum(camber1Prec.^2,1));
            perp1Prec=double((norm1Prec)*(camber1./[Ncamber1 ; Ncamber1]));
            
            
            [void,Ic1]=min(abs(D1(Index1:end))+abs(perp1Prec(Index1:end)'));
            
            
            xlow=xintl(Index1+Ic1-1);
            ylow=yintl(Index1+Ic1-1);
            
            
            Xc(i)=1/2*(Xu(i)+xlow);
            Yc(i)=1/2*(Yu(i)+ylow);
            
            
            Index=Index+Ic+floor((Index1+Ic1-2*N1/4)/N1);
            
            xa(i-1)=Xc(i);
            
        end
        
        
        Xc(Nu)=1/2*(Xu(Nu)+Xl(Nl));
        Yc(Nu)=1/2*(Yu(Nu)+Yl(Nl));
        
        for i=2:Nu-1
            angle(i-1)=atan( (Yc(i)-Yc(i-1)) / (Xc(i)-Xc(i-1)) );
        end
        
        
        
end
end %function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

function [xa,angle]=slope2(foil)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SLOPE: Essential function for TORNADO					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculates the angle of normalrotation due to camber	 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Author:Adrien Berard, KTH, Department of Aeronautics  %
%			              and
%         Tomas Melin, University of Bristol, Aero Dept.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONTEXT:	subsidiary function for TORNADO			     %
% Called by:	geometry								 %
% Calls:		MATLAB 5.2 std fcns						 %
%													     %
%
%  Load: the airfoil data points

% Check type, file or formula...   %TM20070206
%disp('flat2 1246 slope2')

if isnumeric(foil{1}) % already a Selig format foil array
    'foilarray in slope2'
    A = foil{1};
else
    foilch = char(foil{1});
    %Airfoil from file
    try
        tmp = config2('getdir');
        A   = foilfileread([tmp.afdir filesep foilch]);
        % note: naca 4 and 5 points obtained form a formula
    catch
        %  disp([' Airfoil file missing?: ' foilch ' flat assumed'])
        xa=0:0.01:1;
        angle=zeros(size(xa));
        return
    end
end
% returned foil format is Selig translate into Lednicer
atmp = A;
npa  = size(atmp,1);
A    = zeros(npa+2,2);
xmin = min(atmp(:,1));
iu   = find(atmp(:,1)==xmin);
A(2:iu+1,:)   = flipud(atmp(1:iu,:));
A(iu+2:end,:) = atmp(iu:end,:);
A(1,1)= iu; A(1,2) = npa-A(1,1);

% Create evenly distributed abscissae
N=101;
X = linspace(0,1,N);
% Take the number of data points in the data file
L=A(1,1);

%Upper surface
Xu = A(2:L+1,1)/A(L+1,1); %% It is divided by A(L+1,1), which is the max absciss of the aifoil, in order to normalize the airfoil to a chord c=1
Yu = A(2:L+1,2)/A(L+1,1);

% Interpolate to get all the points of the upper surface at known X
% coordinates that will be the same abscisses for lower and upper surfaces
Yiu = interp1(Xu,Yu,X,'pchip','extrap');

% Lower surface
Xl = A(L+2:end,1)/A(end,1);
Yl = A(L+2:end,2)/A(end,1);

% Interpolate the lower surface at the X abcisses
Yil = interp1(Xl,Yl,X,'pchip','extrap');

% Compute the camber vector
C     = 1/2*(Yiu+Yil);
xa    = 1/2*(X(1:end-1)+X(2:end));
angle = atan(diff(C)./diff(X));
end %Function

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [panel,par] = tmesh2(wx,wy,wz,nx,ny,meshtype)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TMESH: Essential function for TORNADO						%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                           %
% 	tmesh generated vertex points for						%
%	quadrilateral wing division given input arguments	    %
%	division corners, numbers of panels in 					%
%	x- and y-direction										%
%															%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Author:	Tomas Melin, KTH, Division of Aeronautics		%
%				2000										%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONTEXT:	Essential function for TORNADO					%
% Called by:	geometry									%
% Calls:			MATLAB 5.2 std fcns						%
%															%
% Loads: None												%
% Saves: none												%
% Input: wing division corners, nuber of elements in 		%
%			x- n' y-direction								%
% Output:Panel corner coordinates (nx5x3) Matrix			%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a1=[wx(1) wy(1) wz(1)];
b1=[wx(2) wy(2) wz(2)];

b2=[wx(3) wy(3) wz(3)];
a2=[wx(4) wy(4) wz(4)];

percent_cy=(0:ny)./ny;
percent_cx=(0:nx)./nx;

switch meshtype
    case 1
        %Linear lattice, both in x and y
    case 2
        %Linear in x, half cosine in y
        percent_cy=cos(pi/2*(1-percent_cy));
    case 3
        %Cosine in x, half cosine in y
        percent_cx=(cos(pi*(1-percent_cx))+1)/2 ;
        percent_cy=cos(pi/2*(1-percent_cy));
    case 4
        %Cosine in x, cosine in y
        percent_cx=(cos(pi*(1-percent_cx))+1)/2;
        percent_cy=(cos(pi*(1-percent_cy))+1)/2;   
    case 5   %Linear in x, cosine in y
        percent_cy=(cos(pi*(1-percent_cy))+1)/2;    
    case 6   % Cosine in x, Linear in y
        percent_cx=(cos(pi*(1-percent_cx))+1)/2; %(Added 22/08/2008 AT)  
    case 7
        %secret, hush-hush ground effect special mission mesh.
        %percent_cx=1.7*percent_cx.^3 - 2.6*percent_cx.^2 + 1.9*percent_cx + 0;
        %percent_cy=1.7*percent_cy.^3 - 2.6*percent_cy.^2 + 1.9*percent_cy + 0;
        percent_cx=2.2*percent_cx.^3 - 3.3*percent_cx.^2 + 2.1*percent_cx;
        percent_cy=2.2*percent_cy.^3 - 3.3*percent_cy.^2 + 2.1*percent_cy;
        
    otherwise
        disp('NOT IMPLEMENTED')
        %Put new functione here for panel distribution scheme.
end

c1=b1-a1;
c2=b2-a2;
A = zeros(ny+1,nx+1,3);
for i=1:ny+1
    perc_y=percent_cy(i);
    m  = a1+perc_y*c1;
    n  = a2+perc_y*c2;
    c3 = n-m;
    for j=1:nx+1
        A(i,j,:)=m+percent_cx(j)*c3;
    end
end

t=0;
panel = zeros(nx*ny,5,3);
par   = zeros(nx*ny,5,2);
for i=1:ny
    pcyi   = percent_cy(i);
    pcyip1 = percent_cy(i+1);
    for j=1:nx
        pcxj   = percent_cx(j);
        pcxjp1 = percent_cx(j+1);
        t=t+1;
        panel(t,1,:)=A(i  ,j  ,:);
        panel(t,2,:)=A(i+1,j  ,:);
        panel(t,3,:)=A(i+1,j+1,:);
        panel(t,4,:)=A(i  ,j+1,:);
        panel(t,5,:)=A(i  ,j  ,:);
        par(t,1,:) = [pcxj  ,pcyi  ];
        par(t,2,:) = [pcxj  ,pcyip1];
        par(t,3,:) = [pcxjp1,pcyip1];
        par(t,4,:) = [pcxjp1,pcyi  ];
        par(t,5,:) = [pcxj  ,pcyi  ];
    end
end
warning('on')
end%function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



