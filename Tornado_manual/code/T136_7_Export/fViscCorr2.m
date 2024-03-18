function [outdata]=fViscCorr(geo,state,lattice,results,ref);

clalpha=2*pi;


[rho,a,p,mu]=ISAtmosphere(state.ALT);

Re=rho*state.AS*ref.C_mac/mu;


p_dyn=(state.rho*state.AS^2)/2;                     %Dynamic Pressure,

[a b]=size(geo.ny);                                 %number of partitions    

for i=1:b
    spp(:,i)=(geo.ny(:,i)).*(geo.symetric+1)'   ;   %strips per partition
end


[lc lb sf]=fLocal_chord3(geo,lattice);              %Local chords on each strip.
                                                    %Local strip width.
                                                    %Span Fraction, how far out on panel the strip is. 

ls=lc.*lb;                                          %Strip area
                                                    
F0=fStripforce(geo,results,lattice,state);          %This is the lift force on each panel
                                    
m=0;
for i=1:a                                       %% Per wing loop
    for j=1:geo.nelem(i)                        %% Per partition loop
        Pinner=profilegen(geo.foil(i,j,1));
        Pouter=profilegen(geo.foil(i,j,2));
        
        alpha=0;
        for k=1:spp(i,j)      %Strip
            m=m+1;
            Pmix_yu=Pinner(:,2)*(1-sf(m))+Pouter(:,2)*(sf(m));
            Pmix_yl=Pinner(:,3)*(1-sf(m))+Pouter(:,3)*(sf(m));
            
            [a void]=size(Pmix_yu);
            [b void]=size(Pmix_yl);
            
            Pmix_yl(end)=Pmix_yu(end); %closing TE gap
            
            Z=[[a b];[(0:0.01:1)' Pmix_yu];[(0:0.01:1)' Pmix_yl]]; %Airfoil file format
            
            delta_alpha=10;
            %Converge towards prescribed cl
            while abs(delta_alpha)>0.0001;
                [strip]=fPablo(Z,alpha*180/pi,Re);
                
                cl=strip.cl; 
                cl_inv=F0(m)/(p_dyn*ls(m));
            
                delta_cl=cl-cl_inv;
                delta_alpha=-delta_cl/clalpha;
            
                alpha=alpha+delta_alpha;
                cd=strip.cd;
                
                
                
            end
            
            outdata.Striplift(m)=cl*(p_dyn*ls(m));
            outdata.Stripdrag(m)=cd*(p_dyn*ls(m));
            outdata.Stripalpha(m)=alpha;
            outdata.upperbl(m,:)=strip.upperbl;
            outdata.lowerbl(m,:)=strip.lowerbl;

            
        end 
        
            %Not finished yet
            %outdata.partitionlift(i,j)=sum(outdata.Striplift((end-XXX)) 
            %outdata.partitiondrag(i,j)=       
    end
end

outdata.totalliftcoeff= sum(outdata.Striplift)./(ref.S_ref*p_dyn);
outdata.totalvdragcoeff= sum(outdata.Stripdrag)./(ref.S_ref*p_dyn);

end %Function

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function[lc lb sf]=fLocal_chord3(geo,lattice)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Geometry function 						 			 	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Computes the Local chord at each collocation 
%  point row.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Author: Tomas Melin, KTH, Department of 
%	Aeronautics, copyright 2002				
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Context: Auxillary function for TORNADO
%	Called by: TORNADO spanload            
%	Calls:	None									
%	Loads:	None									
%	Generates:	Local chord vector lc, same 
%  order as colloc, N, and the others
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[a b]=size(geo.ny); %number of partitions   
for i=1:b
    spp(:,i)=(geo.ny(:,i)).*(geo.symetric+1)'   ;%strips per partition
end


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
lb=[];  %Local span vector.


panelchords1=sqrt(sum((lattice.XYZ(:,1,:)-lattice.XYZ(:,4,:)).^2,3)); %inboard
panelchords2=sqrt(sum((lattice.XYZ(:,2,:)-lattice.XYZ(:,3,:)).^2,3)); %outboard
panelchords3=(panelchords1+panelchords2)/2; %Chord of each panel, CAUTION 
                                            %this is really camber line
                                            %length, so not really chord
                                            %for very cambered profiles
                                            
                                            
 
                                    
                                            
%Local chord loop
for i=1:indx1;			%Wing	
   for j=1:indx2;		%Partition
      lemma=[];  %local chord lemma vector.

      
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

%Local span loop
 A1=((lattice.XYZ(:,1,:)-lattice.XYZ(:,2,:)));
 panelspan=sqrt(A1(:,:,2).^2+A1(:,:,3).^2); %span of each panel

knx=geo.nx+geo.fnx; %total number of chordwise panels
m=0; 
for i=1:indx1;              %Wing	
   for j=1:indx2;           %Partition      
      for k=1:spp(i,j)      %Strip 
          m=m+1;              
          lb(m)=panelspan(1);
          panelspan=panelspan((knx(i,j)+1):end);
          
      end          
   end
end


panelmid=sqrt(sum((lattice.COLLOC(:,2:3).^2),2));
panelmin=sqrt(sum((lattice.XYZ(:,1,:).^2),3));
panelmax=sqrt(sum((lattice.XYZ(:,2,:).^2),3));

sf=[];

for i=1:indx1;              %Wing	
   for j=1:indx2;           %Partition
     
       
       
       m=0;
      for k=1:spp(i,j)      %Strip 
          m=m+1;                      
          stripmid(m)=panelmid(1);
          stripmax(m)=max([panelmin(1) panelmax(1)]);
          
          
          panelmid=panelmid((knx(i,j)+1):end);
          panelmin=panelmin((knx(i,j)+1):end);
          panelmax=panelmax((knx(i,j)+1):end);
          
          
      end
      
      sf=[sf stripmid/max(stripmax)];
      stripmid=[];
      stripmax=[];
      m=0;
      
   end
end


end%function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [profile]=profilegen(foil)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SLOPE: Essential function for TORNADO					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculates the angle of normalrotation due to camber	 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Author:Adrien Bérard, KTH, Department of Aeronautics  %
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

%% Check type, file or formula...   %TM20070206
%% Updated function to give wing profile as outdata:



if isempty(str2num((cell2mat(foil))))==0
    TYPE=1;       %Naca xxxx profile, see case 1 
elseif isempty(str2num((cell2mat(foil))))
    TYPE=2;       %Airfoil from file, see case 2  
else
    disp('Foil error, flatplate assumed')
    xa=[0 0 0];
    angle=[0 0 0];
    return
end

%% First type
switch TYPE
    case 1
    %The Airfoil camber can be described as a function, NACA 4 digits    
        foil=str2num(cell2mat(foil));
        m=fix(foil/1000);	%gives first NACA-4 number
        lemma=foil-m*1000;
        p=fix(lemma/100);	%gives second NACA-4 number
        
        t=(foil-m*1000-p*100)/100;
   
        p=p/10;
        m=m/100;
        
       
   
        
   xa=0:0.01:1;     
   
   for i=1:101
                if xa(i)<p
                 a(i)=(m/(p^2)*xa(i)*(2*p-xa(i)));  
                else
                  a(i)= m/((1-p)^2)* ((1-2*p)+2*p*xa(i)-xa(i)^2);  
                end
   end
   angle=atan(diff(a)./diff(xa));
   
   angle=[angle angle(end)];
   
   
   yt=t*5*(0.2969.*xa.^0.5   -0.1260.*xa   -0.3516.*xa.^2   +0.2843.*xa.^3   -0.1015.*xa.^4);
   
   xu=xa-yt.*sin(angle);
   yu=a+yt.*cos(angle);  
   xl=xa+yt.*sin(angle);
   yl=a-yt.*cos(angle);
   
   yui=interp1(xu,yu,xa);
   yli=interp1(xl,yl,xa);
   
   
   
   profile=[[xa' yui' yli']];
   
   
   
 
   
   return
    
    
    
    
    
%% Second Type        
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

xa=0:0.01:1;
yui=interp1(Xu,Yu,xa,'spline');
yli=interp1(Xl,Yl,xa,'spline');

profile=[[xa' yui' yli']];

return
end
end %function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [F0]=fStripforce(geo,results,lattice,state)
%This lemma function computes the aerodynamic force on each strip.

IL=spanload7(results,geo,lattice,state);             %This is the Inviscous_Lift on each panel.

[ai bi]=size(geo.nx);                                %number of wings and panels
cnx=geo.nx+geo.fnx;                                  %corrected number of xpanels


for i=1:geo.nwing;
    cny(i,:)=geo.ny(i,:).*(geo.symetric(i)+1); %corrected number of ypanels
end
    
m=0;
for i=1:ai          %loop per wing
    for j=1:bi      %loop per partition
        %Per partition, do:
        %Add up all forces on each strip
        index1=1;
        index2=cnx(i,j);
        
        for k=1:cny(i,j)  
            %per strip loop
            m=m+1;
            F0(m)=sum(IL(index1:index2)); 
            index1=index1+cnx(i,j);
            index2=index2+cnx(i,j);
        end
    end
end
end %function stripforce

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [forceLift]=spanload7(results,geo,lattice,state)
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
%	Calls:	None									%
%	Loads:	None									%
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
   										          %Aligned witn panel normals
                                                 
    lemma4(i,:)=B2WTransform*results.F(i,:)';                                         
	forceLift(i)=lemma4(i,3);                     %Lift on each panel, this is outdata for the
                                                  %viscous correction.
end




end%function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%













