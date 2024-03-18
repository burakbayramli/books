function[CD0 Re Swet Vol]=zeroliftdragpred(Mach,Alt,geo,ref)
%zeroliftdragpred, Zero Lift Drag Prediction.
%This function predicts the zero lift drag of an airplane configuration
%
%Component buildup method, Eckerts equation, Raymer formulatation, Melin
%Implementation
%
%--------------------------------------------
%
%Inputs are:
% Tornado variable structures.
% Mach: Mach number, useful range [0 0.7], it will still work up to M=1,
%       but the errors will start to increase
%
% Alt:  Altitude in meters. The atmosphere model used is the 1976 
%       International Standard Atmosphere (ISA).
%
% geo:  Geometry structure at defined in Tornado. However, all airfoil
%       sections MUST be defined by a coordinate file. Plus an interference
%       factor matrix, geo.inter, which defines the interference factors of
%       each partition. If Geo.inter is undefined it will be assumed to be
%       ones, which is fairly close anyway.
%
% ref:  Reference units structure as defined in Tornado.
%
%----------------------------------------------
%
% Outputs:
%
% CD0:  Matrix with zero lift drag for each wing partition in Tornado Standard.
%       First wing centersection drag is set to zero as it is assimed to be
%       inside a fuselage.
%
%                    Partition 1      partition 2    partition 3   ...
%       first wing        0                X              X        ... 
%       second wing       X                X              X        ...
%       third wing        X                X              X        ...
%            :            ...             ...            ...       ...
%
%
% Re
%
%-----------------------------------------------
%
% LIMITATIONS
% 
% Tubulent transition is hardcoded to 10% chord.
% If the maximum thickness to chord ratio is constant across a partition,
% and if it's position moves. The innermost position will be selected.
% Surface roughness is hardcoded to smooth paint.
%

%%%%%%%%%%%%%%%%%%%%%%%
%% Constants definition
ansc=questions(13);       %question string generator function %***
if ansc==0 %*** Currently not used, default case of fixed coefficients used
    %[A b c d]=revdrag14edit4(); %*** Call revdrag14 to run polar breakdown and produce coefficients 
    A=0.455;    %fixed Eckerts equation constants %***
    b=2.58; %***
    c=0.144; %***
    d=0.65; %***    
    
elseif ansc==1 %***
    A=0.455;    %fixed Eckerts equation constants %***
    b=2.58; %***
    c=0.144; %***
    d=0.65; %***
    
else %***
    terror(21) %***
    A=0.455;   %fixed Eckerts equation constants %***
    b=2.58; %***
    c=0.144; %***
    d=0.65; %***
end %***
e=1.053;
k=0.634*10^(-5);                   %HARDCODED, Smooth paint (Raymer table 12.4)
f=1.328;
%%%%%%%%%%%%%%%%%%%%%%%%


[aa bb]=size(geo.nx);               %Getting data size

%%%%%%%%%%%%%%%%%%%%%
%% Computing state
[rho a p mu]=ISAtmosphere(Alt);     %Calling International Standard atmosphere.
V=Mach*a;                           %Computing local TAS 
%%%%%%%%%%%%%%%%%%%%%

%% Wing drag / Lifting Surface Drag!
mgc=meangeometricchords(geo);            %Mean geometric chord of each partition    
[wing.Swet wing.Vol tc xc]=fSwet(geo);   %Areas, Volumes, max thickness of each part.

Re=(rho*V)*ref.C_mac./(mu);                        %Reference reynolds no / Cmac  
wing.Re=(rho*V*mgc)./(mu);               %Reynolds Number for wing
wing.Re_cutoff=38.21*(mgc/k).^e;         %Cutoff Re

for s=1:aa                               %loopning though parts, choosing smallest RE   
    for t=1:bb
        wing.Re_use(s,t)=min([wing.Re(s,t) wing.Re_cutoff(s,t)]);        %Use smallest value
    end
end

warning off
wing.cf_lam=1.328./sqrt((wing.Re_use));
wing.cf_turb=A./(log10(wing.Re_use).^b*(1+c*Mach^2)^d); %wing turbulent skin friction coefficient;
warning on

               

%%%%%%%%%%%%%%%%%%%%%%%
%%Computing Form factors
lambda=geo.SW;
if xc~=0
    wing.FF=(1+0.6./(xc).*(tc)+100*(tc).^4).*(1.34*Mach^0.18*cos(lambda).^0.28); %raymer eq 12:27     
else
    wing.FF=(1.34*Mach^0.18*cos(lambda).^0.28); %Thin, really thin, wing.
end
    
    tpoint=0.1;               %Hardcoded transitionpoint all parts

wing.cf_composite=(1-tpoint)*wing.cf_turb+tpoint*wing.cf_lam;
%wing.cf_composite(1,1)=0; % Wing-fuselage carryover panel.


try  %The interference factor might not be inclided in the geo struct
    QQ=geo.inter;
catch
    QQ=ones(size(wing.FF)); %In which case, set it to one.
end

CD0=wing.cf_composite.*wing.FF.*QQ.*wing.Swet./ref.S_ref;
CD0(isnan(CD0))=0;   %Remove NAN entries
Swet=wing.Swet;
Vol=wing.Vol;
end %Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function[mgc]=meangeometricchords(geo)

loopsperwing=geo.nelem;
noofloops=loopsperwing;
noofwings=size(loopsperwing');


for s=1:noofwings			    %Intermediate variable setuploop
	CHORDS(s,1)=geo.c(s);		%calculating chords of first element
end


for s=1:noofwings
	for t=1:(noofloops(s))
      %Looping trough all quads
      CHORDS(s,t+1)=CHORDS(s,t)*geo.T(s,t);	%calculating
      
      mgc(s,t)=(CHORDS(s,t)+CHORDS(s,t+1))/2;
      
      
      
   end
end

end%Function

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [Swet,Vol,mtc,mtcpos]=fSwet(geo)
%Haven't written anything here yet
settings=config('startup');


loopsperwing=geo.nelem;
noofloops=loopsperwing;
noofwings=size(loopsperwing');


for s=1:noofwings			%Intermediate variable setuploop
	CHORDS(s,1)=geo.c(s);		%calculating chords of first element
end



for s=1:noofwings
	for t=1:(noofloops(s))
      
      %Looping trough all quads
      CHORDS(s,t+1)=CHORDS(s,t)*geo.T(s,t);	%calculating
      
      
      [A B]=getprofcoordinates(geo.foil,s,t);
      
        
        %try
        %cd aircraft
        %cd airfoil
        %    A=load(char(geo.foil(s,t,1))); %inner coordinates 
        %    B=load(char(geo.foil(s,t,2))); %outer coordinates
        %cd ..
        %cd ..
        %catch
        %    disp('Ouuaaah!')
        %    disp('This shouldnt happen, are all your profiles defined with coordinate files? They should be!')
        %end
        
%% Wetted area
        %reorder
        ind=(A(1,1)+2);
        A2=flipud(A(ind:end,:));
        A3=A(2:ind-1,:);
        A4=[A2
            A3];
        
        ind=(B(1,1)+2);
        B2=flipud(B(ind:end,:));
        B3=B(2:ind-1,:);
        B4=[B2
            B3];
        %end reorder
        
        LwetA=sum(sqrt(sum((diff(A4*CHORDS(s,t))).^2,2)));
        LwetB=sum(sqrt(sum((diff(B4*CHORDS(s,t+1))).^2,2)));
            
        Swet(s,t)=(LwetA+LwetB)/2*geo.b(s,t);
        
        if geo.symetric(s)==1
            Swet(s,t)=Swet(s,t)*2;
        end
        
        C_mgc=(CHORDS(s,t)+CHORDS(s,t+1))/2;
        
%% Internal Volume        
       

       
    Area_A=sum((diff(A4(:,1))).*((A4(2:end,2)+A4(1:end-1,2))/2)*CHORDS(s,t)^2);   %Area of innermost section   
    Area_B=sum((diff(B4(:,1))).*((B4(2:end,2)+B4(1:end-1,2))/2)*CHORDS(s,t+1)^2); %Area of outermost section 
    Vol(s,t)=1/3*geo.b(s,t)*(Area_A+Area_B+sqrt(Area_A*Area_B));                  %Volume of a frustum
    
        if geo.symetric(s)==1
            Vol(s,t)=Vol(s,t)*2;
        end
        
%% End internal Volume        
        
%% Max thickness inner airfoil
        N=101;
        for i=1:N
            X(i)=(i-1)/(N-1);
        end

        % Take the number of data points in the data file
        L=A(1,1);

        %Upper surface
        Xu = A(2:L+1,1)/A(L+1,1); % It is divided by A(L+1,1), which is the max absciss of the aifoil, in order to normalize the airfoil to a chord c=1
        Yu = A(2:L+1,2)/A(L+1,1); % Again, normalising (this should alrady have been done in the coordinate file.)                                       
        Yiu = interp1(Xu,Yu,X,'pchip','extrap'); % Interpolate to get all the points of the upper surface at kown X
                                                 % coordinates that will be
                                                 % the same abscisses for lower and upper surfaces
        % Lower surface
        Xl = A(L+2:end,1)/A(L+1,1);
        Yl = A(L+2:end,2)/A(L+1,1);
        Yil = interp1(Xl,Yl,X,'pchip','extrap'); % Interpolate the lower surface at the X abcisses

        tc=Yiu-Yil; %thickness to chord ratio 
        
        [mtci,indx]=(max(tc));
        mtcipos=X(indx);
       
%% Max thickness outer airfoil
        N=101;
        for i=1:N
            X(i)=(i-1)/(N-1);
        end

        % Take the number of data points in the data file
        L=B(1,1);

        %Upper surface
        Xu = B(2:L+1,1)/B(L+1,1); % It is divided by A(L+1,1), which is the max absciss of the aifoil, in order to normalize the airfoil to a chord c=1
        Yu = B(2:L+1,2)/B(L+1,1); % Again, normalising (this should alrady have been done in the coordinate file.)                                       
        Yiu = interp1(Xu,Yu,X,'pchip','extrap'); % Interpolate to get all the points of the upper surface at kown X
                                                 % coordinates that will be
                                                 % the same abscisses for lower and upper surfaces
        % Lower surface
        Xl = B(L+2:end,1)/B(L+1,1);
        Yl = B(L+2:end,2)/B(L+1,1);
        Yil = interp1(Xl,Yl,X,'pchip','extrap'); % Interpolate the lower surface at the X abcisses

        tc=Yiu-Yil; %thickness to chord ratio       
        
        [mtco,indx]=(max(tc));
        mtcopos=X(indx);
        
        mtcc=[mtcipos mtcopos];
        
[mtc(s,t) indx]=max([mtci mtco]);        
mtcpos(s,t)=mtcc(indx);     
    
    end
end
end%function 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [A,B]=getprofcoordinates(foils,s,t)
settings=config('startup');

for k=1:2
    foil=(foils(s,t,k));
    
    if isempty(str2num((cell2mat(foil))))==0
        TYPE=1;       %Naca xxxx profile, see case 1 
    elseif isempty(str2num((cell2mat(foil))))
        TYPE=2;       %Airfoil from file, see case 2  
    end

switch TYPE
    
    case 1
        
            foil=str2num(cell2mat(foil));
            m=fix(foil/1000);	%gives first NACA-4 number
            lemma=foil-m*1000;
            p=fix(lemma/100);	%gives second NACA-4 number
            lemma=(foil-m*1000)-p*100;
            tk=lemma/100;
        
            p=p/10;
            m=m/100;
        
            xa=0:0.01:1;     
   
            for i=1:101
                if xa(i)<p
                 camber(i)=(m/(p^2)*xa(i)*(2*p-xa(i)));  
                else
                  camber(i)= m/((1-p)^2)* ((1-2*p)+2*p*xa(i)-xa(i)^2);  
                end
            end
    
            y=tk/0.2.*(0.2969*sqrt(xa)-0.1260*xa-0.3515*xa.^2+0.2843*xa.^3-0.1015*xa.^4);
    
            theta=atan(diff(camber)./diff(xa));
            yu=camber+y.*[0 cos(theta)];
            yl=camber-y.*[0 cos(theta)];
            xu=xa-y.*[0 sin(theta)];
            xl=xa+y.*[0 sin(theta)];
            
            
            
            if k==1
                 A(:,:)=[101 101;xu' yu';xl' yl'];; %inner coordinates 
            else
                 B(:,:)=[101 101;xu' yu';xl' yl'];; %outer coordinates    
            end
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %
    %
    %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %Coordinate file data
    
    case 2
        try
        cd(settings.afdir)
            if k==1
                 A(:,:)=load(char(foil)); %inner coordinates 
            else
                 B(:,:)=load(char(foil)); %outer coordinates    
            end
           
        cd(settings.hdir)
        end

end%switch
end%outher for loop

%A=(K(:,:,1));
%B=(K(:,:,2));
end%function

