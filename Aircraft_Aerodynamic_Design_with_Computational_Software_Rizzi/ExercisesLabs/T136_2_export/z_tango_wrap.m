function [shout]=z_tango_wrap(geomfile,state,CL_target);

%% INITIALIZE
verbose=1;                      %set for talkative code.              
settings=config('startup');
results=[];                     %Initializing results struct.





%% Check input
if ischar(geomfile)
    cd(settings.acdir)                                     
        load(geomfile)                                         %load aircraft file
    cd(settings.hdir)  
end

if ischar(state)
    cd(settings.sdir)                                     %sure we get the unsteady file    
        load(state)                                         %load aircraft file
    cd(settings.hdir)  
end


%CL_target=0.5;


geo=rip_main_wing(geo);                             %Using only main wing in study
geo=gridconv(geo,state,verbose);                    %Do grid convergence check
state.alpha=0;          
geo=set_incidence(geo,state,CL_target,verbose);     %Set main wing incidence to give target CL at alpha =0;
shout.incidence=geo.TW(1,1,1);

[results,ref,out,geo]=twister(geo,state,verbose);   %Twist the wing to maximize glideslope
shout.twist=geo.TW(1,end,2);
shout.geo=geo;

%%Outputs:
if verbose
    [lattice,ref]=fLattice_setup2(geo,state,1);    %Setting up the lattice
    figure(4)% 	Delta cp plot
    rotate3d on 
    colormap(hot);
    fill3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)',results.cp')
    title('Delta cp distribution')
    colorbar('vert')
    axis equal
end


end





function [geo]=set_incidence(geo,state,CL_target,verbose);
%%This function changes the global twist on the main wing (wing#1) to give
%%target CL at zero alpha.
results=[];
latticetype=1;
state.alpha=0;

[lattice,ref]=fLattice_setup2(geo,state,latticetype);    %Setting up the lattice
[results]=solver9(results,state,geo,lattice,ref);       %Solving for forces
[results]=coeff_create3(results,lattice,state,ref,geo); %computing coefficients
CL0=results.CL;

converged=0;
dTW=0.01;
t=0;
while converged==0
    t=t+1;
    if abs(results.CL-CL_target)<0.0001
        disp('CONVERGED! - Main Wing Incidence set OK.')
        %set incidence here
        if verbose
           geometryplot(lattice,geo,ref);
        end
        break
    end
    
    if t==40
        disp('Maxiter')
        break
    end
    
    geo.TW(1,:,:)=geo.TW(1,:,:)+dTW;
    [lattice,ref]=fLattice_setup2(geo,state,latticetype);    %Setting up the lattice
    [results]=solver9(results,state,geo,lattice,ref);       %Solving for forces
    [results]=coeff_create3(results,lattice,state,ref,geo); %computing coefficients
    
    CL1=results.CL;
    if verbose
        figure(99)
        plot(t,CL1,'*')
        hold on
    end
    
    dCL=CL1-CL0;
    dCL_dTW=dCL/dTW;
    
    dTW=(CL_target-CL1)/dCL_dTW;
    CL0=CL1;
    
end
end


function [geo2]=gridconv(geo2,state,verbose);
lattictype=0;  %Tornado freestream following wake VLM 
gny=geo2.ny;             %Saving original distribution   
%geo2.nx=geo2.nx./geo2.nx;
geo2.ny=geo2.ny./geo2.ny;
%geo2.nx=(1-isnan(geo2.nx))*2;
geo2.ny=(1-isnan(geo2.ny))*2;


%% Check main wing grid convergence



    
    [lattice,ref]=fLattice_setup2(geo2,state,lattictype);    %Setting up the lattice
  
    wing=1;                 % Main wing
    direction=2;            %Spanwise check
    criterion=0.01;        %Convergence criterion

    

    converge=zgridconverge2(geo2,state,wing,direction,criterion);



    if converge.converged
            geo2.ny=converge.ny;
                     
            
            [lattice,ref]=fLattice_setup2(geo2,state,lattictype);    %Setting up the lattice
           
            if verbose    
                disp('Current geometry updated to converged resolution.')
                lattice=[];
            
                h=figure(200);
                set(h,'Position',[100 100 400 400*4/3]);
                subplot(3,1,1)
                title('Convergence history')
                plot(converge.panels,converge.CL,'-o')
                %axis([0 max(converge.panels) 0 max(converge.CL)*2 ])
                xlabel('Number of Panels')
                ylabel('Lift coefficient, C_L,   [-]')
                subplot(3,1,2)
                plot(converge.panels,converge.CD,'-o')
                xlabel('Number of Panels')
                ylabel('Drag coefficient, C_{Di},   [-]')
                subplot(3,1,3)
                plot(converge.panels,converge.Cm,'-o')
                xlabel('Number of Panels')
                ylabel('Pitching moment coefficient, C_m,   [-]')
            end 
    end
end




function [results,ref,out,geo]=twister(geo,state,verbose);
%% Wing Twist distributor - This functions twists the main wing in order to maximize the glideslope at the target CL
old_geo=geo;
latticetype=1;
results=[];           

CL_target=0.5;
dGS_dTW(1)=10;

%geo.nx(1,:)=[5 5 5];
%geo.ny(1,:)=[1 5 5];

[results,state.alpha]=fFindAlphaAtCL(geo,state,latticetype,CL_target);  %Finding trim alpha

[lattice,ref]=fLattice_setup2(geo,state,latticetype);    %Setting up the lattice
[results]=solver9(results,state,geo,lattice,ref);       %Solving for forces
[results]=coeff_create3(results,lattice,state,ref,geo); %computing coefficients

 %geometryplot(lattice,geo,ref);
    if verbose
        figure(11)
        maxspan=max(results.ystation(:,1));
        hold on, grid on
        plot(results.ystation(:,1)./maxspan,results.CL_local(:,1));
        title('Local CL on main wing');
        ylabel('CL');
        xlabel('Spanstation')
        text(0,0,strcat('Total CL ',num2str(results.CL)));
    end
        twisted=0;
        
        tsf=cumsum(geo.b(1,:))./sum(geo.b(1,:)); %twist_span_factor
        
    t=1;  
    %TW=[0 0.0001];
    step=0.01;
    dTW=[0.000 step];   %rad

    GS(t)=results.CL/results.CD;
    realtwist(t)=-((geo.TW(1,1,1)-geo.TW(1,end,2))*57);
while twisted==0
     t=t+1;
     TWS=dTW(t);
     
    geo.TW(1,:,2)=geo.TW(1,:,2)+tsf*TWS;                                   %Distributing the twist on the first wing
    geo.TW(1,2:end,1)=geo.TW(1,1:end-1,2);
    realtwist(t)=-((geo.TW(1,1,1)-geo.TW(1,end,2))*57);
  
    [lattice,ref]=fLattice_setup2(geo,state,latticetype);    %Setting up the lattice
    [results,state.alpha]=fFindAlphaAtCL(geo,state,latticetype,CL_target);
    
    if verbose
        disp(strcat('New alpha for heave trim:',num2str(state.alpha*57),' Degrees'))
        disp(strcat('CL @',num2str(results.CL)))
    end
    
    [lattice,ref]=fLattice_setup2(geo,state,latticetype);    %Setting up the lattice, again.
    [results]=solver9(results,state,geo,lattice,ref);       %Solving for forces
    [results]=coeff_create3(results,lattice,state,ref,geo); %computing coefficients
    

    GS(t)=results.CL/results.CD;   
    dGS=diff(GS);
    dGS_dTW(t)=dGS(end)/dTW(end);
    dTW(t+1)=sign(dGS_dTW(end))*step;     %Version 3
    %dir(t)=sign(dGS_dTW(end));
    

    if verbose
            figure(14)
            plot(realtwist,GS)
            
            figure(13)
            %st=((geo.TW(1,1,1)-geo.TW(1,end,2))*57);
            plot(realtwist)
            hold on
        
            figure(11)
            plot(results.ystation(:,1)./maxspan,results.CL_local(:,1),'r');
            drawnow
    
            figure(12)
            plot(GS)
            drawnow
            disp('******************');
        disp(strcat('New alpha for heave trim:',num2str(state.alpha*57),' Degrees'))
        disp(strcat('CL @',num2str(results.CL)))
        disp(strcat('Wingtwist is: ',num2str(realtwist(t)),' degrees, root to tip.'))
    end
    
    if t==40 %maxiter     
            break
    end
    
    lemma=diff(GS);
    if lemma(end)<0.01
        break
    end
     
     dir=sign(dGS_dTW);
     
     limit=sum(dir)./t;
     if t>10
      if limit<0.8
          break
      end
     end
        
end
 TW=geo.TW(1,end,2);
 %geometryplot(lattice,geo,ref);
 out.twist=TW(end);
 
end



function [geo2]=rip_main_wing(geo);
geo2.ref_point=[0 0 0];
geo2.CG=[0 0 0];
geo2.flapped=geo.flapped(1,:);
geo2.nwing=1;
geo2.nelem=geo.nelem(1);
geo2.symetric=geo.symetric(1);
geo2.startx=0;
geo2.starty=0;
geo2.startz=0;
geo2.dihed=geo.dihed(1,:);
geo2.c=geo.c(1);
geo2.foil(1,:,:)=geo.foil(1,:,:);
geo2.T(1,:)=geo.T(1,:);
geo2.TW(1,:,:)=geo.TW(1,:,:);
geo2.nx=geo.nx(1,:);
geo2.fnx=geo.fnx(1,:);
geo2.ny=geo.ny(1,:);
geo2.b=geo.b(1,:);
geo2.T=geo.T(1,:);
geo2.SW=geo.SW(1,:);
geo2.meshtype=geo.meshtype(1,:);
geo2.fc=geo.fc(1,:);
geo2.fsym=geo.fsym(1,:);
geo2.flap_vector=geo.flap_vector(1,:);

end






%%%%%%%%%%%%%%%%%%%%%%%%%
%Archeologic code below





%Computation start
% [lattice,ref]=fLattice_setup2(geo,state,lattictype);    %Setting up the lattice
% [results]=solver9(results,state,geo,lattice,ref);       %Solving for forces
% [results]=coeff_create3(results,lattice,state,ref,geo); %computing coefficients


































function [out]=check_ellipsillity(results);
%% This function evaluates how elliptic a lift distribution is 

maxspan=max(results.ystation(:,1));

y=results.ystation(:,1)./maxspan;
phi=acos(y);
x=sin(phi);

figure(999)
plot(y,x)
hold on

err=sqrt(((results.CL_local(:,1)-results.CL)).^2);

plot(y,err,'r')

out=sum(err);





end




