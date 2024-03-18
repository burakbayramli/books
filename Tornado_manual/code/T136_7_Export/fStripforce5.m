function [results]=fStripforce(geo,results,lattice,state,ref,vCfraction)
%This lemma function computes the aerodynamic force on each strip.

dynp=0.5*state.rho*state.AS^2;
S=ref.S_ref;

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
        if cny(i,j)>1
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
           Mpoint=(1-vCfraction)*localC1+(vCfraction)*localC2;
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

            F3(m,:)=sum(F(index1:index2,:));
            M3(m,:)=sum(cross(F(index1:index2,:),h));
      
            clear h
            %% Coefficients
            CZprime(m)=F0(m)/(dynp*la);
      
        end        
    end
    [yps or]=sort(yprimestation);
    [a b]=size(yps);
    
    results.load.ypstation(1:b,i)=(yps);
    results.load.stripforce(1:b,i)=F0(or);
    results.load.CZprime(1:b,i)=CZprime(or);
    results.load.forcepermeter(1:b,i)=F0(or)./ls(or);
    results.load.F(1:b,i,1)=F3(or,1)./ls(or)';
    results.load.F(1:b,i,2)=F3(or,2)./ls(or)';
    results.load.F(1:b,i,3)=F3(or,3)./ls(or)';   
    results.load.M(1:b,i,1)=M3(or,1)./ls(or)';
    results.load.M(1:b,i,2)=M3(or,2)./ls(or)';
    results.load.M(1:b,i,3)=M3(or,3)./ls(or)';
   
    clear yprimestation F3 M3 h F0 ls m
    m=0;
end


    return
    
    
    
    
    
    
    
    
    
 out.ypstation=results.load.ypstation;
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
h1=plot(out.ypstation,out.shear);
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
        