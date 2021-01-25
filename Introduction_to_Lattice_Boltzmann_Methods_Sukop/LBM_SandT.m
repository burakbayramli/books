% A short and simple gravity-driven LBM solver based on the code snippets 
% in Sukop and Thorne's 'Lattice Boltzmann Modeling'

% Note indexing differences between book's C code and MATLAB: 
% C uses 0 for the first index value, while MATLAB starts at one.
% Numerous changes are needed. In some places, I have just 
% explicitly added one to the C index.

close('all');clear('all')

display('initialize')

LY=11
LX=3
tau = 1
g=0.001

%set solid nodes at walls on top and bottom
is_solid_node=zeros(LY,LX);
for i=1:LX
    is_solid_node(1,i)=1;
    is_solid_node(LY,i)=1;
end

display('solid nodes')
is_solid_node

%define initial density and fs
rho=ones(LY,LX);

f(:,:,1) = (4./9. )*rho;
f(:,:,2) = (1./9. )*rho;
f(:,:,3) = (1./9. )*rho;
f(:,:,4) = (1./9. )*rho;
f(:,:,5) = (1./9. )*rho;
f(:,:,6) = (1./36.)*rho;
f(:,:,7) = (1./36.)*rho;
f(:,:,8) = (1./36.)*rho;
f(:,:,9) = (1./36.)*rho;

display('intitial f')

f;

%define lattice velocity vectors

ex(0+1)= 0; ey(0+1)= 0;
ex(1+1)= 1; ey(1+1)= 0;
ex(2+1)= 0; ey(2+1)= 1;
ex(3+1)=-1; ey(3+1)= 0;
ex(4+1)= 0; ey(4+1)=-1;
ex(5+1)= 1; ey(5+1)= 1;
ex(6+1)=-1; ey(6+1)= 1;
ex(7+1)=-1; ey(7+1)=-1;
ex(8+1)= 1; ey(8+1)=-1;


for ts=1:300 %Time loop
    
    ts
    
    % Computing macroscopic density, rho, and velocity, u=(ux,uy).
    for j=1:LY 
        
        for i=1:LX
            
            u_x(j,i) = 0.0;
            u_y(j,i) = 0.0;
            rho(j,i) = 0.0;
            
            if ~is_solid_node(j,i)
                
                for a=0:8
                    
                    rho(j,i) = rho(j,i) + f(j,i,a+1);
                                       
                    u_x(j,i) = u_x(j,i) + ex(a+1)*f(j,i,a+1);
                    u_y(j,i) = u_y(j,i) + ey(a+1)*f(j,i,a+1);
                    
                end
                
                u_x(j,i) = u_x(j,i)/rho(j,i);
                u_y(j,i) = u_y(j,i)/rho(j,i);
                
            end
            
            %add space matricies for plotting
            x(j,i)=i;
            y(j,i)=j;
            
        end
    end
    
       
    % Compute the equilibrium distribution function, feq.
    f1=3.;
    f2=9./2.;
    f3=3./2.;
    
    for j=1:LY
        
        for i=1:LX
            
            if ~is_solid_node(j,i)
                
                rt0 = (4./9. )*rho(j,i);
                rt1 = (1./9. )*rho(j,i);
                rt2 = (1./36.)*rho(j,i);
                ueqxij =  u_x(j,i)+tau*g; %add forcing
                ueqyij =  u_y(j,i);
                uxsq   =  ueqxij * ueqxij;%changes from book here! See Book's Errata.
                uysq   =  ueqyij * ueqyij;
                uxuy5  =  ueqxij +  ueqyij;
                uxuy6  = -ueqxij +  ueqyij;
                uxuy7  = -ueqxij + -ueqyij;
                uxuy8  =  ueqxij + -ueqyij;
                usq    =  uxsq + uysq;
                
                feq(j,i,0+1) = rt0*( 1.                              - f3*usq);
                feq(j,i,1+1) = rt1*( 1. + f1*ueqxij + f2*uxsq        - f3*usq);
                feq(j,i,2+1) = rt1*( 1. + f1*ueqyij + f2*uysq        - f3*usq);
                feq(j,i,3+1) = rt1*( 1. - f1*ueqxij + f2*uxsq         - f3*usq);
                feq(j,i,4+1) = rt1*( 1. - f1*ueqyij + f2*uysq         - f3*usq);
                feq(j,i,5+1) = rt2*( 1. + f1*uxuy5  + f2*uxuy5*uxuy5 - f3*usq);
                feq(j,i,6+1) = rt2*( 1. + f1*uxuy6  + f2*uxuy6*uxuy6 - f3*usq);
                feq(j,i,7+1) = rt2*( 1. + f1*uxuy7  + f2*uxuy7*uxuy7 - f3*usq);
                feq(j,i,8+1) = rt2*( 1. + f1*uxuy8  + f2*uxuy8*uxuy8 - f3*usq);
                
            end
        end
    end
        
    % Collision step.
    for j=1:LY
        for i=1:LX
            
            if is_solid_node(j,i); 
                
                % Standard bounceback
                
                temp   = f(j,i,1+1); f(j,i,1+1) = f(j,i,3+1); f(j,i,3+1) = temp;
                temp   = f(j,i,2+1); f(j,i,2+1) = f(j,i,4+1); f(j,i,4+1) = temp;
                temp   = f(j,i,5+1); f(j,i,5+1) = f(j,i,7+1); f(j,i,7+1) = temp;
                temp   = f(j,i,6+1); f(j,i,6+1) = f(j,i,8+1); f(j,i,8+1) = temp;
                
            else  
                % Regular collision
                
                for a=1:9
                    
                    f(j,i,a) = f(j,i,a)-( f(j,i,a) - feq(j,i,a))/tau; %1st term rhs was f ?????
                    
                end  
                
            end
        end          
    end
        
    % Streaming step; subtle changes to periodicity here due to indexing
    for j=1:LY
        
        if j>1 
            jn = j-1;
        else
            jn = LY;
        end
        
        if j<LY
            jp = j+1;
        else 
            jp = 1;
        end
        
        for i=1:LX
                  
            if i>1
                in = i-1; 
            else 
                in = LX; 
            end
            if i<LX 
                ip = i+1; 
            else 
                ip = 1; 
            end
            
            ftemp(j,i,0+1)  = f(j,i,0+1);
            ftemp(j,ip,1+1) = f(j,i,1+1);
            ftemp(jp,i,2+1) = f(j,i,2+1);
            ftemp(j,in,3+1) = f(j,i,3+1);
            ftemp(jn,i ,4+1) = f(j,i,4+1);
            ftemp(jp,ip,5+1) = f(j,i,5+1);
            ftemp(jp,in,6+1) = f(j,i,6+1);
            ftemp(jn,in,7+1) = f(j,i,7+1);
            ftemp(jn,ip,8+1) = f(j,i,8+1);
            
        end
    end
    
    f(:,:,:)=ftemp(:,:,:);  
    
end %end time loop

figure
quiver(x,y,u_x,u_y)

width=LY-2

figure
%Model results as red circles
plot(y(:,1)-1.5-width/2,u_x(:,1),'ro') 

hold on

%Poiseuille velocity profile as blue line
nu=1/3*(tau-1/2)
plot(y(:,1)-1.5-width/2,g/(2*nu)*((width/2)^2-(y(:,1)-1.5-width/2).^2)) 