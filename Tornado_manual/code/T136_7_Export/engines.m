function [engine] = engines(geo)
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
% usage: []=engines();
%
% This is a function to allow the user to input the aircraft engine data.
% It requires no input variables from other m-files.
%
% Example:
%
%   []=engines();
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Engine input
disp(' ')
engine.number=input('Please input number of engines: ');
engine.moments=ones(engine.number,3);
geo.ref_point=[0 0 0];

for i=1:1:engine.number
    
    YN = 0;
    disp(' ')
    disp(strcat('*****************Engine No: ',num2str(i),'****************'))
    
    if i > 1
        disp(' ')
        disp('Is engine a symmetrical equivalent of a previous engine?')
        YN=input('Y=1 N=0 ');
    end

    if YN == 1
        engsym=input(strcat('Which engine number is engine ',num2str(i),' symmetrical to? '));

        engine.thrust(i,1)=engine.thrust(engsym);
        %engine.thrustcrz(i)=engine.thrustcrz(engsym);
        
        for j=1:1:3
            engine.xyz(i,j)=((-1)^(j+1))*engine.xyz(engsym,j);
            engine.vector(i,j)=((-1)^(j+1))*engine.vector(engsym,j);
            engine.unitvector(i,j)=((-1)^(j+1))*engine.unitvector(engsym,j);
            engine.moments(i,j)=((-1)^j)*engine.moments(engsym,j);
            %engine.forces(i,j)=((-1)^(j+1))*engine.forces(engsym,j);
            %engine.forcescrz(i,j)=engine.forces(i,j)*engine.thrustcrz(i)/engine.thrust(i);
        end
        
        %engine.omega(i)=engine.omega(engsym);
        %engine.bpr(i)=engine.bpr(engsym);
        %engine.maxmach(i)=engine.maxmach(engsym);
    
        %engine.mass(i)=engine.mass(engsym);
        %engine.length(i)=engine.length(engsym);
        %engine.diameter(i)=engine.diameter(engsym);
        %engine.sfcmaxt(i)=engine.sfcmaxt(engsym);
        %engine.sfccrz(i)=engine.sfccrz(engsym);
        
    else
        disp(' ')
        disp('Please input coordinates of engine:');
        %disp(' ')
        disp('********************************************')
        disp('* Please note that x is positive rearward  *')
        disp('* Please note that y is positive starboard *')
        disp('* Please note that z is positive upward    *')
        disp('********************************************')
        %disp(' ')
        engine.xyz(i,1)=input('x: ');
        engine.xyz(i,2)=input('y: ');
        engine.xyz(i,3)=input('z: ');
        disp(' ')
        engine.thrust(i,1)=1000*input('Please enter engine thrust (kN): ');
        disp(' ')
        disp('Please enter engine thrust vector in the form [x y z]:');
        %disp(' ')
        disp('********************************************')
        disp('* Please note that x is positive forward   *')
        disp('* Please note that y is positive starboard *')
        disp('* Please note that z is positive downward  *')
        disp('********************************************')
        %disp(' ')
        engine.vector(i,1)=-input('x: ');
        engine.vector(i,2)=input('y: ');
        engine.vector(i,3)=-input('z: ');
        
        for j=1:1:3
            engine.unitvector(i,j)=engine.vector(i,j)/sqrt(sum(engine.vector(i,:).^2));
        end
        
        engine.moments(i,:)=cross((engine.xyz(i,:)-geo.ref_point),engine.thrust(i)*engine.unitvector(i,:));
        
        %engine.omega(i)=(pi()/30)*input('Please enter engine rpm: ');
        %engine.bpr(i)=input('Please enter engine bypass ratio: ');
        %engine.maxmach(i)=input('Please enter engine maximum Mach no: ');
    
        %engine.mass(i)=14.7*(engine.thrust(i)^1.1)*exp(-0.045*engine.bpr(i));
        %engine.length(i)=0.49*(engine.thrust(i)^0.4)*(engine.maxmach(i)^0.2);
        %engine.diameter(i)=0.15*(engine.thrust(i)^0.5)*exp(0.04*engine.bpr(i));
        %engine.sfcmaxt(i)=19*exp(-0.12*engine.bpr(i));
        %engine.sfccrz(i)=25*exp(-0.05*engine.bpr(i));
        %engine.thrustcrz(i)=0.35*(engine.thrust(i)^0.9)*exp(0.02*engine.bpr(i));
    
        %for j=1:1:3
            %engine.forces(i,j)=engine.thrust(i)*engine.vector(i,j)/sqrt(sum(engine.vector(i,:).^2));
            %engine.forcescrz(i,j)=engine.forces(i,j)*engine.thrustcrz(i)/engine.thrust(i);
        %end

    end
    
    %engine.theta(i,1)=atan(engine.vector(i,3)/engine.vector(i,2));
    %engine.theta(i,2)=atan(engine.vector(i,3)/engine.vector(i,1));
    %engine.theta(i,3)=atan(engine.vector(i,2)/engine.vector(i,1));

end

%for i=1:1:engine.number
%    engine.tfactor(i)=engine.thrust(i)/sum(engine.thrust);
%end

end