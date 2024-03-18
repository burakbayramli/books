function [bc]=boundary(lattice,state,geo)
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
% usage: [boundarycondition] = boundary4(lattice,state,geo)
%
% This function computes the right hand side of the vortex lattice equation
% system. I.e. the velocity parallell to the panel normal through each 
% collocation point due to rotations and angle of attack and sideslip.
%
% Example:
%
%  rhs=(setboundary4(lattice,state,geo))';
%
% Calls:
%           None    
%
% Author: Tomas Melin <melin@kth.se>
% Keywords: Tornado core function
%
% Revision History:
%   Bristol,  2007 06 27:  Addition of new header. TM.
%   Spånga, 2021-09-19:   Updated to MATLAB R2020, TM  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[a b c]=size(lattice.COLLOC);
V=state.AS;

delta=config('delta');     %Differential delta.

%%%%
%Steady state boundary condition column
wind=V.*([cos(state.alpha)*cos(state.betha) -cos(state.alpha)*sin(state.betha) sin(state.alpha)]);
Wind=ones(a,1)*wind;

%% This is the wonky part
%disp('WONKY BUG HERE')
%Atmos=fwind_profile(lattice.COLLOC(:,3));
%Atx=Atmos*cos(state.phi);
%Aty=-Atmos*sin(state.phi);

%ATMO=[Atx Aty zeros(size(Atx))]
ATMO=[];


%% End Wonky part



for i=1:a
   Rot(i,:)=cross((lattice.COLLOC(i,:)-geo.CG),[state.P state.Q state.R]);
end


%veloc=Wind+Rot+ATMO;
veloc=Wind+Rot;

bc(:,1)=sum(lattice.N.*veloc,2)';  %steady state bc
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
%alpha derivative column
state.alpha=state.alpha+delta;
wind=V.*([cos(state.alpha)*cos(state.betha) -cos(state.alpha)*sin(state.betha) sin(state.alpha)]);
Wind=ones(a,1)*wind;
for i=1:a
   Rot(i,:)=cross((lattice.COLLOC(i,:)-geo.CG),[state.P state.Q state.R]);
end                                   
veloc=Wind+Rot;
bc(:,2)=sum(lattice.N.*veloc,2)'; 
state.alpha=state.alpha-delta;
%%%%%%%

%%%%%
%betha derivative column
state.betha=state.betha+delta;
wind=V.*([cos(state.alpha)*cos(state.betha) -cos(state.alpha)*sin(state.betha) sin(state.alpha)]);
Wind=ones(a,1)*wind;
for i=1:a
   Rot(i,:)=cross((lattice.COLLOC(i,:)-geo.CG),[state.P state.Q state.R]);
end                                   
veloc=Wind+Rot;
bc(:,3)=sum(lattice.N.*veloc,2)'; 
state.betha=state.betha-delta;
%%%%%%%

%%%%%
%rollrate, P, derivative column
state.P=state.P+delta;
wind=V.*([cos(state.alpha)*cos(state.betha) -cos(state.alpha)*sin(state.betha) sin(state.alpha)]);
Wind=ones(a,1)*wind;
for i=1:a
   Rot(i,:)=cross((lattice.COLLOC(i,:)-geo.CG),[state.P state.Q state.R]);
end                                   
veloc=Wind+Rot;
bc(:,4)=sum(lattice.N.*veloc,2)'; 
state.P=state.P-delta;
%%%%%%%

%%%%%
%pitchrate, Q, derivative column
state.Q=state.Q+delta;
wind=V.*([cos(state.alpha)*cos(state.betha) -cos(state.alpha)*sin(state.betha) sin(state.alpha)]);
Wind=ones(a,1)*wind;
for i=1:a
   Rot(i,:)=cross((lattice.COLLOC(i,:)-geo.CG),[state.P state.Q state.R]);
end                                   
veloc=Wind+Rot;
bc(:,5)=sum(lattice.N.*veloc,2)'; 
state.Q=state.Q-delta;
%%%%%%

%%%%%
%yaw rate, R, derivative column
state.R=state.R+delta;
wind=V.*([cos(state.alpha)*cos(state.betha) -cos(state.alpha)*sin(state.betha) sin(state.alpha)]);
Wind=ones(a,1)*wind;
for i=1:a
   Rot(i,:)=cross((lattice.COLLOC(i,:)-geo.CG),[state.P state.Q state.R]);
end                                   
veloc=Wind+Rot;
bc(:,6)=sum(lattice.N.*veloc,2)'; 
state.R=state.R-delta;
%%%%%%

                                                %Oh god im tired, I bet
                                                %there will be a bug here
%%%%%%%%%%%%%%%%%%%%%%%%
ntece=sum(sum(geo.flapped)); %Number of trailing edge control effectors
[n,m]=find(geo.flapped');

for rudder=1:ntece
    geo.flap_vector(m(rudder),n(rudder))=geo.flap_vector(m(rudder),n(rudder))+delta; 
    [lattice,ref]=fLattice_setup2(geo,state,0);

    wind=V.*([cos(state.alpha)*cos(state.betha) -cos(state.alpha)*sin(state.betha) sin(state.alpha)]);
    Wind=ones(a,1)*wind;
    for i=1:a
        Rot(i,:)=cross((lattice.COLLOC(i,:)-geo.CG),[state.P state.Q state.R]);
    end                                   
    veloc=Wind+Rot;
    bc(:,6+rudder)=sum(lattice.N.*veloc,2)'; 
    geo.flap_vector(m(rudder),n(rudder))=geo.flap_vector(m(rudder),n(rudder))-delta; 
    %no need to reset lattice  as it will be done in next loop
end

end %function

