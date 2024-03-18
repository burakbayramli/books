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