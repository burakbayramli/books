function [output]=fCheckThickness(geo)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Subsidary function to TORNADO.
% Checks thickness of the airfoils loaded in the geo structure.
% 
% If all airfoils have nonzero thickness, output is 1.
% Otherwise 0.
%
% This function is used prior to running the strip theory scripts,
% which will crash at a zero thickness entry.
%
% Tomas Melin 2008-09-22 
%%%%%

output=1;
[a b]=size(geo.nx);
for i =1:a
    for j=1:b
        for k=1:2

         foil=geo.foil(i,j,k);
            if isempty(cell2mat(foil))
                %Skip empty partitions

            elseif isempty(str2num((cell2mat(foil))))==0
                 TYPE=1;       %Naca xxxx profile.
                 
                 foil2=str2num(cell2mat(foil));
                 m=fix(foil2/1000);	%gives first NACA-4 number
                 foil3=foil2-m*1000;
                 p=fix(foil3/100);	%gives second NACA-4 number
                 foil4=foil3-p*100;
                 
                 if foil4<=0
                     
                     %disp('Flat plate aifoil, geometry unsuitable for strip theory.')
                     output=0;
                     return
                 end
                 
                 
                 
            elseif isempty(str2num((cell2mat(foil))))
                 TYPE=2;       %Airfoil from file. 
                 %disp('Aifoil thickness not validated.')
                 %disp('Zero thickness will crash program.')
                 
                 
            else
                disp('Foil error, flatplate assumed')
                return
            end
         
        end
         
     end
end
