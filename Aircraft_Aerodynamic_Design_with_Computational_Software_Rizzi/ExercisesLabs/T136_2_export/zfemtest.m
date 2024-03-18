%fem test script
clear

cd aircraft
load('F16-5clean.mat')
cd ..

cd output
load('q1-Cx')
cd ..

wingno=1

structure.nx=[50 50 50];
structure.spars=[0.3 0.7];
%structure.skin_thickness=0.005;




%structure.fueled_span=[0.80];      %Part of wing with tanks
                      
%structure.sp=[0 0.32 0.33 0.6 0.61 0.8 0.81 1]    %Skin thickness position];

structure.st=[0.7 0.7 0.60 0.6 0.6 0.6 0.6 0.6]     %Skin thickness at
                                                    %the thickness positions   
                                                    %Listed in structures.sp  


structure.skin_thick=0.001;     %Box wall thickness
structure.wingmat='AA2024';     %Material name, see /FEM/material_data.m

                
%geo.vCfraction(1)=0.6;              %FIX THIS!
                
                
                structure.skin_thickness=structure.skin_thick.*structure.st


FEM3(geo,results,structure,wingno)