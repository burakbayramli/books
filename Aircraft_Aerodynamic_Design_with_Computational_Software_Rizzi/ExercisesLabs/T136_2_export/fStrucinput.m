function [ structure ] = fStrucinput( input_args )
%This function collects all neccesary data for the wing in order to do the
%aeroelastic calculations.

%MAIN WINGONLY

structure.nx=100;   %number of FEM nodepoints on the wing.

structure.spars(1,1)=input('Front spar postition, chord fraction. [0..1]:');
structure.spars(1,2)=input('Rear spar postition, chord fraction. [0..1]:');

structure.fueled_span(1,1)=input('Span fraction of fuel tankage, [0..1]:');
structure.skin_thick(1,1)=input('Baseline skin thickness, [m]:');

cd aircraft
    cd material
        disp('Availabe Materials: ')
        ls
        try
        file=input('Input material filename:','s');
        structure.material=file(1:end-4);
        load(file)
        catch
            disp(' ')
            disp(' ')
            disp('File not found.')
            disp('Default AA2024 used')
            structure.material='AA2024';
            disp(' ')
        end
        
        
    cd ..
cd ..

disp(' ')
disp(' Define a vector containing the span stations to set the skin thickness at.')
disp('    For the same thickness throughout, set 0')
disp('     For a linear variation root to tip, set [0 1]')
disp('     For a discontinious skin thickness midspan set [0 0.5 0.51 1]')
disp(' ')
structure.sp=input('Vector of normalised span stations to set thickness [0..1]: ');
disp(' ')
for i=1:size(structure.sp,2)
    disp(' ')
    disp(strcat('Skin thickness at spanstation ',num2str(structure.sp(i))))
    structure.st(i)=input('Skin thickness fraction of baseline thickness: ');

end
    
structure

end

