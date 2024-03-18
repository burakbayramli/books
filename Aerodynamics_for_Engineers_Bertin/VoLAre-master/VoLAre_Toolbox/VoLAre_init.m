% Function to read the input file and initialize the panel model.  
% Paneling can be preprocessed (viewed) after initialization.  
%
% Anthony Ricciardi
% August 2017
%
% Inputs
% filename = [string] text filename of ZAERO-formatted input file
%
% Outputs
% FEM = [struct] model data
%
function FEM = VoLAre_init(filename)
FEMcell = importFieldModelAdvanced(filename);
FEM = createPanelModel(FEMcell);
end

