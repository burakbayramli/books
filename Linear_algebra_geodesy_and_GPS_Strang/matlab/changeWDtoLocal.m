%  changes the Matlab working directory to the YUMA directory
%  - assumes the GPSVisibilityTool is launched from its local
%    directory

function callDir = changeWDtoLocal
    callDir = cd;
    cd([callDir slashType(cd) 'YUMA'])  