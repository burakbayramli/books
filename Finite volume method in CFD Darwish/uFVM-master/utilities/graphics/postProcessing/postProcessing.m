function varargout = postProcessing(varargin)
% POSTPROCESSING MATLAB code for postProcessing.fig
%      POSTPROCESSING, by itself, creates a new POSTPROCESSING or raises the existing
%      singleton*.
%
%      H = POSTPROCESSING returns the handle to a new POSTPROCESSING or the handle to
%      the existing singleton*.
%
%      POSTPROCESSING('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in POSTPROCESSING.M with the given input arguments.
%
%      POSTPROCESSING('Property','Value',...) creates a new POSTPROCESSING or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before postProcessing_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to postProcessing_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help postProcessing

% Last Modified by GUIDE v2.5 08-Mar-2019 18:57:26

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @postProcessing_OpeningFcn, ...
                   'gui_OutputFcn',  @postProcessing_OutputFcn, ...
                   'gui_LayoutFcn',  [] , ...
                   'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


% --- Executes just before postProcessing is made visible.
function postProcessing_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to postProcessing (see VARARGIN)

% Choose default command line output for postProcessing
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

%
% Set icons
%
postProcessorPath = fileparts(mfilename('fullpath'));
iconsDirectoryPath = [postProcessorPath, filesep, 'icons'];

% Vector field
vectorFieldIcon = imread([iconsDirectoryPath, filesep, 'vector.jpg']);
handles.vectorField.CData = imresize(vectorFieldIcon, [30 30]);

% slicetool tool
applyToolIcon = imread([iconsDirectoryPath, filesep, 'apply.png']);
handles.apply.CData = imresize(applyToolIcon, [30 30]);

% Clear all tool
clearAllIcon = imread([iconsDirectoryPath, filesep, 'clear.png']);
handles.clearGraphics.CData = imresize(clearAllIcon, [20 20]);

% initial tool
initialToolIcon = imread([iconsDirectoryPath, filesep, 'initial.png']);
handles.initial.CData = imresize(initialToolIcon, [24 24]);

% backward tool
backwardsToolIcon = imread([iconsDirectoryPath, filesep, 'backward.png']);
handles.backward.CData = imresize(backwardsToolIcon, [24 24]);

% Play tool
playToolIcon = imread([iconsDirectoryPath, filesep, 'play.png']);
handles.play.CData = imresize(playToolIcon, [22 22]);

% Forward tool
forwardToolIcon = imread([iconsDirectoryPath, filesep, 'forward.png']);
handles.forward.CData = imresize(forwardToolIcon, [24 24]);

% Final tool
endToolIcon = imread([iconsDirectoryPath, filesep, 'end.png']);
handles.final.CData = imresize(endToolIcon, [24 24]);

handles.axes1;

set(handles.axes1,'xtick',[])
set(handles.axes1,'xticklabel',[])
set(handles.axes1,'ytick',[])
set(handles.axes1,'yticklabel',[])
set(handles.axes1,'Box','On')
set(handles.axes1,'View',[0 90])
set(handles.axes1,'Clipping','off');

axis off;
axis equal;

%
% Define data base
%

global Region;

if isempty(Region)
    Region.caseDirectoryPath = pwd;
    
    % Read mesh
    cfdReadPolyMesh;

    % Read system files
    cfdReadSystem;     
    
    % Read field at time 0
    cfdReadTimeDirectory;
else
    if strcmp(cfdGetCaseDirectoryPath, pwd)        
        if ~isfield(Region,'mesh')
            % Read mesh
            cfdReadPolyMesh;            
        end
    else
        Region.caseDirectoryPath = pwd;
        
        % Read mesh
        cfdReadPolyMesh;

        % Read system files
        cfdReadSystem;    
        
        % Read field at time 0
        cfdReadTimeDirectory;        
    end    
end

% Deactivate vector field button if no velocity field is available
if ~cfdIsFieldAvailable('U')
    handles.plotVectors.Enable = 'off';
end
    

%
% Read all fields at all existing time steps
%
theTimeSteps = cfdGetTimeSteps;

% store time steps
handles.timeSteps = theTimeSteps;

% Loop over time steps and read fields
iTime = 1;
for timeStep=theTimeSteps'
    
    files = dir([Region.caseDirectoryPath, filesep, num2str(timeStep)]);
    
    for iFile=1:length(files)
        if (files(iFile).bytes)==0 || (files(iFile).isdir)
            continue;
        end
        
        % get field name from file name
        fieldName = files(iFile).name;
        theField = cfdReadFieldFromTimeDirectory(fieldName, timeStep);
        
        % Skip if surface field
        if strcmp(theField.type, 'surfaceScalarField')
            continue;
        end
        
        % Skip if derived field
        if strcmp(theField.name, 'DU1') || strcmp(theField.name, 'DU2') || strcmp(theField.name, 'DU3')
            continue;
        end        
        
        % Skip if derived field
        if strcmp(theField.name, 'DUT1') || strcmp(theField.name, 'DUT2') || strcmp(theField.name, 'DUT3')
            continue;
        end                
        
        % Skip if pressure correction
        if strcmp(theField.name, 'pp')
            continue;
        end                        
        
        % Store
        Region.postProcessing.(fieldName)(:,iTime,:) = theField.phi;
    end
    
    iTime = iTime + 1;
end

%
% Plot goemetry in axes
%

theNumberOfNodes = cfdGetNumberOfNodes;
theNumberOfFaces = cfdGetNumberOfFaces;
theNumberOfElements = cfdGetNumberOfElements;
theNumberOfBPatches = cfdGetNumberOfBPatches;

theNodeCentroids = cfdGetNodeCentroids;
theFaceNodeIndices = cfdGetFaceNodeIndices;

% Fill in vertices and faces arrays
for iNode=1:theNumberOfNodes
    v(iNode,:) = theNodeCentroids(iNode,:);
end

for iFace=1:theNumberOfFaces
    f{iFace,:} = theFaceNodeIndices{iFace};
end

% Convert cell f to matrix form
f = cfdConvertCelltoMatrix(f);

% Add face patches to figure
patch('vertices', v, 'faces', f, 'FaceColor', [0.5 0.5 0.5], 'FaceAlpha', 1, 'EdgeAlpha', 1, 'Tag', 'Mesh');

% Adjusts to figure
rotate3d;

view(45,45);

% [minLimit_x, maxLimit_x, minLimit_y, maxLimit_y, minLimit_z, maxLimit_z] = cfdGetBoundingBox;

% length_scale = max([maxLimit_x-minLimit_x maxLimit_y-minLimit_y maxLimit_z-minLimit_z])*0.04;
% 
% axis([2*minLimit_x-length_scale, 2*maxLimit_x+length_scale, 2*minLimit_y-length_scale, 2*maxLimit_y+length_scale, 2*minLimit_z-length_scale, 2*maxLimit_z+length_scale]);

% Set field list
handles.fieldsList.String = fieldnames(Region.postProcessing);

% Check the mesh lines box
handles.showMeshLines.Value = 1;


% Update mesh preview buttons
handles.elementsPanel.Title = ['Elements: (1-',num2str(theNumberOfElements),')'];
handles.facesPanel.Title = ['Faces: (1-',num2str(theNumberOfFaces),')'];
handles.patchesPanel.Title = ['Patches: (1-',num2str(theNumberOfBPatches),')'];

% Mesh faces/contour faces opacity to 1
handles.opacity.Value = 1;


% UIWAIT makes postProcessing wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = postProcessing_OutputFcn(hObject, eventdata, handles) 
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;


% --- Executes on button press in vectorField.
function vectorField_Callback(hObject, eventdata, handles)
% hObject    handle to vectorField (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)



% --- Executes on selection change in fieldsList.
function fieldsList_Callback(hObject, eventdata, handles)
% hObject    handle to fieldsList (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = cellstr(get(hObject,'String')) returns fieldsList contents as cell array
%        contents{get(hObject,'Value')} returns selected item from fieldsList


% --- Executes during object creation, after setting all properties.
function fieldsList_CreateFcn(hObject, eventdata, handles)
% hObject    handle to fieldsList (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in showMeshLines.
function showMeshLines_Callback(hObject, eventdata, handles)
% hObject    handle to showMeshLines (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of showMeshLines

if handles.showMeshLines.Value==1
     obj = findobj('Tag','Mesh');
     if ~isempty(obj)
        set(obj, 'EdgeColor', 'k');
     end
     
     obj = findobj('Tag','FieldContour');
     if ~isempty(obj)
        set(obj, 'EdgeColor', 'k');
     end     
else
     obj = findobj('Tag','Mesh');
     if ~isempty(obj)
        set(obj, 'EdgeColor', 'none');
     end
     
     obj = findobj('Tag','FieldContour');
     if ~isempty(obj)
        set(obj, 'EdgeColor', 'none');
     end     
end


% --- Executes on slider movement.
function limitSilder_Callback(hObject, eventdata, handles)
% hObject    handle to limitSilder (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'Value') returns position of slider
%        get(hObject,'Min') and get(hObject,'Max') to determine range of slider


% --- Executes during object creation, after setting all properties.
function limitSilder_CreateFcn(hObject, eventdata, handles)
% hObject    handle to limitSilder (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: slider controls usually have a light gray background.
if isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor',[.9 .9 .9]);
end


% --- Executes on button press in sliceTool.
function sliceTool_Callback(hObject, eventdata, handles)
% hObject    handle to sliceTool (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in pushbutton3.
function pushbutton3_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in pushbutton9.
function pushbutton9_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton9 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in pushbutton10.
function pushbutton10_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton10 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in pushbutton11.
function pushbutton11_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton11 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in pushbutton12.
function pushbutton12_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton12 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in pushbutton13.
function pushbutton13_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton13 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in pushbutton14.
function pushbutton14_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton14 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in pushbutton15.
function pushbutton15_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton15 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in pushbutton16.
function pushbutton16_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton16 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in pushbutton17.
function pushbutton17_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton17 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in pushbutton18.
function pushbutton18_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton18 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in pushbutton19.
function pushbutton19_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton19 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in updateMeshLinesColor.
function updateMeshLinesColor_Callback(hObject, eventdata, handles)
% hObject    handle to updateMeshLinesColor (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in initial.
function initial_Callback(hObject, eventdata, handles)
% hObject    handle to initial (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global Region;

theSelectedFieldName = handles.fieldsList.String{handles.fieldsList.Value};
timeIndex = 1;

isCentroid = handles.centroid.Value;
theNodeCentroids = cfdGetNodeCentroids;
theFaceNodeIndices = cfdGetFaceNodeIndices;
type = cfdGetFieldType(theSelectedFieldName);

if ~cfdIsFileExists(theSelectedFieldName, '0')
    return;
end

phi = Region.postProcessing.(theSelectedFieldName)(:,timeIndex,:);

if strcmp(type, 'volVectorField')
    phi = cfdMag([phi(:,1,1), phi(:,1,2), phi(:,1,3)]);
end

% Convert cell f to matrix form
theFaceNodeIndices = cfdConvertCelltoMatrix(theFaceNodeIndices);

handles.axes1;
cla;

edgeColor = 'none';
if handles.showMeshLines.Value==1
    edgeColor = [0 0 0];
end

opacity = handles.opacity.Value;

if isCentroid
    cdata = cfdInterpolateFromElementsToFaces('linear', phi);
    patch('vertices', theNodeCentroids, 'faces', theFaceNodeIndices, 'FaceVertexCData', cdata, 'FaceColor', 'flat', 'Tag', 'FieldContour','EdgeColor',edgeColor,'FaceAlpha',opacity);
else
    cdata = cfdInterpolateFromElementsToNodes(phi);
    patch('vertices', theNodeCentroids, 'faces', theFaceNodeIndices, 'FaceVertexCData', cdata, 'FaceColor', 'interp', 'Tag', 'FieldContour','EdgeColor',edgeColor,'FaceAlpha',opacity);
end

% Update time
timeSteps = cfdGetTimeSteps;
handles.time.String = timeSteps(1);  

if handles.plotVectors.Value==1
    plotVectors_Callback(hObject, eventdata, handles);
end

colorbar;
title(theSelectedFieldName);
colormap jet;


% --- Executes on button press in forward.
function forward_Callback(hObject, eventdata, handles)
% hObject    handle to forward (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global Region;

theSelectedFieldName = handles.fieldsList.String{handles.fieldsList.Value};
theTimeStep = str2double(handles.time.String);
theTimeSteps = cfdGetTimeSteps;
timeIndex = find(theTimeSteps==theTimeStep);

if timeIndex==length(theTimeSteps)
    return;
else
    timeIndex = timeIndex + 1;
end

isCentroid = handles.centroid.Value;
theNodeCentroids = cfdGetNodeCentroids;
theFaceNodeIndices = cfdGetFaceNodeIndices;
type = cfdGetFieldType(theSelectedFieldName);

if ~cfdIsFileExists(theSelectedFieldName, num2str(theTimeSteps(timeIndex)))
    return;
end

phi = Region.postProcessing.(theSelectedFieldName)(:,timeIndex,:);

if strcmp(type, 'volVectorField')
    phi = cfdMag([phi(:,1,1), phi(:,1,2), phi(:,1,3)]);
end

% Convert cell f to matrix form
theFaceNodeIndices = cfdConvertCelltoMatrix(theFaceNodeIndices);

handles.axes1;
cla;

edgeColor = 'none';
if handles.showMeshLines.Value==1
    edgeColor = [0 0 0];
end

opacity = handles.opacity.Value;

if isCentroid
    cdata = cfdInterpolateFromElementsToFaces('linear', phi);
    patch('vertices', theNodeCentroids, 'faces', theFaceNodeIndices, 'FaceVertexCData', cdata, 'FaceColor', 'flat', 'Tag', 'FieldContour','EdgeColor',edgeColor,'FaceAlpha',opacity);
else
    cdata = cfdInterpolateFromElementsToNodes(phi);
    patch('vertices', theNodeCentroids, 'faces', theFaceNodeIndices, 'FaceVertexCData', cdata, 'FaceColor', 'interp', 'Tag', 'FieldContour','EdgeColor',edgeColor,'FaceAlpha',opacity);
end

% Update time
timeSteps = cfdGetTimeSteps;
handles.time.String = timeSteps(timeIndex);

if handles.plotVectors.Value==1
    plotVectors_Callback(hObject, eventdata, handles);
end

colorbar;
title(theSelectedFieldName);
colormap jet;


% --- Executes on button press in backward.
function backward_Callback(hObject, eventdata, handles)
% hObject    handle to backward (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global Region;

theSelectedFieldName = handles.fieldsList.String{handles.fieldsList.Value};
theTimeStep = str2double(handles.time.String);
theTimeSteps = cfdGetTimeSteps;
timeIndex = find(theTimeSteps==theTimeStep);

if timeIndex==1
    return;
else
    timeIndex = timeIndex - 1;
end

isCentroid = handles.centroid.Value;
theNodeCentroids = cfdGetNodeCentroids;
theFaceNodeIndices = cfdGetFaceNodeIndices;
type = cfdGetFieldType(theSelectedFieldName);

if ~cfdIsFileExists(theSelectedFieldName, num2str(theTimeSteps(timeIndex)))
    return;
end

phi = Region.postProcessing.(theSelectedFieldName)(:,timeIndex,:);

if strcmp(type, 'volVectorField')
    phi = cfdMag([phi(:,1,1), phi(:,1,2), phi(:,1,3)]);
end

% Convert cell f to matrix form
theFaceNodeIndices = cfdConvertCelltoMatrix(theFaceNodeIndices);

handles.axes1;
cla;

edgeColor = 'none';
if handles.showMeshLines.Value==1
    edgeColor = [0 0 0];
end

opacity = handles.opacity.Value;

if isCentroid
    cdata = cfdInterpolateFromElementsToFaces('linear', phi);
    patch('vertices', theNodeCentroids, 'faces', theFaceNodeIndices, 'FaceVertexCData', cdata, 'FaceColor', 'flat', 'Tag', 'FieldContour','EdgeColor',edgeColor,'FaceAlpha',opacity);
else
    cdata = cfdInterpolateFromElementsToNodes(phi);
    patch('vertices', theNodeCentroids, 'faces', theFaceNodeIndices, 'FaceVertexCData', cdata, 'FaceColor', 'interp', 'Tag', 'FieldContour','EdgeColor',edgeColor,'FaceAlpha',opacity);
end

% Update time
timeSteps = cfdGetTimeSteps;
handles.time.String = timeSteps(timeIndex);

if handles.plotVectors.Value==1
    plotVectors_Callback(hObject, eventdata, handles);
end

colorbar;
title(theSelectedFieldName);
colormap jet;


% --- Executes on button press in final.
function final_Callback(hObject, eventdata, handles)
% hObject    handle to final (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global Region;

theSelectedFieldName = handles.fieldsList.String{handles.fieldsList.Value};
theTimeSteps = cfdGetTimeSteps;
timeIndex = length(theTimeSteps);

isCentroid = handles.centroid.Value;
theNodeCentroids = cfdGetNodeCentroids;
theFaceNodeIndices = cfdGetFaceNodeIndices;
type = cfdGetFieldType(theSelectedFieldName);

if ~cfdIsFileExists(theSelectedFieldName, num2str(theTimeSteps(end)))
    return;
end

phi = Region.postProcessing.(theSelectedFieldName)(:,timeIndex,:);

if strcmp(type, 'volVectorField')
    phi = cfdMag([phi(:,1,1), phi(:,1,2), phi(:,1,3)]);
end

% Convert cell f to matrix form
theFaceNodeIndices = cfdConvertCelltoMatrix(theFaceNodeIndices);

handles.axes1;
cla;

edgeColor = 'none';
if handles.showMeshLines.Value==1
    edgeColor = [0 0 0];
end

opacity = handles.opacity.Value;

if isCentroid
    cdata = cfdInterpolateFromElementsToFaces('linear', phi);
    patch('vertices', theNodeCentroids, 'faces', theFaceNodeIndices, 'FaceVertexCData', cdata, 'FaceColor', 'flat', 'Tag', 'FieldContour','EdgeColor',edgeColor,'FaceAlpha',opacity);
else
    cdata = cfdInterpolateFromElementsToNodes(phi);
    patch('vertices', theNodeCentroids, 'faces', theFaceNodeIndices, 'FaceVertexCData', cdata, 'FaceColor', 'interp', 'Tag', 'FieldContour','EdgeColor',edgeColor,'FaceAlpha',opacity);
end

% Update time
handles.time.String = theTimeSteps(end);  

if handles.plotVectors.Value==1
    plotVectors_Callback(hObject, eventdata, handles);
end

colorbar;
title(theSelectedFieldName);
colormap jet;


function time_Callback(hObject, eventdata, handles)
% hObject    handle to time (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of time as text
%        str2double(get(hObject,'String')) returns contents of time as a double


% --- Executes during object creation, after setting all properties.
function time_CreateFcn(hObject, eventdata, handles)
% hObject    handle to time (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes during object deletion, before destroying properties.
function uitable1_DeleteFcn(hObject, eventdata, handles)
% hObject    handle to uitable1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in play.
function play_Callback(hObject, eventdata, handles)
% hObject    handle to play (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global Region;

theSelectedFieldName = handles.fieldsList.String{handles.fieldsList.Value};
theTimeStep = str2double(handles.time.String);
currentTimeIndex = find(cfdGetTimeSteps==theTimeStep);

isCentroid = handles.centroid.Value;
theNodeCentroids = cfdGetNodeCentroids;
theFaceNodeIndices = cfdGetFaceNodeIndices;
type = cfdGetFieldType(theSelectedFieldName);

% Convert cell f to matrix form
theFaceNodeIndices = cfdConvertCelltoMatrix(theFaceNodeIndices);

for timeIndex=currentTimeIndex:length(cfdGetTimeSteps)
    phi = Region.postProcessing.(theSelectedFieldName)(:,timeIndex,:);

    if strcmp(type, 'volVectorField')
        phi = cfdMag([phi(:,1,1), phi(:,1,2), phi(:,1,3)]);
    end

    handles.axes1;
    cla;

    edgeColor = 'none';
    if handles.showMeshLines.Value==1
        edgeColor = [0 0 0];
    end

    opacity = handles.opacity.Value;

    if isCentroid
        cdata = cfdInterpolateFromElementsToFaces('linear', phi);
        patch('vertices', theNodeCentroids, 'faces', theFaceNodeIndices, 'FaceVertexCData', cdata, 'FaceColor', 'flat', 'Tag', 'FieldContour','EdgeColor',edgeColor,'FaceAlpha',opacity);
    else
        cdata = cfdInterpolateFromElementsToNodes(phi);
        patch('vertices', theNodeCentroids, 'faces', theFaceNodeIndices, 'FaceVertexCData', cdata, 'FaceColor', 'interp', 'Tag', 'FieldContour','EdgeColor',edgeColor,'FaceAlpha',opacity);
    end
    
    % Update time
    timeSteps = cfdGetTimeSteps;
    handles.time.String = timeSteps(timeIndex);
    
    if handles.plotVectors.Value==1
        plotVectors_Callback(hObject, eventdata, handles);
    end    
    
    colorbar;
    title(theSelectedFieldName);
    colormap jet;
    
    pause(0.1);
    
end


% --- Executes on button press in clearGraphics.
function clearGraphics_Callback(hObject, eventdata, handles)
% hObject    handle to clearGraphics (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
handles.axes1;
cla;

colorbar off;
title '';


% --- Executes on slider movement.
function vectorScale_Callback(hObject, eventdata, handles)
% hObject    handle to vectorScale (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'Value') returns position of slider
%        get(hObject,'Min') and get(hObject,'Max') to determine range of slider

value = handles.vectorScale.Value;

obj = findobj('Tag','VectorField');

if ~isempty(obj)
    set(obj, 'AutoScaleFactor', 2*value);
end


% --- Executes during object creation, after setting all properties.
function vectorScale_CreateFcn(hObject, eventdata, handles)
% hObject    handle to vectorScale (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: slider controls usually have a light gray background.
if isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor',[.9 .9 .9]);
end


% --- Executes on button press in display.
function display_Callback(hObject, eventdata, handles)
% hObject    handle to display (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

handles.axes1;
cla;
axis equal;
axis off;
title('');

theNumberOfNodes = cfdGetNumberOfNodes;
theNumberOfFaces = cfdGetNumberOfFaces;

theNodeCentroids = cfdGetNodeCentroids;
theFaceNodeIndices = cfdGetFaceNodeIndices;

% Fill in vertices anf faces arrays
for iNode=1:theNumberOfNodes
    v(iNode,:) = theNodeCentroids(iNode,:);
end

for iFace=1:theNumberOfFaces
    f{iFace,:} = theFaceNodeIndices{iFace};
end

% Convert cell f to matrix form
f = cfdConvertCelltoMatrix(f);

% Add face patches to figure
patch('vertices', v, 'faces', f, 'FaceColor', [0.5 0.5 0.5], 'FaceAlpha', 0.3, 'EdgeAlpha', 0.3, 'Tag', 'Mesh');

% Adjusts to figure
rotate3d;
set(handles.axes1,'Clipping','off');

[minLimit_x, maxLimit_x, minLimit_y, maxLimit_y, minLimit_z, maxLimit_z] = cfdGetBoundingBox;

length_scale = max([maxLimit_x-minLimit_x maxLimit_y-minLimit_y maxLimit_z-minLimit_z])*0.0/4;

axis([2*minLimit_x-length_scale, 2*maxLimit_x+length_scale, 2*minLimit_y-length_scale, 2*maxLimit_y+length_scale, 2*minLimit_z-length_scale, 2*maxLimit_z+length_scale]);


hold on;

if handles.enableElements.Value
    minElement = eval(handles.minElement.String);
    maxElement = eval(handles.maxElement.String);
    
    theNumberOfElements = cfdGetNumberOfElements;
    theElementFaces = cfdGetElementFaceIndices;
    theFaceNodes = cfdGetFaceNodeIndices;
    theNodeCentroids = cfdGetNodeCentroids;
    
    for iElement=minElement:maxElement
        if iElement<=theNumberOfElements && iElement>0
            iFaces = theElementFaces{iElement,1};
            for iFace=iFaces
                theNodes = theFaceNodes{iFace,1};
                local_phi = iElement;
                XYZ = theNodeCentroids(theNodes,:);
                patch(XYZ(:,1),XYZ(:,2),XYZ(:,3),local_phi,'FaceAlpha', 1, 'EdgeAlpha', .1); 
            end
        end
    end    
    
end

hold on;

if handles.enableFaces.Value
    minFace = eval(handles.minFace.String);
    maxFace = eval(handles.maxFace.String);   

    theFaceNodes = cfdGetFaceNodeIndices;
    theNodeCentroids = cfdGetNodeCentroids;
    theNumberOfFaces = cfdGetNumberOfFaces;
    
    for iFace=minFace:maxFace
        if iFace<=theNumberOfFaces && iFace>0
            theNodes = theFaceNodes{iFace,1};
            local_phi = iFace;
            XYZ = theNodeCentroids(theNodes,:);
            patch(XYZ(:,1),XYZ(:,2),XYZ(:,3),local_phi,'FaceAlpha', 1, 'EdgeAlpha', .1); 
        end
    end    
end

hold on;

if handles.enablePatches.Value
    minPatch = eval(handles.minPatch.String);
    maxPatch = eval(handles.maxPatch.String);   
    
    theFaceNodes = cfdGetFaceNodeIndices;
    theNodeCentroids = cfdGetNodeCentroids;
    theNumberOfBPatches = cfdGetNumberOfBPatches;
    
    for iBPatch=minPatch:maxPatch
        if iBPatch<=theNumberOfBPatches && iBPatch>0
            startingFaceIndex = cfdGetStartingFaceIndexForBoundaryPatch(iBPatch);
            numberOfBFaces = cfdGetNumberOfFacesForBoundaryPatch(iBPatch);
            for iFace=startingFaceIndex:startingFaceIndex+numberOfBFaces-1
                theNodes = theFaceNodes{iFace,1};
                local_phi = iBPatch;
                XYZ = theNodeCentroids(theNodes,:);
                patch(XYZ(:,1),XYZ(:,2),XYZ(:,3),local_phi,'FaceAlpha', 1, 'EdgeAlpha', .1); 
            end       
        end
    end
end


rotate3d;




function elementMin_Callback(hObject, eventdata, handles)
% hObject    handle to elementMin (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of elementMin as text
%        str2double(get(hObject,'String')) returns contents of elementMin as a double


% --- Executes during object creation, after setting all properties.
function elementMin_CreateFcn(hObject, eventdata, handles)
% hObject    handle to elementMin (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function elementMax_Callback(hObject, eventdata, handles)
% hObject    handle to elementMax (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of elementMax as text
%        str2double(get(hObject,'String')) returns contents of elementMax as a double


% --- Executes during object creation, after setting all properties.
function elementMax_CreateFcn(hObject, eventdata, handles)
% hObject    handle to elementMax (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit4_Callback(hObject, eventdata, handles)
% hObject    handle to edit4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit4 as text
%        str2double(get(hObject,'String')) returns contents of edit4 as a double


% --- Executes during object creation, after setting all properties.
function edit4_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function minElement_Callback(hObject, eventdata, handles)
% hObject    handle to minElement (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of minElement as text
%        str2double(get(hObject,'String')) returns contents of minElement as a double


% --- Executes during object creation, after setting all properties.
function minElement_CreateFcn(hObject, eventdata, handles)
% hObject    handle to minElement (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function maxElement_Callback(hObject, eventdata, handles)
% hObject    handle to maxElement (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of maxElement as text
%        str2double(get(hObject,'String')) returns contents of maxElement as a double


% --- Executes during object creation, after setting all properties.
function maxElement_CreateFcn(hObject, eventdata, handles)
% hObject    handle to maxElement (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function minFace_Callback(hObject, eventdata, handles)
% hObject    handle to minFace (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of minFace as text
%        str2double(get(hObject,'String')) returns contents of minFace as a double


% --- Executes during object creation, after setting all properties.
function minFace_CreateFcn(hObject, eventdata, handles)
% hObject    handle to minFace (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function maxFace_Callback(hObject, eventdata, handles)
% hObject    handle to maxFace (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of maxFace as text
%        str2double(get(hObject,'String')) returns contents of maxFace as a double


% --- Executes during object creation, after setting all properties.
function maxFace_CreateFcn(hObject, eventdata, handles)
% hObject    handle to maxFace (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit10_Callback(hObject, eventdata, handles)
% hObject    handle to edit10 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit10 as text
%        str2double(get(hObject,'String')) returns contents of edit10 as a double


% --- Executes during object creation, after setting all properties.
function edit10_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit10 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit11_Callback(hObject, eventdata, handles)
% hObject    handle to edit11 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit11 as text
%        str2double(get(hObject,'String')) returns contents of edit11 as a double


% --- Executes during object creation, after setting all properties.
function edit11_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit11 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit12_Callback(hObject, eventdata, handles)
% hObject    handle to edit12 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit12 as text
%        str2double(get(hObject,'String')) returns contents of edit12 as a double


% --- Executes during object creation, after setting all properties.
function edit12_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit12 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit13_Callback(hObject, eventdata, handles)
% hObject    handle to edit13 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit13 as text
%        str2double(get(hObject,'String')) returns contents of edit13 as a double


% --- Executes during object creation, after setting all properties.
function edit13_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit13 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in enableElements.
function enableElements_Callback(hObject, eventdata, handles)
% hObject    handle to enableElements (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of enableElements

if handles.enableElements.Value==1
    handles.minElement.Enable = 'on';
    handles.maxElement.Enable = 'on';
    
    handles.enableFaces.Value = 0;
    handles.enablePatches.Value = 0;      
else
    handles.minElement.Enable = 'off';
    handles.maxElement.Enable = 'off';    
end



function minPatch_Callback(hObject, eventdata, handles)
% hObject    handle to minPatch (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of minPatch as text
%        str2double(get(hObject,'String')) returns contents of minPatch as a double


% --- Executes during object creation, after setting all properties.
function minPatch_CreateFcn(hObject, eventdata, handles)
% hObject    handle to minPatch (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function maxPatch_Callback(hObject, eventdata, handles)
% hObject    handle to maxPatch (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of maxPatch as text
%        str2double(get(hObject,'String')) returns contents of maxPatch as a double


% --- Executes during object creation, after setting all properties.
function maxPatch_CreateFcn(hObject, eventdata, handles)
% hObject    handle to maxPatch (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in enablePatches.
function enablePatches_Callback(hObject, eventdata, handles)
% hObject    handle to enablePatches (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of enablePatches

if handles.enablePatches.Value==1
    handles.minPatch.Enable = 'on';
    handles.maxPatch.Enable = 'on';
    
    handles.enableElements.Value = 0;
    handles.enableFaces.Value = 0;
else
    handles.minPatch.Enable = 'off';
    handles.maxPatch.Enable = 'off';    
end


% --- Executes on button press in enableFaces.
function enableFaces_Callback(hObject, eventdata, handles)
% hObject    handle to enableFaces (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of enableFaces

if handles.enableFaces.Value==1
    handles.minFace.Enable = 'on';
    handles.maxFace.Enable = 'on';
    
    handles.enableElements.Value = 0;
    handles.enablePatches.Value = 0;    
else
    handles.minFace.Enable = 'off';
    handles.maxFace.Enable = 'off';    
end


% --- Executes during object creation, after setting all properties.
function elementsPanel_CreateFcn(hObject, eventdata, handles)
% hObject    handle to elementsPanel (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called


% --- Executes on button press in plotVectors.
function plotVectors_Callback(hObject, eventdata, handles)
% hObject    handle to plotVectors (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of plotVectors


if handles.plotVectors.Value==0
    handles.axes1;
    obj = findobj('Tag','VectorField');
    handles.vectorScale.Enable = 'off';
    delete(obj);
    return;
end

global Region;

handles.axes1;
hold on;

theTimeStep = str2double(handles.time.String);
timeIndex = find(cfdGetTimeSteps==theTimeStep);

isCentroid = handles.centroid.Value;
theNodeCentroids = cfdGetNodeCentroids;

% Get mesh and info
theNumberOfElements = cfdGetNumberOfElements;
theNumberOfBElements = cfdGetNumberOfBFaces;

phi = Region.postProcessing.U(:,timeIndex,:);
U = [phi(:,1,1), phi(:,1,2), phi(:,1,3)];

% Settings
cfdVectorSkip = 0;

% Activate scale slider
handles.vectorScale.Enable = 'on';
handles.vectorScale.Value = 0.5;
cfdVectorScale = 2*handles.vectorScale.Value;

% Plot
if isCentroid
    theCentroids = cfdGetCentroidsForElements;
    
    x = theCentroids(:,1);
    y = theCentroids(:,2);
    z = theCentroids(:,3);
    
    vx = zeros(theNumberOfElements,1);
    vy = zeros(theNumberOfElements,1);
    vz = zeros(theNumberOfElements,1);
    
    if cfdVectorSkip==0
        for iElement=1:theNumberOfElements
            vx(iElement,1) = U(iElement,1);
            vy(iElement,1) = U(iElement,2);
            vz(iElement,1) = U(iElement,3);
        end
    else
        cfdVectorSkip = cfdVectorSkip + 1;
        for iElement=1:cfdVectorSkip:theNumberOfElements
            vx(iElement,1) = U(iElement,1);
            vy(iElement,1) = U(iElement,2);
            vz(iElement,1) = U(iElement,3);
        end
    end    
    
    quiver3(x,y,z,vx,vy,vz,cfdVectorScale,'Tag','VectorField','color','b');
else

    theElementCentroids = cfdGetCentroidsForElements;
    theBElementCentroids = cfdGetFaceCentroidsSubArrayForAllBoundaryPatchFaces;    
    
    theCentroids = [theElementCentroids; theBElementCentroids];
    
    x = theCentroids(:,1);
    y = theCentroids(:,2);
    z = theCentroids(:,3);
    
    vx = zeros(theNumberOfElements+theNumberOfBElements,1);
    vy = zeros(theNumberOfElements+theNumberOfBElements,1);
    vz = zeros(theNumberOfElements+theNumberOfBElements,1);
    
    if cfdVectorSkip==0
        for iElement=1:theNumberOfElements+theNumberOfBElements
            vx(iElement,1) = U(iElement,1);
            vy(iElement,1) = U(iElement,2);
            vz(iElement,1) = U(iElement,3);
        end
    else
        cfdVectorSkip = cfdVectorSkip + 1;
        for iElement=1:cfdVectorSkip:theNumberOfElements+theNumberOfBElements
            vx(iElement,1) = U(iElement,1);
            vy(iElement,1) = U(iElement,2);
            vz(iElement,1) = U(iElement,3);
        end
    end
    
    x = theNodeCentroids(:,1);
    y = theNodeCentroids(:,2);
    z = theNodeCentroids(:,3);
    
    vx = cfdInterpolateFromElementsToNodes(vx);
    vy = cfdInterpolateFromElementsToNodes(vy);
    vz = cfdInterpolateFromElementsToNodes(vz);
    
    quiver3(x,y,z,vx,vy,vz,cfdVectorScale,'Tag','VectorField','color','b');
end

axis equal;
axis off;


% --- Executes on button press in apply.
function apply_Callback(hObject, eventdata, handles)
% hObject    handle to apply (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global Region;

theSelectedFieldName = handles.fieldsList.String{handles.fieldsList.Value};
theTimeStep = str2double(handles.time.String);
timeIndex = find(cfdGetTimeSteps==theTimeStep);

isCentroid = handles.centroid.Value;
theNodeCentroids = cfdGetNodeCentroids;
theFaceNodeIndices = cfdGetFaceNodeIndices;
type = cfdGetFieldType(theSelectedFieldName);

if ~cfdIsFileExists(theSelectedFieldName, num2str(theTimeStep))
    return;
end

phi = Region.postProcessing.(theSelectedFieldName)(:,timeIndex,:);

if strcmp(type, 'volVectorField')
    phi = cfdMag([phi(:,1,1), phi(:,1,2), phi(:,1,3)]);
end

% Convert cell f to matrix form
theFaceNodeIndices = cfdConvertCelltoMatrix(theFaceNodeIndices);

handles.axes1;
cla;

edgeColor = 'none';
if handles.showMeshLines.Value==1
    edgeColor = [0 0 0];
end

opacity = handles.opacity.Value;

if isCentroid
    cdata = cfdInterpolateFromElementsToFaces('linear', phi);
    patch('vertices', theNodeCentroids, 'faces', theFaceNodeIndices, 'FaceVertexCData', cdata, 'FaceColor', 'flat', 'Tag', 'FieldContour','EdgeColor',edgeColor,'FaceAlpha',opacity);
else
    cdata = cfdInterpolateFromElementsToNodes(phi);
    patch('vertices', theNodeCentroids, 'faces', theFaceNodeIndices, 'FaceVertexCData', cdata, 'FaceColor', 'interp', 'Tag', 'FieldContour','EdgeColor',edgeColor,'FaceAlpha',opacity);
end

colorbar;
title(theSelectedFieldName);
colormap jet;

if handles.plotVectors.Value==1
    plotVectors_Callback(hObject, eventdata, handles);
end


% --- Executes on slider movement.
function opacity_Callback(hObject, eventdata, handles)
% hObject    handle to opacity (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'Value') returns position of slider
%        get(hObject,'Min') and get(hObject,'Max') to determine range of slider

obj = findobj('Tag','Mesh');
if ~isempty(obj)
    value = handles.opacity.Value;
    set(obj, 'FaceAlpha', value);
end

obj = findobj('Tag','FieldContour');
if ~isempty(obj)
    value = handles.opacity.Value;
    set(obj, 'FaceAlpha', value);
end


% --- Executes during object creation, after setting all properties.
function opacity_CreateFcn(hObject, eventdata, handles)
% hObject    handle to opacity (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: slider controls usually have a light gray background.
if isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor',[.9 .9 .9]);
end
