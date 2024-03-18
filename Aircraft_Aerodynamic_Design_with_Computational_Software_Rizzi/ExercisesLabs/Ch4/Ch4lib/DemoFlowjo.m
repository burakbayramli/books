function varargout = DemoFlowjo(varargin)
% DEMOFLOW M-file for Demoflow.fig
%      DEMOFLOW, by itself, creates a new DEMOFLOW or raises the existing
%      singleton*.
%
%      H = DEMOFLOW returns the handle to a new DEMOFLOW or the handle to
%      the existing singleton*.
%
%      DEMOFLOW('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in DEMOFLOW.M with the given input arguments.
%
%      DEMOFLOW('Property','Value',...) creates a new DEMOFLOW or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before Demoflow_OpeningFunction gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to Demoflow_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Copyright 2002-2003 The MathWorks, Inc.

% Edit the above text to modify the response to help Demoflow

% Last Modified by GUIDE v2.5 13-Jun-2006 15:12:56
% modified JO 20015 to work with octave and GUI created w/o GUIDE
% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name', mfilename, ...
    'gui_Singleton',  gui_Singleton, ...
    'gui_OpeningFcn', @DemoFlowjo_OpeningFcn, ...
    'gui_OutputFcn',  @DemoFlowjo_OutputFcn, ...
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


% --- Executes just before Demoflow is made visible.
function DemoFlowjo_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to Demoflow (see VARARGIN)

% Choose default command line output for Demoflow
  handles.output = hObject;
% Update handles structure
  guidata(hObject, handles);
%clc
  [RangeVar, PlotParam, Data] = indata();
%  handles = SetRange(handles,RangeVar,PlotParam); % no sliders
  handles = SetEdit(handles);
  handles = SetRadiobutton(handles,'byShock');
  handles = SetRadiobutton(handles,'ExplicitRoe');
  handles = SetRadiobutton(handles,'Pressuredist');
  handles = SetRadiobutton(handles,'Residual');
  set(handles.radiobuttonbyState, 'enable','on');
  handles.Data=Data;
  handles.Data.isReset=1;
  set(handles.pushbuttonReset,'string','IsReset');
  set(hObject,'resize','on')
  set(hObject,'clipping','off')
  hhh = findobj(hObject,'type','axes')
  handles.axes1 = hhh(1);
  handles.axes2 = hhh(2);
  
  %guidata(hObject,Data)
  
  set(handles.pushbuttonReset, 'callback',{@pushbuttonReset_Callback,handles});
  set(handles.pushbuttonExit,  'callback',{@pushbuttonExit_Callback,handles});
  set(handles.pushbuttonSolve, 'callback',{@pushbuttonSolve_Callback,handles});
  
  set(handles.radiobuttonImplicitJameson,'callback',{@radiobuttonImplicitJameson_Callback,handles});
  set(handles.radiobuttonExplicitJameson,'callback',{@radiobuttonExplicitJameson_Callback,handles});
  set(handles.radiobuttonImplicitRoe,    'callback',{@radiobuttonImplicitRoe_Callback,handles});
  set(handles.radiobuttonExplicitRoe,    'callback',{@radiobuttonExplicitRoe_Callback,handles});
  
  set(handles.radiobuttonbyShock,'callback', {@radiobuttonbyShock_Callback,handles});
  set(handles.radiobuttonbyState,'callback', {@radiobuttonbyState_Callback,handles});
  
  set(handles.radiobuttonPressuredist,'callback', {@radiobuttonPressuredist_Callback,handles});
  set(handles.radiobuttonVelocitydist,'callback', {@radiobuttonVelocitydist_Callback,handles});
  set(handles.radiobuttonAreadist,'callback', {@radiobuttonAreadist_Callback,handles});
  
  set(handles.radiobuttonDensitydist,'callback', {@radiobuttonDensitydist_Callback,handles});
  set(handles.radiobuttonResidual,'callback', {@radiobuttonResidual_Callback,handles});
  set(handles.radiobuttonStepsizedist,'callback', {@radiobuttonStepsizedist_Callback,handles});
  guidata(hObject,handles);
  disp('openfcn done')
  
% UIWAIT makes Demoflow wait for user response (see UIRESUME)
% uiwait(handles.figure1);
%uiwait([])

% --- Outputs from this function are returned to the command line.
function varargout = DemoFlowjo_OutputFcn(hObject, eventdata, handles)
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% Get default command line output from handles structure
 varargout{1} = handles.output;

function pushbuttonExport_Callback(hObject, eventdata, handles)
  exportplot(handles);

% #### Edits ####

function editNodes_Callback(hObject, eventdata, handles)

function editNodes_CreateFcn(hObject, eventdata, handles)

function editInletArea_Callback(hObject, eventdata, handles)

function editInletArea_CreateFcn(hObject, eventdata, handles)

function editOutletArea_Callback(hObject, eventdata, handles)

function editOutletArea_CreateFcn(hObject, eventdata, handles)

function editShockPos_Callback(hObject, eventdata, handles)

function editShockPos_CreateFcn(hObject, eventdata, handles)

function editp01_Callback(hObject, eventdata, handles)

function editp01_CreateFcn(hObject, eventdata, handles)

function editt01_Callback(hObject, eventdata, handles)

function editt01_CreateFcn(hObject, eventdata, handles)

function editp2_Callback(hObject, eventdata, handles)

function editp2_CreateFcn(hObject, eventdata, handles)

function editTimestepsExplicit_Callback(hObject, eventdata, handles)

function editTimestepsExplicit_CreateFcn(hObject, eventdata, handles)

function editTimestepsImplicit_Callback(hObject, eventdata, handles)

function editTimestepsImplicit_CreateFcn(hObject, eventdata, handles)

function editVis2Explicit_Callback(hObject, eventdata, handles)

function editVis2Explicit_CreateFcn(hObject, eventdata, handles)

function editVis2Implicit_Callback(hObject, eventdata, handles)

function editVis2Implicit_CreateFcn(hObject, eventdata, handles)

function editVis4Implicit_Callback(hObject, eventdata, handles)

function editVis4Implicit_CreateFcn(hObject, eventdata, handles)

function editVis4Explicit_Callback(hObject, eventdata, handles)

function editVis4Explicit_CreateFcn(hObject, eventdata, handles)

function editCFLNumber_Callback(hObject, eventdata, handles)

function editCFLNumber_CreateFcn(hObject, eventdata, handles)
  
function editPlotInterval_Callback(hObject, eventdata, handles)

function editPlotInterval_CreateFcn(hObject, eventdata, handles)

% #### Radiobuttons ####
function radiobuttonbyShock_Callback(hObject, eventdata, handles)
handles = SetRadiobutton(handles,'byShock');

function radiobuttonbyState_Callback(hObject, eventdata, handles)
handles = SetRadiobutton(handles,'byState');

function radiobuttonExplicitRoe_Callback(hObject, eventdata, handles)
handles = SetRadiobutton(handles,'ExplicitRoe');

function radiobuttonExplicitJameson_Callback(hObject, eventdata, handles)
handles = SetRadiobutton(handles,'ExplicitJameson');

function radiobuttonImplicitRoe_Callback(hObject, eventdata, handles)
handles = SetRadiobutton(handles,'ImplicitRoe');

function radiobuttonImplicitJameson_Callback(hObject, eventdata, handles)
handles = SetRadiobutton(handles,'ImplicitJameson');

function radiobuttonPressuredist_Callback(hObject, eventdata, handles)
handles = SetRadiobutton(handles,'Pressuredist');
handles = plotparam(handles);

function radiobuttonVelocitydist_Callback(hObject, eventdata, handles)
handles = SetRadiobutton(handles,'Velocitydist');
handles = plotparam(handles);

function radiobuttonDensitydist_Callback(hObject, eventdata, handles)
handles = SetRadiobutton(handles,'Densitydist');
handles = plotparam(handles);

function radiobuttonResidual_Callback(hObject, eventdata, handles)
handles = SetRadiobutton(handles,'Residual');
handles = plotparam(handles);

function radiobuttonAreadist_Callback(hObject, eventdata, handles)
handles = SetRadiobutton(handles,'Areadist');
handles = plotparam(handles);

function radiobuttonStepsizedist_Callback(hObject, eventdata, handles)
handles = SetRadiobutton(handles,'Stepsizedist');
handles = plotparam(handles);
