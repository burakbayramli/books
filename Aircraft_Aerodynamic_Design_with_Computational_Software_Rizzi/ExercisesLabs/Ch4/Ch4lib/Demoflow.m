function varargout = Demoflow(varargin)
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

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name', mfilename, ...
    'gui_Singleton',  gui_Singleton, ...
    'gui_OpeningFcn', @Demoflow_OpeningFcn, ...
    'gui_OutputFcn',  @Demoflow_OutputFcn, ...
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
function Demoflow_OpeningFcn(hObject, eventdata, handles, varargin)
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
  handles = SetRange(handles,RangeVar,PlotParam);
  handles = SetEdit(handles);
  handles = SetRadiobutton(handles,'byShock');
  handles = SetRadiobutton(handles,'ExplicitRoe');
  handles = SetRadiobutton(handles,'Pressuredist');
  handles = SetRadiobutton(handles,'Residual');
  set(handles.radiobuttonbyState, 'enable','on');
  handles.Data=Data;
  handles.Data.isReset=1;
  
  get(hObject)
  get(hObject,'position')
%2.6469     1.3234   167.8109    47.6435
  set(hObject,'position',[5 5 150 40])
  set(hObject,'resize','on')
  set(hObject,'clipping','off')

  guidata(hObject,handles)
%guidata(hObject,Data)

% UIWAIT makes Demoflow wait for user response (see UIRESUME)
% uiwait(handles.figure1);

% --- Outputs from this function are returned to the command line.
function varargout = Demoflow_OutputFcn(hObject, eventdata, handles)
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
  varargout{1} = handles.output;


function pushbuttonSolve_Callback(hObject, eventdata, handles)

if handles.Data.isReset == 1;
    % Gets the values in the edit fields and stores it in the struc handles.Data
    handles = GetFields(handles);
    handles = InitEmpty(handles);
    handles = handle_grid(handles);
    handles = exact_solution(handles);
    handles = iniflow(handles);
    handles = bcond(handles);
    set(handles.pushbuttonSolve,'string','Solve');
    handles.Data.isReset=0;
    guidata(hObject,handles)
end

switch get(handles.pushbuttonSolve,'string')
    case 'Solve'
        tic;
        handles = GetFields(handles);
        set(handles.pushbuttonReset, 'enable','off');
        set(handles.pushbuttonSolve,'string','Pause');
        handles = SetRadiobutton(handles,'inSolution');
        guidata(hObject,handles)
        
        %Explicit Roe
        if get(handles.radiobuttonExplicitRoe,'value')
            cfl = handles.Data.CFLNumber;
            handles.Data.meth = 'eroe';
            handles = tstep(handles);
            while handles.Data.iter <= handles.Data.TimestepsExplicit && ...
                                       handles.Data.drho >= handles.Data.convtol
                if get(handles.pushbuttonSolve,'string') == 'Solve'
                    handles.Data.tocold = handles.Data.tocold + toc;
                    handles.Data.iter = 0;
                    guidata(hObject,handles)
                    return
                end
                
                handles.Data.Wold = handles.Data.W;
              
                for irk = 1:handles.Data.nrk
                  % disp([ min(handles.Data.dt) handles.Data.tim])
                    handles = update_p(handles);
                    handles = bcond(handles);
                    traceon = 0;
                    if traceon
                        figure(2)
                        clf
                        plotprim(handles);
                         hold on     
                    end
                    handles = lr_state(handles);
                    if traceon
                        plotlr(handles);
                        pause
                    end
                    handles = flux_roe(handles);
                    handles = srcterm(handles);
                    handles = res_ts(handles,irk);
                    handles = update_W(handles);
                end
                handles.Data.tim = handles.Data.tim + cfl*min(handles.Data.dt);
                
                handles = convergence(handles);
                handles.Data.dthist = [handles.Data.dthist; handles.Data.dt(1)];
                
                if handles.Data.iplot == handles.Data.PlotInterval
                    handles.Data.toc = toc;
                    handles = plotparam(handles);
                    guidata(hObject,handles)
                end
                handles.Data.iter = handles.Data.iter + 1;
                handles.Data.iplot = handles.Data.iplot + 1;
                %pause(0.00001)
            end
            
            if handles.Data.drho <= handles.Data.convtol
                set(handles.pushbuttonSolve,'string','Convergence');
                set(handles.pushbuttonReset, 'enable','on');
            end
            
            if handles.Data.iter >= handles.Data.TimestepsExplicit
                handles.Data.iter = 0;
                set(handles.pushbuttonSolve,'string','Solve');
                set(handles.pushbuttonReset, 'enable','on');
                guidata(hObject,handles)
            end
        end
        
        % Explicit Jameson
        if get(handles.radiobuttonExplicitJameson,'value')
            handles.Data.meth = 'ejam';
            handles = tstep(handles);
            cfl = handles.Data.CFLNumber;
            while handles.Data.iter <= handles.Data.TimestepsExplicit %&& handles.Data.drho >= handles.Data.convtol
                
                if get(handles.pushbuttonSolve,'string') == 'Solve'
                    handles.Data.tocold = handles.Data.tocold + toc;
                    handles.Data.iter = 0;
                    guidata(hObject,handles)
                    return
                end
                handles.Data.Wold = handles.Data.W;
                handles = RK(handles);
%                 for irk = 1:handles.Data.nrk
%                     handles = dissipation(handles);
%                     handles = flux_center(handles);
%                     handles = srcterm(handles);
%                     handles = res_ts(handles,irk);
%                     handles = update_W(handles);
%                     handles = update_p(handles);
%                     handles = bcond(handles);
%                 end
                handles.Data.tim = handles.Data.tim + cfl*min(handles.Data.dt);
                handles = convergence(handles);
                
                handles.Data.dthist = [handles.Data.dthist; handles.Data.dt(1)];
                if handles.Data.iplot == handles.Data.PlotInterval
                    handles.Data.toc = toc;
                    handles = plotparam(handles);
                    guidata(hObject,handles)
                end
                handles.Data.iter = handles.Data.iter + 1;
                handles.Data.iplot = handles.Data.iplot + 1;
                pause(0.0001)
            end
            
            if handles.Data.drho <= handles.Data.convtol
                set(handles.pushbuttonSolve,'string','Convergence');
                set(handles.pushbuttonReset, 'enable','on');
            end
            
            if handles.Data.iter >= handles.Data.TimestepsExplicit
                handles.Data.iter = 0;
                set(handles.pushbuttonSolve,'string','Solve');
                set(handles.pushbuttonReset, 'enable','on');
                guidata(hObject,handles)
            end
        end
        
        
        % Implicit Roe
        if get(handles.radiobuttonImplicitRoe,'value')
            handles.Data.meth = 'iroe';
            tic; handles.Data.roe = 1;
            global ImpVar
            
            handles = tstep(handles);
            handles.Data.dt = handles.Data.dt(1)*handles.Data.Nodes;
            
            ImpVar =  handles.Data;
            
            Y = jackev('MasterFunction',handles.Data.W(:));
            S=zeros(size(Y)); ii=find(Y); S(ii)=1; S=sparse(S);
            
            while handles.Data.iter <= handles.Data.TimestepsImplicit && handles.Data.drho >= handles.Data.convtol
                if get(handles.pushbuttonSolve,'string') == 'Solve'
                    handles.Data.tocold = handles.Data.tocold + toc;
                    handles.Data.iter = 0;
                    guidata(hObject,handles)
                    return
                end
                
                handles.Data.Wold = handles.Data.W;
                tmp = handles.Data.W;
                %tmp = tmp';
                [W,dt]= bwe1('MasterFunction',tmp(:),handles.Data.dt,handles.Data.tol,handles.Data.ntim,handles.Data.maxiter,S);
                
                W=reshape(W,length(W)/3,3);
                W(1,1:3)=W(2,1:3);
                W(end,1:3)=W(end-1,1:3);
                
                handles.Data.W=W; handles.Data.dt=dt;
                handles.Data.dthist = [handles.Data.dthist; dt];
                
                handles = convergence(handles);
                
                handles.Data.toc = toc;
                handles = plotparam(handles);
                guidata(hObject,handles)
                
                handles.Data.iter = handles.Data.iter + 1;
                pause(0.0001)
            end
            
            if handles.Data.drho <= handles.Data.convtol
                set(handles.pushbuttonSolve,'string','Convergence');
                set(handles.pushbuttonReset, 'enable','on');
            end
            
            if handles.Data.iter >= handles.Data.TimestepsImplicit
                handles.Data.iter = 0;
                set(handles.pushbuttonSolve,'string','Solve');
                set(handles.pushbuttonReset, 'enable','on');
                guidata(hObject,handles)
            end
        end
        
        % Implicit Jameson
        if get(handles.radiobuttonImplicitJameson,'value')
            handles.Data.meth = 'ijam';
            tic; handles.Data.roe = 0;
            global ImpVar
            
            handles = tstep(handles);
            handles.Data.dt = handles.Data.dt(1)*handles.Data.Nodes;
            
            ImpVar =  handles.Data;
            
            Y = jackev('MasterFunction',handles.Data.W(:));
            S=zeros(size(Y)); ii=find(Y); S(ii)=1; S=sparse(S);       
            
            while handles.Data.iter <= handles.Data.TimestepsImplicit && handles.Data.drho >= handles.Data.convtol
                if get(handles.pushbuttonSolve,'string') == 'Solve'
                    handles.Data.tocold = handles.Data.tocold + toc;
                    handles.Data.iter = 0;
                    guidata(hObject,handles)
                    return
                end
                
                handles.Data.Wold = handles.Data.W;
                
                [W,dt]= bwe1('MasterFunction',handles.Data.W(:),handles.Data.dt,handles.Data.tol,handles.Data.ntim,handles.Data.maxiter,S);
                dt
                W=reshape(W,length(W)/3,3);
                W(1,1:3)=W(2,1:3);
                W(end,1:3)=W(end-1,1:3);
                
                handles.Data.W=W; handles.Data.dt=dt;
                handles.Data.dthist = [handles.Data.dthist; dt];
                
                handles = convergence(handles);
                
                handles.Data.toc = toc;
                handles = plotparam(handles);
                guidata(hObject,handles)
                
                handles.Data.iter = handles.Data.iter + 1;
                pause(0.0001)
            end
            
            if handles.Data.drho <= handles.Data.convtol
                set(handles.pushbuttonSolve,'string','Convergence');
                set(handles.pushbuttonReset, 'enable','on');
            end
            
            if handles.Data.iter >= handles.Data.TimestepsImplicit
                handles.Data.iter = 0;
                set(handles.pushbuttonSolve,'string','Solve');
                set(handles.pushbuttonReset, 'enable','on');
                guidata(hObject,handles)
            end
        end
        
    case 'Pause'
        set(handles.pushbuttonReset, 'enable','on');
        set(handles.pushbuttonSolve,'string','Solve');
           
end

function pushbuttonExport_Callback(hObject, eventdata, handles)
  exportplot(handles);

% #### Reset Btn ####
function pushbuttonReset_Callback(hObject, eventdata, handles)
  [RangeVar, PlotParam] = indata();
  handles = SetRange(handles,RangeVar,PlotParam);
  handles = SetEdit(handles);
  handles = SetRadiobutton(handles,'noSolution');
  handles = SetRadiobutton(handles,'byShock');
%handles = SetRadiobutton(handles,'ExplicitRoe');
handles = SetRadiobutton(handles,'Pressuredist');
handles = SetRadiobutton(handles,'Residual');
set(handles.pushbuttonSolve,'string','Solve');
handles.Data.isReset=1;
handles = plotparam(handles);
guidata(hObject,handles)


% #### Exit Btn ####
function pushbuttonExit_Callback(hObject, eventdata, handles)
close(gcf)

% #### Sliders and Edits ####
function sliderNodes_Callback(hObject, eventdata, handles)
set(handles.editNodes,'String',num2str(round(get(handles.sliderNodes,'Value'))));
%kalle = get(handles.sliderNodes,'Value')

function sliderNodes_CreateFcn(hObject, eventdata, handles)

function editNodes_Callback(hObject, eventdata, handles)
SetSlider(handles.sliderNodes,hObject,'round')

function editNodes_CreateFcn(hObject, eventdata, handles)

function sliderInletArea_Callback(hObject, eventdata, handles)
set(handles.editInletArea,'String',num2str(get(handles.sliderInletArea,'Value')));

function sliderInletArea_CreateFcn(hObject, eventdata, handles)

function editInletArea_Callback(hObject, eventdata, handles)
SetSlider(handles.sliderInletArea,hObject,'float')

function editInletArea_CreateFcn(hObject, eventdata, handles)

function sliderOutletArea_Callback(hObject, eventdata, handles)
set(handles.editOutletArea,'String',num2str(get(handles.sliderOutletArea,'Value')));

function sliderOutletArea_CreateFcn(hObject, eventdata, handles)

function editOutletArea_Callback(hObject, eventdata, handles)
SetSlider(handles.sliderOutletArea,hObject,'float')

function editOutletArea_CreateFcn(hObject, eventdata, handles)

function sliderShockPos_Callback(hObject, eventdata, handles)
set(handles.editShockPos,'String',num2str(get(handles.sliderShockPos,'Value')));

function sliderShockPos_CreateFcn(hObject, eventdata, handles)

function editShockPos_Callback(hObject, eventdata, handles)
SetSlider(handles.sliderShockPos,hObject,'float')

function editShockPos_CreateFcn(hObject, eventdata, handles)

function sliderp01_Callback(hObject, eventdata, handles)
set(handles.editp01,'String',num2str(get(handles.sliderp01,'Value')));

function sliderp01_CreateFcn(hObject, eventdata, handles)

function editp01_Callback(hObject, eventdata, handles)
SetSlider(handles.sliderp01,hObject,'float')

function editp01_CreateFcn(hObject, eventdata, handles)

function slidert01_Callback(hObject, eventdata, handles)
set(handles.editt01,'String',num2str(get(handles.slidert01,'Value')));

function slidert01_CreateFcn(hObject, eventdata, handles)

function editt01_Callback(hObject, eventdata, handles)
SetSlider(handles.slidert01,hObject,'float')

function editt01_CreateFcn(hObject, eventdata, handles)

function sliderp2_Callback(hObject, eventdata, handles)
set(handles.editp2,'String',num2str(get(handles.sliderp2,'Value')));

function sliderp2_CreateFcn(hObject, eventdata, handles)

function editp2_Callback(hObject, eventdata, handles)
SetSlider(handles.sliderp2,hObject,'float')

function editp2_CreateFcn(hObject, eventdata, handles)

function sliderTimestepsExplicit_Callback(hObject, eventdata, handles)
set(handles.editTimestepsExplicit,'String',num2str(round(get(handles.sliderTimestepsExplicit,'Value'))));

function sliderTimestepsExplicit_CreateFcn(hObject, eventdata, handles)

function editTimestepsExplicit_Callback(hObject, eventdata, handles)
SetSlider(handles.sliderTimestepsExplicit,hObject,'float')

function editTimestepsExplicit_CreateFcn(hObject, eventdata, handles)

function sliderTimestepsImplicit_Callback(hObject, eventdata, handles)
set(handles.editTimestepsImplicit,'String',num2str(round(get(handles.sliderTimestepsImplicit,'Value'))));
function sliderTimestepsImplicit_CreateFcn(hObject, eventdata, handles)

function editTimestepsImplicit_Callback(hObject, eventdata, handles)
SetSlider(handles.sliderTimestepsImplicit,hObject,'float')
function editTimestepsImplicit_CreateFcn(hObject, eventdata, handles)

function sliderVis2Explicit_Callback(hObject, eventdata, handles)
set(handles.editVis2Explicit,'String',num2str(get(handles.sliderVis2Explicit,'Value')));
function sliderVis2Explicit_CreateFcn(hObject, eventdata, handles)

function editVis2Explicit_Callback(hObject, eventdata, handles)
SetSlider(handles.sliderVis2Explicit,hObject,'float')
function editVis2Explicit_CreateFcn(hObject, eventdata, handles)

function sliderVis2Implicit_Callback(hObject, eventdata, handles)
set(handles.editVis2Implicit,'String',num2str(get(handles.sliderVis2Implicit,'Value')));
function sliderVis2Implicit_CreateFcn(hObject, eventdata, handles)

function editVis2Implicit_Callback(hObject, eventdata, handles)
SetSlider(handles.sliderVis2Implicit,hObject,'float')

function editVis2Implicit_CreateFcn(hObject, eventdata, handles)

function sliderVis4Implicit_Callback(hObject, eventdata, handles)
set(handles.editVis4Implicit,'String',num2str(get(handles.sliderVis4Implicit,'Value')));

function sliderVis4Implicit_CreateFcn(hObject, eventdata, handles)

function editVis4Implicit_Callback(hObject, eventdata, handles)
SetSlider(handles.sliderVis4Implicit,hObject,'float')

function editVis4Implicit_CreateFcn(hObject, eventdata, handles)

function sliderVis4Explicit_Callback(hObject, eventdata, handles)
set(handles.editVis4Explicit,'String',num2str(get(handles.sliderVis4Explicit,'Value')));

function sliderVis4Explicit_CreateFcn(hObject, eventdata, handles)

function editVis4Explicit_Callback(hObject, eventdata, handles)
SetSlider(handles.sliderVis4Explicit,hObject,'float')

function editVis4Explicit_CreateFcn(hObject, eventdata, handles)

function sliderCFLNumber_Callback(hObject, eventdata, handles)
set(handles.editCFLNumber,'String',num2str(get(handles.sliderCFLNumber,'Value')));

function sliderCFLNumber_CreateFcn(hObject, eventdata, handles)

function editCFLNumber_Callback(hObject, eventdata, handles)
SetSlider(handles.sliderCFLNumber,hObject,'float')

function editCFLNumber_CreateFcn(hObject, eventdata, handles)

function sliderPlotInterval_Callback(hObject, eventdata, handles)
set(handles.editPlotInterval,'String',num2str(round(get(handles.sliderPlotInterval,'Value'))));

function sliderPlotInterval_CreateFcn(hObject, eventdata, handles)

function editPlotInterval_Callback(hObject, eventdata, handles)
SetSlider(handles.sliderPlotInterval,hObject,'float')

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









