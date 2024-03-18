function handles = SetRadiobutton(handles,button);
% jo 2005 - no sliders
disp(['setradbut: ' button])
switch button
    case 'ExplicitRoe'
        set(handles.radiobuttonExplicitRoe,'value',    1);
        set(handles.radiobuttonExplicitJameson,'value',0);
        set(handles.radiobuttonImplicitRoe,'value'    ,0);
        set(handles.radiobuttonImplicitJameson,'value',0);
        
        set(handles.editTimestepsExplicit,'enable','on');
        set(handles.editVis2Explicit,     'enable','on');
        set(handles.editVis4Explicit,     'enable','on');
        set(handles.editCFLNumber,        'enable','on');
        set(handles.editPlotInterval,     'enable','on');
        
        set(handles.editTimestepsImplicit,'enable','off');
        set(handles.editVis2Implicit,     'enable','off');
        set(handles.editVis4Implicit,     'enable','off');
        
    case 'ExplicitJameson'
        set(handles.radiobuttonExplicitRoe,'value'    ,0);
        set(handles.radiobuttonExplicitJameson,'value',1);
        set(handles.radiobuttonImplicitRoe,'value'    ,0);
        set(handles.radiobuttonImplicitJameson,'value',0);
        
        set(handles.editTimestepsExplicit,'enable', 'on');
        set(handles.editVis2Explicit,'enable',      'on');
        set(handles.editVis4Explicit,'enable',      'on');
        set(handles.editCFLNumber,'enable',         'on');
        set(handles.editPlotInterval,'enable',      'on');
        
        set(handles.editTimestepsImplicit,'enable','off');
        set(handles.editVis2Implicit,'enable',     'off');
        set(handles.editVis4Implicit,'enable',     'off');
        
    case 'ImplicitRoe'
        set(handles.radiobuttonExplicitRoe,    'value',0);
        set(handles.radiobuttonExplicitJameson,'value',0);
        set(handles.radiobuttonImplicitRoe,    'value',1);
        set(handles.radiobuttonImplicitJameson,'value',0);
        
        set(handles.editTimestepsExplicit,'enable','off');
        set(handles.editVis2Explicit,     'enable','off');
        set(handles.editVis4Explicit,     'enable','off');
        set(handles.editCFLNumber,        'enable','off');
  
        set(handles.editPlotInterval,        'enable','on');
        set(handles.editTimestepsImplicit,   'enable','on');
        set(handles.editVis2Implicit,        'enable','on');
        set(handles.editVis4Implicit,        'enable','on');
        
    case 'ImplicitJameson'
        set(handles.radiobuttonExplicitRoe,    'value',0);
        set(handles.radiobuttonExplicitJameson,'value',0);
        set(handles.radiobuttonImplicitRoe,    'value',0);
        set(handles.radiobuttonImplicitJameson,'value',1);
        
        set(handles.editTimestepsExplicit,'enable','off');
        set(handles.editVis2Explicit,     'enable','off');
        set(handles.editVis4Explicit,     'enable','off');
        set(handles.editCFLNumber,        'enable','off');
        
        set(handles.editPlotInterval,     'enable','on');
        set(handles.editTimestepsImplicit,'enable','on');
        set(handles.editVis2Implicit,     'enable','on');
        set(handles.editVis4Implicit,     'enable','on');
        
    case 'Pressuredist'
        set(handles.radiobuttonPressuredist,'value',1);
        set(handles.radiobuttonVelocitydist,'value',0);
        set(handles.radiobuttonDensitydist,'value',0);
        
    case 'Velocitydist'
        set(handles.radiobuttonPressuredist,'value',0);
        set(handles.radiobuttonVelocitydist,'value',1);
        set(handles.radiobuttonDensitydist,'value',0);
        
    case 'Densitydist'
        set(handles.radiobuttonPressuredist,'value',0);
        set(handles.radiobuttonVelocitydist,'value',0);
        set(handles.radiobuttonDensitydist,'value',1);
        
    case 'byShock'
        set(handles.radiobuttonbyShock,'value',1);
        set(handles.radiobuttonbyState,'value',0);
        
        set(handles.editp01,'enable','off');
        set(handles.editt01,'enable','off');
        set(handles.editp2,'enable','off');
        
        set(handles.editShockPos,'enable','on');
        
    case 'byState'
        set(handles.radiobuttonbyShock,'value',0);
        set(handles.radiobuttonbyState,'value',1);
        
        set(handles.editp01, 'enable','on');
        set(handles.editt01, 'enable','on');
        set(handles.editp2,  'enable','on');
        
        set(handles.editShockPos,'enable','off');
        
    case 'Residual'
        set(handles.radiobuttonResidual,'value',1);
        set(handles.radiobuttonAreadist,'value',0);
        set(handles.radiobuttonStepsizedist,'value',0);
        
    case 'Areadist'
        set(handles.radiobuttonResidual,'value',0);
        set(handles.radiobuttonAreadist,'value',1);
        set(handles.radiobuttonStepsizedist,'value',0);
        
    case 'Stepsizedist'
        set(handles.radiobuttonResidual,'value',0);
        set(handles.radiobuttonAreadist,'value',0);
        set(handles.radiobuttonStepsizedist,'value',1);
        
    case 'inSolution'
        set(handles.editNodes,     'enable','off');
        set(handles.editInletArea, 'enable','off');
        set(handles.editOutletArea,'enable','off');
        set(handles.editp01,       'enable','off');
        set(handles.editt01,       'enable','off');
        set(handles.editp2,        'enable','off');
        set(handles.editShockPos,  'enable','off');
        
    case 'noSolution'
        set(handles.editNodes,     'enable','on');
        set(handles.editInletArea, 'enable','on');
        set(handles.editOutletArea,'enable','on');
        set(handles.editp01,       'enable','on');
        set(handles.editt01,       'enable','on');
        set(handles.editp2,        'enable','on');
        set(handles.editShockPos,  'enable','on');
end
