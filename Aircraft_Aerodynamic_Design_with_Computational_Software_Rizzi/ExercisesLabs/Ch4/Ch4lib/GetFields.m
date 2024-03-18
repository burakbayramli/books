function [handles] = GetFields(handles)

if handles.Data.isReset == 1
handles.Data.Nodes = str2num(get(handles.editNodes,'String'));
handles.Data.InletArea  = str2num(get(handles.editInletArea,'String'));
handles.Data.OutletArea = str2num(get(handles.editOutletArea,'String'));
handles.Data.ShockPos   = str2num(get(handles.editShockPos,'String'));
handles.Data.p01 = str2num(get(handles.editp01,'String'));
handles.Data.t01 = str2num(get(handles.editt01,'String'));
handles.Data.p2  = str2num(get(handles.editp2,'String'));

handles.Data.TimestepsExplicit = str2num(get(handles.editTimestepsExplicit,'String'));
handles.Data.TimestepsImplicit = str2num(get(handles.editTimestepsImplicit,'String'));
handles.Data.Vis2Explicit = str2num(get(handles.editVis2Explicit,'String'));
handles.Data.Vis4Explicit = str2num(get(handles.editVis4Explicit,'String'));
handles.Data.Vis2Implicit = str2num(get(handles.editVis2Implicit,'String'));
handles.Data.Vis4Implicit = str2num(get(handles.editVis4Implicit,'String'));
handles.Data.CFLNumber    = str2num(get(handles.editCFLNumber,'String'));

handles.Data.PlotInterval = str2num(get(handles.editPlotInterval,'String'));
end

if handles.Data.isReset == 0
handles.Data.TimestepsExplicit = str2num(get(handles.editTimestepsExplicit,'String'));
handles.Data.TimestepsImplicit = str2num(get(handles.editTimestepsImplicit,'String'));
handles.Data.Vis2Explicit = str2num(get(handles.editVis2Explicit,'String'));
handles.Data.Vis4Explicit = str2num(get(handles.editVis4Explicit,'String'));
handles.Data.Vis2Implicit = str2num(get(handles.editVis2Implicit,'String'));
handles.Data.Vis4Implicit = str2num(get(handles.editVis4Implicit,'String'));
handles.Data.CFLNumber    = str2num(get(handles.editCFLNumber,'String'));

handles.Data.PlotInterval = str2num(get(handles.editPlotInterval,'String'));
end

