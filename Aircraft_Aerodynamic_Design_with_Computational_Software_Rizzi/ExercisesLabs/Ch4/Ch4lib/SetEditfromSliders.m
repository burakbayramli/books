function [handles]=SetEditfromSliders(handles);

set(handles.editNodes,'String',num2str(get(handles.sliderNodes,'Value')));
set(handles.editInletArea,'String',num2str(get(handles.sliderInletArea,'Value')));
set(handles.editOutletArea,'String',num2str(get(handles.sliderOutletArea,'Value')));
set(handles.editShockPos,'String',num2str(get(handles.sliderShockPos,'Value')));
set(handles.editp01,'String',num2str(get(handles.sliderp01,'Value')));
set(handles.editt01,'String',num2str(get(handles.slidert01,'Value')));
set(handles.editp2,'String',num2str(get(handles.sliderp2,'Value')));
set(handles.editTimestepsExplicit,'String',num2str(get(handles.sliderTimestepsExplicit,'Value')));
set(handles.editTimestepsImplicit,'String',num2str(get(handles.sliderTimestepsImplicit,'Value')));
set(handles.editVis2Explicit,'String',num2str(get(handles.sliderVis2Explicit,'Value')));
set(handles.editVis4Explicit,'String',num2str(get(handles.sliderVis4Explicit,'Value')));
set(handles.editVis2Implicit,'String',num2str(get(handles.sliderVis2Implicit,'Value')));
set(handles.editVis4Implicit,'String',num2str(get(handles.sliderVis4Implicit,'Value')));
set(handles.editCFLNumber,'String',num2str(get(handles.sliderCFLNumber,'Value')));

set(handles.editPlotInterval,'String',num2str(get(handles.sliderPlotInterval,'Value')));

