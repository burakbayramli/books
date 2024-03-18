function [handles]= SetRange(handles,RangeVar, PlotParam);

set(handles.sliderNodes,'Min',RangeVar(1,1)); 
set(handles.sliderNodes,'Value',RangeVar(1,2)); 
set(handles.sliderNodes,'Max',RangeVar(1,3));

set(handles.sliderInletArea,'Min',RangeVar(2,1)); 
set(handles.sliderInletArea,'Value',RangeVar(2,2)); 
set(handles.sliderInletArea,'Max',RangeVar(2,3));

set(handles.sliderOutletArea,'Min',RangeVar(3,1)); 
set(handles.sliderOutletArea,'Value',RangeVar(3,2)); 
set(handles.sliderOutletArea,'Max',RangeVar(3,3));

set(handles.sliderShockPos,'Min',RangeVar(4,1)); 
set(handles.sliderShockPos,'Value',RangeVar(4,2)); 
set(handles.sliderShockPos,'Max',RangeVar(4,3));

set(handles.sliderp01,'Min',RangeVar(5,1)); 
set(handles.sliderp01,'Value',RangeVar(5,2)); 
set(handles.sliderp01,'Max',RangeVar(5,3));

set(handles.slidert01,'Min',RangeVar(6,1)); 
set(handles.slidert01,'Value',RangeVar(6,2)); 
set(handles.slidert01,'Max',RangeVar(6,3));

set(handles.sliderp2,'Min',RangeVar(7,1)); 
set(handles.sliderp2,'Value',RangeVar(7,2)); 
set(handles.sliderp2,'Max',RangeVar(7,3));

set(handles.sliderTimestepsExplicit,'Min',RangeVar(8,1)); 
set(handles.sliderTimestepsExplicit,'Value',RangeVar(8,2)); 
set(handles.sliderTimestepsExplicit,'Max',RangeVar(8,3));

set(handles.sliderTimestepsImplicit,'Min',RangeVar(9,1)); 
set(handles.sliderTimestepsImplicit,'Value',RangeVar(9,2)); 
set(handles.sliderTimestepsImplicit,'Max',RangeVar(9,3));

set(handles.sliderVis2Explicit,'Min',RangeVar(10,1)); 
set(handles.sliderVis2Explicit,'Value',RangeVar(10,2)); 
set(handles.sliderVis2Explicit,'Max',RangeVar(10,3));

set(handles.sliderVis4Explicit,'Min',RangeVar(11,1)); 
set(handles.sliderVis4Explicit,'Value',RangeVar(11,2)); 
set(handles.sliderVis4Explicit,'Max',RangeVar(11,3));

set(handles.sliderVis2Implicit,'Min',RangeVar(10,1)); 
set(handles.sliderVis2Implicit,'Value',RangeVar(10,2)); 
set(handles.sliderVis2Implicit,'Max',RangeVar(10,3));
set(handles.sliderVis4Implicit,'Min',RangeVar(11,1)); 
set(handles.sliderVis4Implicit,'Value',RangeVar(11,2)); 
set(handles.sliderVis4Implicit,'Max',RangeVar(11,3));
set(handles.sliderCFLNumber,'Min',RangeVar(12,1)); 
set(handles.sliderCFLNumber,'Value',RangeVar(12,2)); 
set(handles.sliderCFLNumber,'Max',RangeVar(12,3));

set(handles.sliderPlotInterval,'Min',PlotParam(1,1)); 
set(handles.sliderPlotInterval,'Value',PlotParam(1,2)); 
set(handles.sliderPlotInterval,'Max',PlotParam(1,3));

%set(handles.radiobutton1,'value',1)
%set(handles.radiobutton2,'value',0)
%set(handles.radiobutton3,'value',0)
