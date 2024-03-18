function pushbuttonReset_Callback(hObject,eventdata,handles)
  handles = guidata(hObject);
  disp('reset pushed')
  [RangeVar, PlotParam] = indata();
 % handles = SetRange(handles,RangeVar,PlotParam);
  handles = SetEdit(handles);
  handles = SetRadiobutton(handles,'noSolution');
  handles = SetRadiobutton(handles,'byShock');
  handles = SetRadiobutton(handles,'ExplicitRoe');
  handles = SetRadiobutton(handles,'Pressuredist');
  handles = SetRadiobutton(handles,'Residual');
  set(handles.pushbuttonSolve,'string','Solve');
  handles.Data.isReset=1;
  set(handles.pushbuttonReset,'string','IsReset')
  handles = plotparam(handles);
  guidata(hObject,handles);