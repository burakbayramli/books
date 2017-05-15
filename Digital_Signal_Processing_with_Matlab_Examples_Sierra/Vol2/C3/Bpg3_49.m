%Webcam image acquisition

%Preparation--------------------------------------------

Nfs=10; % set frame rate per second
hFigure=figure(1); %set figure with handle
%set webcam driver 
try   
   vid = videoinput('winvideo', 1);
catch
   errordlg('No webcam available');
end
%set video parameters 
set(vid,'FramesPerTrigger',1); %capture one frame each time
set(vid,'TriggerRepeat',Inf); %loop until stop

%set color acquisition
%set(vid,'ReturnedColorSpace','grayscale');
set(vid,'ReturnedColorSpace','rgb');

triggerconfig(vid, 'Manual');

% set timer (it uses the function ColorWdisp)
TimerData=timer('TimerFcn', {@ColorWdisp,vid},'Period',1/Nfs,'ExecutionMode','fixedRate','BusyMode','drop');

% Work-------------------------------------------
% Start video and timer object
start(vid);
start(TimerData);
 
% Continue until the figure is closed
uiwait(hFigure);
 
% Clean everything
stop(TimerData);
delete(TimerData);
stop(vid);
delete(vid);
clear functions;
