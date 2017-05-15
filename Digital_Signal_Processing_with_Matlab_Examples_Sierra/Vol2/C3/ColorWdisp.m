% Function for frame display
% It is called by the timer
 
function ColorWdisp(obj, event,vid)
persistent IM;
persistent handlesRaw;
trigger(vid);
IM=getdata(vid,1,'uint8');
 
if isempty(handlesRaw) %if  first frame, set figure
   handlesRaw=imagesc(IM);
   title('Captured image');
else
   %updating
   set(handlesRaw,'CData',IM);
end