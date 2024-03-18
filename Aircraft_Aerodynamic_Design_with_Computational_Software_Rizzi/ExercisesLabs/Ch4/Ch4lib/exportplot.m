function [handles]=exportplot(handles)


if handles.Data.isReset == 0
pexact = handles.Data.pex;
rhoexact = handles.Data.rhoexact;
M = handles.Data.M;
p = handles.Data.p;
convhist = handles.Data.convhist;
a = handles.Data.a;
x = handles.Data.x;
spofs = handles.Data.spofs;
W = handles.Data.W;
gamma = handles.Data.gamma;

rho=W(1:end,1)./a;
velocity=(W(1:end,2)./W(1:end,1));

rrho     = a./W(:,1);
rhou     = W(:,2)./a;
rhoe     = W(:,3)./a;
p = (gamma-1).*(rhoe-0.5.*rhou.^2.*rrho);

figure(1);
axes(handles.axes1);
if get(handles.radiobuttonPressuredist,'value')
    figure(1),subplot(2,1,1),plot(pexact,'r'), hold on, plot(p,'.-'), xlabel('Pressure dist.'), hold off; end
if get(handles.radiobuttonVelocitydist,'value')
    figure(1),subplot(2,1,1),plot(M.*spofs,'M'), hold on, plot(velocity), xlabel('Velocity dist.'), hold off; end
if get(handles.radiobuttonDensitydist,'value')
    figure(1),subplot(2,1,1),plot(rhoexact,'r'), hold on, plot(rho), xlabel('Density dist.'), hold off; end


axes(handles.axes2);
if get(handles.radiobuttonResidual,'value')
    figure(1),subplot(2,1,2),semilogy(convhist), xlabel('Residual');end
if get(handles.radiobuttonAreadist,'value')
    figure(1),subplot(2,1,2),plot(x,sqrt(a/pi),'b'); hold on; plot(x,-sqrt(a/pi),'b'); plot(x,a,'g'); hold off, xlabel('Area dist.');
    axis([0 1 -max(a) max(a)]); end
if get(handles.radiobuttonStepsizedist,'value')
    figure(1),subplot(2,1,2),semilogy(handles.Data.dthist), xlabel('Stepsize dist.');end


set(handles.textNumberofIterations,'String',num2str(length(convhist)-1));
set(handles.textTimeElapsed,'String',[num2str(handles.Data.toc + handles.Data.tocold), 'sec']);
set(handles.textTimeperIteration,'String',[num2str((handles.Data.toc + handles.Data.tocold)/(length(convhist)-1)), 'sec']);
set(handles.textResidual,'String',[num2str(handles.Data.convhist(end)), 'sec']);
set(handles.textTimeStep,'String',[num2str(handles.Data.dthist(end)), 'sec']);

drawnow

handles.Data.iplot = 0;
end

if handles.Data.isReset == 1;
    axes(handles.axes2);
    figure(1),plot(0)
    axes(handles.axes1);
    figure(1),plot(0)
    
set(handles.textNumberofIterations,'String',' ');
set(handles.textTimeElapsed,'String',' ');
set(handles.textTimeperIteration,'String',' ');
set(handles.textResidual,'String',' ');
set(handles.textTimeStep,'String',' ');

end

