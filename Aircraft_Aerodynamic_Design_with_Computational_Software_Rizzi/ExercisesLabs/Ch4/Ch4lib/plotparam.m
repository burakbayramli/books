function [handles]=plotparam(handles)
  disp(['plotparam: ',num2str(handles.Data.isReset)])
if handles.Data.isReset == 0
    gamma = handles.Data.gamma;
    R     = handles.Data.R;
    pL    = handles.Data.p01;
    pR    = handles.Data.p2;
    TL    = handles.Data.t01;
    TR    = TL;
    % geometry
    a = handles.Data.a;
    x = handles.Data.x;
    % exact steady
    pexact   = handles.Data.pex;
    rhoexact = handles.Data.rhoexact;
    M        = handles.Data.M;
    spofs    = handles.Data.spofs;
    uexact   = M.*spofs;
    %p        = handles.Data.p;
    % actual 
    convhist = handles.Data.convhist;
    W = handles.Data.W;
    np = size(x,1);
    if norm(a-mean(a))<= 0.001 % there's an unsteady exact solution
        xe = handles.Data.xe;
        ne = length(xe);
        p5 = handles.Data.p5;
        rho5 = handles.Data.rho5;
        u5 = handles.Data.u5;
        
        V = handles.Data.V;
        p3 = handles.Data.p3;
        rho3 = handles.Data.rho3;
        u3 = handles.Data.u3;
        
        C = handles.Data.C;
        p2 = handles.Data.pp2;
        rho2 = handles.Data.rho2;
        u2 = handles.Data.u2;
        tt = handles.Data.tim;
        xx = x-0.5*x(end);
        iL = xx < min(xe)*tt;
        i5 = (min(xe)*tt <= xx) & (xx < max(xe)*tt);
        i3 = (max(xe)*tt <= xx) & (xx < V*tt);
        i2 = (V*tt <= xx) & (xx < C*tt);
        iR = (C*tt <= xx);
        rhoexact = zeros(np,1);
        uexact   = zeros(np,1);
        pexact   = zeros(np,1);
        
        rhoexact(iL) = pL/(TL*R);
        pexact(iL)   = pL;
        uexact(iL)   = 0;
        
        pexact(i5)   = interp1(xe*tt,p5,xx(i5));
        rhoexact(i5) = interp1(xe*tt,rho5,xx(i5));
        uexact(i5)   = interp1(xe*tt,u5,xx(i5));
        % 
        rhoexact(i3) = rho3;
        pexact(i3)   = p3;
        uexact(i3)   = u3;
        
        rhoexact(i2) = rho2;
        pexact(i2)   = p2;
        uexact(i2)   = u2;
        
        rhoexact(iR) = pR/(R*TR);
        pexact(iR)   = pR;
        uexact(iR)   = 0;   
    end
    
    rho=W(1:end,1)./a;
    velocity=(W(1:end,2)./W(1:end,1));
    
    rrho     = a./W(:,1);
    rhou     = W(:,2)./a;
    rhoe     = W(:,3)./a;
    p        = (gamma-1).*(rhoe-0.5.*rhou.^2.*rrho);
    
    axes(handles.axes1);
    if get(handles.radiobuttonPressuredist,'value')
        plot(pexact,'r'), hold on, plot(p,'.-k'), hold off; end
    if get(handles.radiobuttonVelocitydist,'value')
        plot(uexact,'r'), hold on, plot(velocity,'.-k'), hold off; end
    if get(handles.radiobuttonDensitydist,'value')
        plot(rhoexact,'r'), hold on, plot(rho,'.-k'), hold off; end
    
    axes(handles.axes2);
    if get(handles.radiobuttonResidual,'value')
        semilogy(convhist);end
    if get(handles.radiobuttonAreadist,'value')
        plot(x,sqrt(a/pi),'b'); hold on; plot(x,-sqrt(a/pi),'b'); plot(x,a,'g'); hold off;
        axis([0 1 -max(a) max(a)]); end
    if get(handles.radiobuttonStepsizedist,'value')
        semilogy(handles.Data.dthist);end  
    
    set(handles.textNumberofIterations,'String',num2str(length(convhist)-1));
    tel = handles.Data.toc + handles.Data.tocold;
    set(handles.textTimeElapsed,'String',     sprintf('%6.2f',tel));
    set(handles.textTimeperIteration,'String',sprintf('%8.3e ',(tel/(length(convhist)-1))));
    set(handles.textResidual,'String',        sprintf('%6.2f',log10(handles.Data.convhist(end))));
    set(handles.textTimeStep,'String',        sprintf('%6.2e',handles.Data.dthist(end)));  
    drawnow
    handles.Data.iplot = 0;
end

if handles.Data.isReset == 1;
    axes(handles.axes2);
    plot(0)
    axes(handles.axes1);
    plot(0)
    set(handles.textNumberofIterations,'String',' ');
    set(handles.textTimeElapsed,'String',' ');
    set(handles.textTimeperIteration,'String',' ');
    set(handles.textResidual,'String',' ');
    set(handles.textTimeStep,'String',' '); 
end

