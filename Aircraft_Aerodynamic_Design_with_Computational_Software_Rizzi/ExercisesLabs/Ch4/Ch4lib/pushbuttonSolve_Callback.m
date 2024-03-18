function pushbuttonSolve_Callback(hObject,eventdata,handles)
  handles = guidata(hObject);
  if get(handles.pushbuttonReset,'string')=='IsReset'
    % Gets the values in the edit fields and stores it in the struc handles.Data
    handles = GetFields(handles);
    handles = InitEmpty(handles);
    handles = handle_grid(handles);
    handles = exact_solution(handles);
    handles = iniflow(handles);
    handles = bcond(handles);
    set(handles.pushbuttonSolve,'string','Solve');
    set(handles.pushbuttonReset,'string','DoReset');
    handles.Data.isReset=0;
    guidata(hObject,handles);
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
          'eroe'
            cfl = handles.Data.CFLNumber;
            handles.Data.meth = 'eroe';
            handles = tstep(handles);
         %   'tstep'
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
        %          'irkloop'
                  % disp([ min(handles.Data.dt) handles.Data.tim])
                    handles = update_p(handles);
        %            'updatep'
                    handles = bcond(handles);
         %           'bcond'
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
           %         'flxroe'
                    handles = srcterm(handles);
            %        'srct'
                    handles = res_ts(handles,irk);
            %        'rests'
                    handles = update_W(handles);
            %        'updw'
                end
                handles.Data.tim = handles.Data.tim + cfl*min(handles.Data.dt);
                
                handles = convergence(handles);
            %    'conv'
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
            guidata(hObject,handles)
        end
        
        % Explicit Jameson
        if get(handles.radiobuttonExplicitJameson,'value')
            handles.Data.meth = 'ejam';
            handles = tstep(handles);
            cfl = handles.Data.CFLNumber;
            while handles.Data.iter <= handles.Data.TimestepsExplicit && handles.Data.drho >= handles.Data.convtol
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
    