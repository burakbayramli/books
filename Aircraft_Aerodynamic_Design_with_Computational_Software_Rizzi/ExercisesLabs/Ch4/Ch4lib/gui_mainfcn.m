function varargout = gui_mainfcn (gui_State, varargin)
  if (nargin == 1 || isempty (gui_State.gui_Callback))
    filename = file_in_loadpath ([gui_State.gui_Name ".fig"])
    copies = ifelse (gui_State.gui_Singleton, "reuse", "new");
    H = openfig (filename, copies, "invisible");
  %  drawnow
    for i = 1:2:numel (varargin)
      try
        set(H, varargin{i}, varargin{i+1});
      catch
        break;
      end_try_catch
    endfor
    
    handles = guihandles (H);
    guidata (H, handles);

    feval (gui_State.gui_OpeningFcn, H, [], handles, varargin{:});
    set (H, "Visible", "on");
    handles = guidata (H);
    if nargout > 0
      varargout{1} = feval (gui_State.gui_OutputFcn, H, [], handles);
    end
  else
    [varargout{1:nargout}] = feval (gui_State.gui_Callback, varargin{2:end});
  endif
endfunction