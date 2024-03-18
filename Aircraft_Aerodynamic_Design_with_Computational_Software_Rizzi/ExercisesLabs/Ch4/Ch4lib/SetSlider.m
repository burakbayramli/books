function [] = SetSlider(slider,edit,str)

if str == 'round'
val = round(str2double(get(edit,'String')));
set(edit,'String',val);
else
val=(str2double(get(edit,'String')));
end

if isnumeric(val) & length(val)==1 & ...
        val >= get(slider,'Min') & ...
        val <= get(slider,'Max')
    set(slider,'Value',val);
else
    if val > get(slider,'Max')
        set(edit,'String', get(slider,'Max'));
        set(slider,'Value',get(slider,'Max'));
    else
        set(edit,'String', get(slider,'Min'));
        set(slider,'Value',get(slider,'Min'));
    end
end

