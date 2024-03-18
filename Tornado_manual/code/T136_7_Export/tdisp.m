function[]=tdisp(string)
%small function providing silent running of tornado

    output=config('verbose');
    if output
       disp(string)
    end
