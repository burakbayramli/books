function out=bet(x,idx)

switch idx
    case 1
        out=8-min([x(4),x(3)+x(5)]);
    case 2
        out=8-min([x(5),x(3)+x(4)]);
    case 3
        out=8-min([x(1)+x(5),x(2)+x(4)]);
    case 4
        out=8-min([x(1),x(2)+x(3)]);
    case 5
        out=8-min([x(2),x(1)+x(3)]);
end

out=max(out,0);
