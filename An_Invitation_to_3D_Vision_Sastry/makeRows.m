% set up constraints for homography matrix
function [rows] = makeRows(x1, x2);

rows = [ 0 0 0    -x1'    x2(2)*x1';
        x1'  0 0 0    -x2(1)*x1']; 


