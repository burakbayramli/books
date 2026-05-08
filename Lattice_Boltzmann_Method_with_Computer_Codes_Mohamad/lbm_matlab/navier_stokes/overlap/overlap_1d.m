function overlap = overlap_1d(range1, range2)
% range1 and range2 are length-2 vectors indicating a range.
% the ranges are ordered [min, max].
% if range2(1) is in range1, then overlap(1) = 1
% if range2(2) is in range1, then overlap(2) = 1

overlap = [ ...
    range2(1) <= range1(2) && range2(1) >= range1(1), ...
    range2(2) <= range1(2) && range2(2) >= range1(1) ...
    ];

