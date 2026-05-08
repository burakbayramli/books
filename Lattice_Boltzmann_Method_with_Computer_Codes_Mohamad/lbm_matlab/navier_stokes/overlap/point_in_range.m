function in_range = point_in_range(point, point_range)
% simple function to test whether a point value lies in 1d point_range.

in_range = point >= point_range(1) && point <= point_range(2);