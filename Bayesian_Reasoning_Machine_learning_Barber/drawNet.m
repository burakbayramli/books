function [x, y, h]=drawNet(A,varargin)
%DRAWNET plot network
% drawNet(A,<varinf>)
cla
if nargin==1
    [x, y, h]=draw_layout(A);
else
    [x, y, h]=draw_layout(A,field2cell(varargin{1},'name'));
end