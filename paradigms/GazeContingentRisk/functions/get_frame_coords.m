function [ frame_coords ] = get_frame_coords(rectangle_coords, line_width)
%get_frame_coords returns the rectangle coordinates needed for a frame with
%the specified line width
line_width = .5 * line_width;
frame_coords = [rectangle_coords(1) - line_width, ...
    rectangle_coords(2) - line_width, ...
    rectangle_coords(3) + line_width, ...
    rectangle_coords(4) + line_width];
end