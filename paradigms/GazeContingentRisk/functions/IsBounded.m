function [ within_bounds ] = IsBounded( x, y, boundary)
%IsBounded checks to see if the (x,y) coordinates are bounded by the frame
%set by boundary
%   x,y are x and y positions
%   boundary is a 4x1 array: [xmin ymin xmax ymax]
    if ( (x > boundary(1)) && (x < boundary(3)) && (y > boundary(2)) && (y < boundary(4)) )
        within_bounds = 1;
    else
        within_bounds = 0;
    end
end

