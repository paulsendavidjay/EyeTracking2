function [items_on_x,items_on_y,maxd] = find_geometry(numerosity,sparse,aspect)

%{
FUNCTION find_geometry() CALCULATES THE DIMENSIONS IN DOT-UNITS NEEDED 
TO DISPLAY THE SUBMITTED NUMEROSITY ON A -1 TO 1 Y-AXIS GIVEN THE 
SPARSENESS AND ASPECT RATIO.  ASPECT IS (WIDTH / HEIGHT) AND IS ASSUMED TO
BE RECTANGULAR (HEIGHT < WIDTH).
%}

    max_psns = (numerosity/sparse);
        % each axis is multi;lied by 2 because the origin passes through
        % the center fo the plot.  Aspect is the ratio of just one quadrant
        % x-axis length is 2*aspect, y is the reference and set to 1
        % y-axis height is equivalent to 2/0.866 because of the hexagonal grid
        % .866 = sin(60) in degrees, which is the vertical distance between
        % the center of one position circle and the one in the row above,
        % within the hexogonal grid.
    quadsOnXAxis = 2; % underestimate x-axis to ensure complete fit within window
    quadsOnYAxis = 2;
    area_avail = quadsOnXAxis*quadsOnYAxis*aspect*.866;
    maxd = sqrt(area_avail/max_psns); %3   

% because of rounding errors the maxd is only going to be a close estimate
% begin the largest diameter size and work down until
% all stim will fit
    
    num_items_fit = 0;
    while (num_items_fit < max_psns);
        items_on_x = floor((quadsOnXAxis*aspect)/maxd);
        items_on_y = floor(quadsOnYAxis/(0.866*maxd));
        full_row_pairs = floor(items_on_y/2);
        % offsets make rows non-equivalent, so subtract one item for every
        % two rows
        num_items_fit = items_on_x*items_on_y - full_row_pairs;
        maxd = maxd - 0.001; %3
    end
   
    clear quadsOnXAxis quadsOnYAxis area_avail num_items_fit full_row_pairs 
    