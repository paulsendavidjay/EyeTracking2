% 8 COIN BASE IN 50% WINDOW

[numRows, numCols] = size(positions{1}) ;

%  RANDOMIZE COIN POSITIONS WITHIN PLACE
for i = 1:length(positions{1})
    xr_offset = rand*(maxd - coinSizeFactor*maxd);
    yr_offset = rand*(maxd - coinSizeFactor*maxd);
    currentPositions(i,1) = floor(positions{1}(i, 1) + xr_offset); %#ok<AGROW>
    currentPositions(i,2) = floor(positions{1}(i, 2) + yr_offset); %#ok<AGROW>
end

clear currentImage;
currentImage = zeros(Y1, Y1, 3);
%   CYCLE THROUGH POSITIONS, COPY COIN IMAGE PIXEL BY PIXEL INTO NEW IMAGE
for i = 1:drawCoins;
    for column = 1:length(img(1,:,1))
        for row = 1:length(img(:,1,1))
            if ( (img(row,column,1) > 1) && (img(row,column,1) > 2) && (img(row,column,3) > 1) )
                currentImage(currentPositions(i,2) + row, currentPositions(i,1) + column, 1) = ...
                    img(row,column,1); %#ok<AGROW>
                currentImage(currentPositions(i,2) + row, currentPositions(i,1) + column, 2) = ...
                    img(row,column,2); %#ok<AGROW>
                currentImage(currentPositions(i,2) + row, currentPositions(i,1) + column, 3) = ...
                    img(row,column,3); %#ok<AGROW>
            end
        end
    end
end
%imshow(currentImage);

% REDO RANDOM PLACEMENT FOR NEXT TIME
positions{1}(:, 3) = rand(numRows, 1);
positions{1} = sortrows(positions{1}, 3);


