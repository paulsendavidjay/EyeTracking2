% 

[numRows, numCols] = size(winningLocs) ;


winImage = zeros(winningsFrame(3), winningsFrame(4), 3);
%   CYCLE THROUGH POSITIONS, COPY COIN IMAGE PIXEL BY PIXEL INTO NEW IMAGE
for i = 1:min(winnings, 300);
    for column = 1:length(winImg(1,:,1))
        for row = 1:length(winImg(:,1,1))
            %  MAKE SURE EACH R G B POINT IS NOT BLACK. IF IT IS, DON'T DRAW.
            if ( (winImg(row,column,1) > 1) && (winImg(row,column,1) > 2) && (winImg(row,column,3) > 1) )
                winImage(winningLocs(i,2) + row, winningLocs(i,1) + column, 1) = ...
                    winImg(row,column,1); %#ok<AGROW>
                winImage(winningLocs(i,2) + row, winningLocs(i,1) + column, 2) = ...
                    winImg(row,column,2); %#ok<AGROW>
                winImage(winningLocs(i,2) + row, winningLocs(i,1) + column, 3) = ...
                    winImg(row,column,3); %#ok<AGROW>
            end
        end
    end
end
