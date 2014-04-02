%===============================================================
%                                WinPositions
%===============================================================
%               FULL SIZE

%  ASSUMES OFFSET COLUMNS WILL BE USED (.866 OFFSET)
    totalWinPositions = 0;
    while (totalWindPositions < 301)
        numerosity = 250; sparse = 1;
        aspect50 = winningsFrame(3) / winningsFrame(4) ;
        scalingFactor = 150; %Y1;
        [items_on_x, items_on_y, winMaxd] = find_geometry(numerosity, sparse, aspect50);
        totalWindPositions = items_on_x*items_on_y;
        numerosity = numerosity + 5;
    end
    
    winMaxd = floor(.5*winMaxd*(scalingFactor));  

%  CREATE MATRIX OF (X, Y) STARTING POINTS FOR FULL SIZE CIRCLES IN 50% FRAME
            positions{1} = zeros(items_on_x*items_on_y, 2);
            current_y = 0;
            current_position_number = 1;
            for y = 1:items_on_y
                if (mod(y,2) == 1)  % winMaxd will added back at the first iteration, and offset by column
                    current_x = 0 - winMaxd;
                else
                    current_x = 0 - .5*winMaxd;
                end
                for x = 1:items_on_x
                    current_x = current_x + winMaxd;
                    positions{1}(current_position_number, :) = [current_x, current_y];
                    current_position_number = current_position_number + 1;
                end
                current_y = current_y + 0.866*winMaxd;
            end
            
            %   ADJUST X,Y POSITIONS TO CENTER DOT MATRIX WITHIN FRAME
            xCentering = (dotFrame50(3) - (max(positions{1}(: , 1)) + winMaxd))/2;
            yCentering = (dotFrame50(4) - (max(positions{1}(: , 2)) + winMaxd))/2;
            for i = 1:length(positions{1}(:,1))
                positions{1}(i, :) = positions{1}(i, :) + [xCentering, yCentering];
            end
             positions{1} = floor(positions{1});
 
             % ADD COLUMN OF RANDOM NUMBERS for RANDOM PLACEMENT
            %seed = seed + 1;
            %rand('seed', seed); % using a seed to generate the same random numbers
            positions{1}(:, 3) = rand(length(positions{1}(:,1)), 1);
            positions{1} = sortrows(positions{1}, 3);

            
            %   ADJUST X,Y POSITIONS TO CENTER DOT MATRIX WITHIN FRAME
            xCentering = (dotFrame25(3) - (max(positions{1}{4}(: , 1)) + winMaxd))/2;
            yCentering = (dotFrame25(4) - (max(positions{1}{4}(: , 2)) + winMaxd))/2;
            for i = 1:length(positions{1}{4}(:,1))
                positions{1}{4}(i, :) = positions{1}{4}(i, :) + [xCentering, yCentering];
            end
            positions{1}{4} = floor(positions{1}{4});

            % ADD COLUMN OF RANDOM NUMBERS for RANDOM PLACEMENT
            %seed = seed + 1;
            %rand('seed', seed); % using a seed to generate the same random numbers
            positions{1}{4}(:, 3) = rand(length(positions{1}{4}(:,1)), 1);
            positions{1}{4} = sortrows(positions{1}{4}, 3);


    
    
    