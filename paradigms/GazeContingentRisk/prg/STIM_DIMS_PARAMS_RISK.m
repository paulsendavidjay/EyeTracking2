%  STIM_DIMS_PARAMS.m
%   GENERAL COMMANDS THAT WILL BE USED IN ALL TRIALS
%  AUDIO FILES
%  COLOR DIMENSIONS
%  FRAME PLACEMENT
%  ARROW PLACEMENT
%  COIN POSITIONS
%  IMAGE RESHAPE & MASKING
%
%  May 2011, delete (.5*) from gamble and sure frame definitions so prevent overlapping coins by frame


% DEFINE AUDIO STIMULI FILES
winAudioFile = 'audio/SingleCoin.wav';
picAudioFile = 'audio/pickOneFiltered.wav';
picAudioFile_dp = 'audio/PickOne_dp.wav';
youGetAudioFile = 'audio/youGetUnfiltered.wav';
youLoseAudioFile = 'audio/youLose.wav';
numChannels = 1; % mono

% DEFINE IMAGE AND WINNINGS POSITIONS COORDINATE  FILES 
coin1 = 'Pics/AustralianCoin';




winningLocs = load('winnings_positions2.txt');
for i = 1:length(winningLocs)
    winningLocs(i, 2) = winningLocs(i, 2) + 75;
end


sparse = .9; % proportion of available positions used for 'numerosity' coins
coinSizeFactor = .7; %  .7 proportion of Frame filled by coin
% maxNumerosity = 16; % 8 for RiskyKid; now set in NumDecis_test/practice
singleCoinRatio = 1/3; % size of single coin (training) relative to gamble frame


%===============================================================
%                                DEFINE COLORS
%===============================================================

alpha_setting = .5; % 0=transparent, 1=opaque

black = [0 0 0];    dkGrey = [50 50 50];    ltGrey = [150 150 150];    white = [255 255 255];
red = [190 51 0];   blue = [37 48 157];    lime =  [104 162 47];
yellow = [233 196 36];  green = [10 170 12]; dk_green = [33 100 32]; salmon = [211 151 104];
fushia = [241 94 197]; brown = [130 88 45]; red2 = [140 60 12]; orange = [221 132 43]; 
green2 = [62 148 14];


% rougly isoluminant colors (tobii screen)
isoLum{1} = [154, 51, 66]; % dark red
isoLum{2} = [41, 114, 70]; % dark green
isoLum{3} = [39, 121, 183]; % dark blue

% second set of roughly isoluminant colors (tobii screen)
isoLum{4} = [154, 133, 0]; % dark yellow
isoLum{5} = [242, 86, 91]; % light red
isoLum{6} = [86, 179, 211]; % light blue

coinColor = yellow;
timeBarColor = dk_green;
positionColor = ltGrey;
bgd = dkGrey;
frameColor = blue;
frameFillColor = green;
chosenFrameColor = red2;
confirmFrameColor = red;
winnerFrameColor = isoLum{4};
loserFrameColor = isoLum{5};
sureFrameColor = isoLum{6};
arrowColor = white;
FixColor1 = ltGrey;
FixColor2 = ltGrey;
FixColor3 = salmon;
pImageColor = bgd;
lossFrameColor = isoLum{1};
winFrameColor = isoLum{2};
sureColor = isoLum{3};

%===============================================================
%                                DEFINE POSITIONS
%===============================================================

%   LINEWIDTH  FOR FRAMES
lineWidth = 4;

%   0 FRAMESPACING ALIGNS EACH FRAME WITH X-AXIS & Y-AXIS
XFRAMESPACING = 60; % original: 16
YFRAMESPACING = 70;

% DEFINE  SCREEN POSITIONS RELATIVE TO FULL SCREEN DIMENSIONS
% fullScreen = [ xSTART, ySTART, xEND, yEND ] 
XMID = (fullScreen(3)-fullScreen(1))/2;
YMID = (fullScreen(4)-fullScreen(2))/2;
XMAX = fullScreen(3)-fullScreen(1);
YMAX = fullScreen(4)-fullScreen(2);
% SET PROPORTION OF X-AXIS TO BE USED IN DRAWING FRAMES
% WINDOWSCALE ADUSTS BOX FRAME END TO BOX FRAME END WIDTH
% X1 & Y1 ADJUST ASPECT (RATIO) OF STIMULUS DISPLAY
% FOR EXAMPLE, DILATING STIMULUS FOR ERP STUDY REQUIRES ONLY CHANGING
% windowscale
windowScale = 0.7; % 4.6 inches width 
%windowScale = 1; % 6.5 inches width
X1 = floor(windowScale*XMID/2.5);  %  2*display(x)/display(y); e.g. 2*1280/1024
Y1 = floor(windowScale*YMID/2);



% FIXATION POSITION
CENTER = [XMID-1, YMID-1, XMID+1, YMID+1];
EXPAND = [-1, -1, 1, 1]; % can be added to position arrays to expand object
STARTGROWTH=2;
MAXGROWTH = 20;
GROWTHRATE = 1;
fixPosition = CENTER;


% TIME BAR HEIGHT
time_bar_x = fullScreen(3); % xdim = full width of SCREEN
time_bar_y = fullScreen(4)/60; % ydim=1/60th height of screen




% -------------------       DEFINE GAMBLE POSITION COORDINATES    -----------------


% DEFINE DIMENSIONS OF coin MATRIX WINDOW FRAMES
coinFrame50 = floor([0, 0, X1, Y1]); % will also be used for 75% frame
pImageFrame = floor( coinFrame50 / 2.5 ); %
winningsFrame = [0, 0, max(winningLocs(:,1)) + winMaxd, max(winningLocs(:,2)) + winMaxd]; % WARNING, FRAME SIZE FOR DOTS IS NOT RELATIVE.

%gamble{top = 1, bottom = 2}{left = 1, right = 2}
gamble{1}{1} = floor(coinFrame50 + ...
    [XMID - coinFrame50(3) - XFRAMESPACING, ...
    YMID - coinFrame50(4) - YFRAMESPACING, ...
    XMID - coinFrame50(3) - XFRAMESPACING, ...
    YMID - coinFrame50(4) - YFRAMESPACING]);
gamble{1}{2} = floor(coinFrame50 + ...
    [XMID  + XFRAMESPACING, ...
    YMID - coinFrame50(4) - YFRAMESPACING, ...
    XMID + XFRAMESPACING, ...
    YMID - coinFrame50(4) - YFRAMESPACING]);
gamble{2}{1} = floor(coinFrame50 + ...
    [XMID - coinFrame50(3) - XFRAMESPACING, ...
    YMID + YFRAMESPACING, ...
    XMID - coinFrame50(3) - XFRAMESPACING, ...
    YMID + YFRAMESPACING]);
gamble{2}{2} = floor(coinFrame50 + ...
    [XMID  + XFRAMESPACING, ...
    YMID + YFRAMESPACING, ...
    XMID + XFRAMESPACING, ...
    YMID + YFRAMESPACING]);



gambleFrameWidth = gamble{1}{1}(3) - gamble{1}{1}(1);
singleCoinWidth = singleCoinRatio*gambleFrameWidth;

singleCoinCenter{1}{1} = get_center(gamble{1}{1});
singleCoinLoc{1}{1} = floor(...
    [singleCoinCenter{1}{1}(1) - .5*singleCoinWidth, ...
    gamble{1}{1}(4) + lineWidth, ...
    singleCoinCenter{1}{1}(1) + .5*singleCoinWidth, ...
    gamble{1}{1}(4) + lineWidth + singleCoinWidth]);

singleCoinCenter{1}{2} = get_center(gamble{1}{2});
singleCoinLoc{1}{2} = floor(...
    [singleCoinCenter{1}{2}(1) - .5*singleCoinWidth, ...
    gamble{1}{2}(4) + lineWidth, ...
    singleCoinCenter{1}{2}(1) + .5*singleCoinWidth, ...
    gamble{1}{2}(4) + lineWidth + singleCoinWidth]);

singleCoinLoc{1}{3} = floor(...
    [XMID - .5*singleCoinWidth, ...
    gamble{1}{2}(4) + lineWidth, ...
    XMID + .5*singleCoinWidth, ...
    gamble{1}{2}(4) + lineWidth + singleCoinWidth]);

singleCoinCenter{2}{1} = get_center(gamble{2}{1});
singleCoinLoc{2}{1} = floor(...
    [singleCoinCenter{2}{1}(1) - .5*singleCoinWidth, ...
    gamble{2}{1}(4) + lineWidth, ...
    singleCoinCenter{2}{1}(1) + .5*singleCoinWidth, ...
    gamble{2}{1}(4) + lineWidth + singleCoinWidth]);

singleCoinCenter{2}{2} = get_center(gamble{2}{2});
singleCoinLoc{2}{2} = floor(...
    [singleCoinCenter{2}{2}(1) - .5*singleCoinWidth, ...
    gamble{2}{2}(4) + lineWidth, ...
    singleCoinCenter{2}{2}(1) + .5*singleCoinWidth, ...
    gamble{2}{2}(4) + lineWidth + singleCoinWidth]);

singleCoinLoc{2}{3} = floor(...
    [XMID - .5*singleCoinWidth, ...
    gamble{2}{2}(4) + lineWidth, ...
    XMID + .5*singleCoinWidth, ...
    gamble{2}{2}(4) + lineWidth + singleCoinWidth]);





for i=1:2
    for j=1:2
        gambleFrame{i}{j} = floor(gamble{i}{j} + [-lineWidth, -lineWidth, lineWidth, lineWidth]); %#ok<AGROW>
    end
end

% -------------------       DEFINE SURE POSITION COORDINATES    -----------------


surePos{1} = floor([XMID - .5*coinFrame50(3), ...
    YMID - coinFrame50(4) - YFRAMESPACING, ...
    XMID + .5*coinFrame50(3), ...
    YMID  - YFRAMESPACING]);

% SQUARE SURE BET
sureFrame{1} = floor( surePos{1} + [-lineWidth, -lineWidth, lineWidth, lineWidth]);
sureFrameBackground{1} = surePos{1}; % REDUNDANT FOR SQUARE, BUT MAKES IT EASY FOR NOT HAVING TO REWRITE CODE

% RECTANGLE SURE BET
sureFrameRect{1} = floor([gamble{1}{1}(1:2), gamble{1}{2}(3:4)] + 3*[-lineWidth, -lineWidth, lineWidth, lineWidth]);
%sureFrameBackground{1} = floor([gamble{1}{1}(1:2), gamble{1}{2}(3:4)]);



surePos{2} = floor([XMID - .5*coinFrame50(3), ...
    YMID + YFRAMESPACING, ...
    XMID + .5*coinFrame50(3), ...
    YMID + coinFrame50(4) + YFRAMESPACING]);

% SQUARE SURE BET
sureFrame{2} = floor( surePos{2} + [-lineWidth, -lineWidth, lineWidth, lineWidth]);
sureFrameBackground{2} = surePos{2};

% RECTANGLE SURE BET
sureFrameRect{2} = floor([gamble{2}{1}(1:2), gamble{2}{2}(3:4)] + 3*[-lineWidth, -lineWidth, lineWidth, lineWidth]);
%sureFrameBackground{2} = floor([gamble{2}{1}(1:2), gamble{2}{2}(3:4)]);




% -------------------       DEFINE CENTERED POSITION COORDINATES      -----------------

outcomeCenter = floor(...
    [XMID - .5*coinFrame50(3), ...
    YMID - .5*coinFrame50(4), ...
    XMID + .5*coinFrame50(3), ...
    YMID  + .5*coinFrame50(4)]);

gambleFrameCenter = outcomeCenter + .5*[-lineWidth, -lineWidth, lineWidth, lineWidth]; 
sureFrameCenter = floor(...
    [XMID - coinFrame50(3), ...
    YMID - .5*coinFrame50(4), ...
    XMID + coinFrame50(3), ...
    YMID  + .5*coinFrame50(4)] + ...
    .5*[-lineWidth, -lineWidth, lineWidth, lineWidth]);


winningsCenter = floor(...
    [XMID - .5*winningsFrame(3), ...
    YMID - .5*winningsFrame(4), ...
    XMID + .5*winningsFrame(3), ...
    YMID  + .5*winningsFrame(4)]);


winningsPrizeTop = floor(...
    [XMID - .5*YMAX/3, ...
    YMAX/6, ...
    XMID + .5*YMAX/3, ...
    YMAX/2] + [-60, -60, 60, 60]);

winningsBottom = floor(...
    [XMID - .5*winningsFrame(3), ...
    YMAX - winningsFrame(4) - 2*lineWidth, ...
    XMID + .5*winningsFrame(3), ...
    YMAX - 2*lineWidth]);

winningsBox =  floor(...
    [XMID - .5*winningsFrame(3), ...
    .6*YMAX, ...
    XMID + .5*winningsFrame(3), ...
    YMAX]);

% -------------------       DEFINE ARROW COORDINATES    -----------------

arrowLength = XMID/6;
pointerLength = arrowLength/3;
% changed from 

arrowBeginX{1} = gamble{1}{1}(1) - arrowLength - lineWidth; % x begin on left
arrowEndX{1} = gamble{1}{1}(1) - lineWidth; % x end on left
arrowBeginX{2} = gamble{1}{2}(3) + lineWidth; % x begin on right
arrowEndX{2} = gamble{1}{2}(3) + arrowLength + lineWidth; % x end on right


% arrowBeginX{1} = XMID - 0.5*arrowLength; % x begin on left
% arrowEndX{1} = XMID + 0.5*arrowLength; % x end on left
% arrowBeginX{2} = XMID - 0.5*arrowLength; % x begin on right
% arrowEndX{2} = XMID + 0.5*arrowLength; % x end on right

arrowTopY = (surePos{1}(4) + surePos{1}(2))/2; % situates arrows mid-frame
arrowBottomY = (surePos{2}(4) + surePos{2}(2))/2;
% arrowTopY = (surePos{1}(2) - time_bar_y)/2; % situates arrows between top frame and time bar
% arrowBottomY = surePos{2}(4) + arrowTopY;

% -------------------       DEFINE MAX COIN DIAMETER    -----------------

%  ASSUMES OFFSET COLUMNS WILL BE USED (.866 OFFSET)
aspect50 = coinFrame50(3) / coinFrame50(4) ;
scalingFactor = Y1;
[items_on_x, items_on_y, maxd] = find_geometry(maxNumerosity, sparse, aspect50);
maxd = floor(.5*maxd*(scalingFactor));



%===============================================================
%                                COIN POSITIONS
%===============================================================


%  CREATE MATRIX OF (X, Y) STARTING POINTS FOR FULL SIZE CIRCLES IN 50% FRAME
positions{1} = zeros(items_on_x*items_on_y, 2);
current_y = 0;
current_position_number = 1;
for y = 1:items_on_y
    if (mod(y,2) == 1)  % maxd will added back at the first iteration, and offset by column
        current_x = 0 - maxd;
    else
        current_x = 0 - .5*maxd;
    end
    for x = 1:items_on_x
        current_x = current_x + maxd;
        positions{1}(current_position_number, :) = [current_x, current_y]; %#ok<AGROW>
        current_position_number = current_position_number + 1;
    end
    current_y = current_y + 0.866*maxd;
end

%   ADJUST X,Y POSITIONS TO CENTER coin MATRIX WITHIN FRAME
xCentering = (coinFrame50(3) - (max(positions{1}(: , 1)) + maxd))/2;
yCentering = (coinFrame50(4) - (max(positions{1}(: , 2)) + maxd))/2;
for i = 1:length(positions{1}(:,1))
    %    positions{1}(i, :) = positions{1}(i, :) + [xCentering, yCentering];
end
positions{1} = floor(positions{1});

% ADD COLUMN OF RANDOM NUMBERS for RANDOM PLACEMENT
positions{1}(:, 3) = rand(length(positions{1}(:,1)), 1);
positions{1} = sortrows(positions{1}, 3);




% --------------       IMPORT COIN, MASK, & DETERMINE DIAMETER      ------------

img=imread(coin1, 'JPG');
img2=imread(coin1, 'JPG');
[numRows, numCols, cSpace] = size(img);
% imshow(img);

% CREATE BLACK MASKING FOR COIN IMAGE
upperRGB = 185;
for r = 1:numRows
    for c = 1:numCols
        if (img(r, c, 1) > upperRGB && img(r, c, 2)  > upperRGB  && img(r, c, 3)  > upperRGB )
            img(r, c, 1) = 0;
            img(r, c, 2) = 0;
            img(r, c, 3) = 0;
            
            img2(r, c, 1) = bgd(1);
            img2(r, c, 2) = bgd(2);
            img2(r, c, 3) = bgd(3);
            
        end
    end
end

% RESIZE COIN ACCORDING TO MAXD
winImg = imresize(img, [winMaxd winMaxd]);
img = imresize(img,coinSizeFactor*[maxd maxd]);
img2 = imresize(img2, [singleCoinWidth, singleCoinWidth]);
%imshow(img);


