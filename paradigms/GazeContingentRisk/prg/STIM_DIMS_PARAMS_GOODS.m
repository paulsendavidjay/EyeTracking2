%  STIM_DIMS_PARAMS.m
%   GENERAL COMMANDS THAT WILL BE USED IN ALL TRIALS
%  AUDIO FILES
%  COLOR DEFINITIONS
%  IMAGE READING



%===============================================================
%                                DEFINE COLORS
%===============================================================

black = [0 0 0];    dkGrey = [50 50 50]; grey = [125 125 125];   ltGrey = [170 170 170];    white = [255 255 255];
red = [255 20 20];   blue = [0 0 255];    lime =  [104 162 47];
yellow = [255 255 0];  green = [0 255 0]; salmon = [211 151 104];
fushia = [255 0 255]; brown = [130 88 45]; gold = [245 218 129];

iso_green = [128, 255, 185];
iso_blue = [151, 153, 255];
iso_red = [255, 151, 234];

coinColor = gold;
%positionColor = ltGrey;
bgd = grey; bg_alpha = bgd/255;
legend_bgd = ltGrey;
frameColor = blue;
bankFrameColor = black;
frameFillColor = green;

acceptFrameColor = green;
rejectFrameColor = red;
bankBarColor = yellow;
debtBarColor = red;
chooseFrameColor = blue;
arrowColor = white;

chosenFrameColor = salmon;
winnerFrameColor = yellow;


FixColor1 = ltGrey;

%   LINEWIDTHS
lineWidth = 4;


% DEFINE  SCREEN POSITIONS RELATIVE TO FULL SCREEN DIMENSIONS
XMID = (fullScreen(3)-fullScreen(1))/2;
YMID = (fullScreen(4)-fullScreen(2))/2;
XMAX = fullScreen(3)-fullScreen(1);
YMAX = fullScreen(4)-fullScreen(2);
X1 = XMID/3;
Y1 = YMID/2;


h_margin = .125; %Proportion of 'fullScreen' dimension
v_margin = .125; %Proportion of 'fullScreen' dimension
h_gap = .75; %Proportion of card dimension
v_gap = 0.25; %Proportion of card dimension 


%===============================================================
%                                DEFINE SUPPLENTARY IMAGES 
%===============================================================
%StarImage = imread('pictures/single_star.png', 'BackgroundColor', bg_alpha);
%legend_margin = fullScreen(4)/60; % pixels above and below legend








