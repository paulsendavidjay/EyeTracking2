%   PSYCHTOOLBOX SCRIPT
%{


GazeContingent2 DETAILS: 
THIS VERSION WORKS.
NEED TO DO: 
    TEST COUNTERBALANCING VARIABLES
    
    

The all of the files required for this script are enclosed in the subdirectories of the parent directory.
    STIM_DIMS_PARAMS_RISK contains general settings: colors, frame coordinates, audio files, coin images, etc.
    
    trials_list... files supply the input for each trial, including gamble and sure bet values, probability of winning a gamble,
    and the trial type.
        trialType: 1 - sure bet vs. sure bet (sure1 vs sure2)
        trialTypes: 2-5 - sure bet vs. gamble (sure2 vs mag1 | mag2) note: mag1 value should be larger than mag2 for outcome colors to be consistent
        
        trialType: 6 - gamble vs. gamble (sure1 | sure2 vs mag1 | mag2) - mag1-2 EV > sure1-2 EV
        
        trialType: - - gamble vs. gamble (sure1 | sure2 vs mag1 | mag2) - equal EV
        trialType: - - not implemented any longer, but was for training up on pairing images to probabilities through forced choice (gamble)
        vert: 1 = gamble on top, 0 = gamble on bottom
        topRight: 1 = right arrow is on top, 0 = right arrow is on bottom
        RtLt: 1 - larger mag is on left side, 0 - larger mag is on right side
        
        ISI: ISI1 - value in seconds added a standard 1 second delay between confirmation and presentation of outcome
        ITI: (becomes ISI2 in script) - delay between outcome and start of next trial
        
        the images columns should contain the value 5 consistently. This has to do with the probabilistic learning task no longer implemented. Could be changed,
        but the task could possibly be reinstated, so we're holding off on deleting.
        
        
        key presses:
            return: decision made -> arrow screen
            right & left arrows: selection of choice option
            return: confirmation -> delay -> outcome
            q or esc: quit, close open files, clear screen
            h: escape winnings display
        
    Eye tracking:
        It is expected that when eyetracking the 'START_TRACKING' function has begun. The first call to the tracker will be 'RECORD'

    
    To make practice file, copy the current test file and
    1) set DEBUG = 1;
    2) set if DEBUG fullscreen to []
    3) comment out 
        talk2tobii('STOP_RECORD');
        talk2tobii('STOP_RECORD');
    4) replace all talk2tobii('EVENT' with % talk2tobii('EVENT'  to comment out events
    5) uncomment SCREEN('FillOval', ChoiceStim, FixColor3, fixPosition);


%}


clear subjectNumber


% starting trial number
trial = 1;



DEBUG = 0;
MEMORY = 0; % SET TO 1 IF IMAGES SHOULD DISAPPEAR AFTER CHANGING FIXATION, SET TO 0 IF IMAGES SHOULD REMAIN AFTER CHANGING FIXATION

% DEFINE KEBOARD VALUES
if strcmp(computer, 'MACI')
    left_key = 80;
    right_key = 79;
    esc_key = 41;
    q_key = 20;
    up_key = 82;
    h_key = 11;
    return_key = 40;
    p_key = 19;
    r_key = 21; % resume trials
    c_key = 6; % calibrate
    d_key = 7; % draw eyes or display eyes
    space_key = 44;
    
    
    left_jst = 5;
    right_jst = 6;
    two_jst = 2;
    
    
else
    left_key = 37;
    right_key = 39;
    esc_key = 81;
    q_key = 27;
    up_key = 38;
    h_key = 72;
    return_key = 13;
    p_key = 80;
    r_key = 82;
    space_key = 32;
    %jst
    left_jst = 5;
    right_jst = 6;
    two_jst = 2;
end



WaitSecs(0.5); % wait .5 seconds to give time for keyboard to clear
% CHECK FOR GAMEPAD CONNECTION
if Gamepad('GetNumGamepads') ~= 1
    
    
    % REQUIRE A DEFINITIVE RESPONSE FROM USER, AVOID KEYSTROKES INTO MAIN
    % MATLAB WINDOW
    progress = 0;
    while (progress == 0)
        tt = input('\nGamePad not connected\n\nPress "q" to quit or "p" to progress\n', 's');
        if strcmp(tt,'q'); return; end
        if strcmp(tt,'p'); progress = 1; end
    end  
    
    USINGGAMEPAD = 0;
else
    gamepadIndex = Gamepad('GetGamepadIndicesFromNames', 'Logitech Dual Action');
    % while Gamepad('GetButton', gamepadIndex, 2) == 0; end % example
    USINGGAMEPAD = 1;
end



WaitSecs(0.5); % wait .5 seconds to give time for keyboard to clear
% DETERMINE STATUS OF EYETRACKER (WILL USE MOUSE IF EYETRACKER NOT CONNECTED)
[status,history] = talk2tobii('GET_STATUS');
if (status(2) == 0) % Tobii disconnected
    
    
    % REQUIRE A DEFINITIVE RESPONSE FROM USER, AVOID KEYSTROKES INTO MAIN
    % MATLAB WINDOW
    progress = 0;
    while (progress == 0)
        tt = input('\nEye-tracker not connected\n\nPress "q" to quit or "p" to progress\n', 's');
        if strcmp(tt,'q'); return; end
        if strcmp(tt,'p'); progress = 1; end
    end

    
    EYETRACKER = 0;


else
    EYETRACKER = 1;

    [status,history] = talk2tobii('GET_STATUS');
    if status(7)
        talk2tobii('STOP_TRACKING')
    end

end






% INITIALIZE LAST DISPLAY (0 OR 1)
screenNumber=max(Screen('Screens')); 
Screen('Preference', 'SkipSyncTests',1); % keep active for test only
if DEBUG
    fullScreen = [0 0 800 600];
else
    fullScreen = [];
end
[window, fullScreen] = SCREEN('OpenWindow', screenNumber, [100 100 100], fullScreen, 32);
XMID = fullScreen(3)-fullScreen(1); YMID = fullScreen(4)-fullScreen(2); white = [255 255 255]; bgd = [50 50 50];
SCREEN('TextSize', window, 20);
SCREEN('DrawText', window, 'COLLECTING SCREEN FLIP RATE', floor(.6*XMID), YMID, white, bgd, 1);
SCREEN('CopyWindow', window, window, fullScreen, fullScreen);
SCREEN('Flip', window);

grayIndex = GrayIndex(screenNumber);
ifi = Screen('GetFlipInterval',window,100); % collect frame rate information



%    INITIALIZE AUDIO
InitializePsychSound(0); % initialize sound driver; (0) means ms timing is not crucial, (1) if it is
PsychPortAudio initialized. Will use CoreAudio for audio.




%   LOAD TRIAL PARAMETERS
%[(1) trial #, (2) trialType, (3) mag1, (4) mag2, (5) baseN, (6) oddsType, (7) side, (8) choice(for demo)]
trialList = 'trials_list_Frontiers1GL.txt';
Param = importdata(trialList, '\t', 1);
ntrials_per_block = 29; % used for dislpaying winnings and for time bar




%  SET UP STIMULUS DIMENSIONS AND PARAMETERS
winMaxd = 15; % max diameter for winnings coins, calculated by hand (15 is good for 800x600 res)
maxNumerosity = 25; % maximum number of coins to be shown in any display, aspect and sparse set in STIM_DIMS_PARAMS, larger values make smaller coins, less density
add_to_fix_pos = 20; % number of pixels to add to center of fixation to initiate trial (makes eye-position field larger)
track_avg_pnts = 5;
STIM_DIMS_PARAMS_RISK;




% DO NOT ENTER SUBJECT INFORMATION FOR OUTPUT FILES IF ONLY DEBUGGING
if (~ DEBUG )
    
    %HideCursor;
    
    if ( ~ exist('subjectNumber', 'var') )
        subjectNumber = input('\n\nenter subject number\n');
    end
    
    files_with_subjectNumber = dir(['logs/*', num2str(subjectNumber), '*']);
    while ( ~ isempty(files_with_subjectNumber) )
        subjectNumber = input('\nthat subject number already exists\ntry a new number\n');
        files_with_subjectNumber = dir(['logs/*', num2str(subjectNumber), '*']);
    end


    % open output files for writing
    outputFileName1 = ['logs/risk_reinf_', num2str(subjectNumber), '_test_GL.txt'];
    outputFileName2 = ['winnings/risk_reinf_winnings', num2str(subjectNumber), '_GL.txt'];
    outputFileName3 = ['logs/exp_info_', num2str(subjectNumber), '_GL.txt'];
    trackFileName = ['logs/Eye_tracking_', num2str(subjectNumber), '_GL.txt'];
    eventFileName = ['logs/Eye_events_', num2str(subjectNumber), '_GL.txt'];


    %  OPEN OUTPUT FILES FOR WRITING
    fid1 = fopen(outputFileName1, 'w');
    fid2 = fopen(outputFileName2, 'w');
    fid3 = fopen(outputFileName3, 'w');
    png_path = ['logs/', num2str(subjectNumber), '_pngs/'];
    mkdir(png_path)

    %  PRINT DATA TO INFORMATION FILE
    fprintf(fid3, 'Participant: %d\nfullScreen dimensions: %d %d %d %d\n', subjectNumber, fullScreen(1), fullScreen(2), fullScreen(3), fullScreen(4) );
    fprintf(fid3, 'Pos 1 1 top left: %d %d %d %d\n', gamble{1}{1}(1), gamble{1}{1}(2), gamble{1}{1}(3), gamble{1}{1}(4) );
    fprintf(fid3, 'Pos 1 2 top right: %d %d %d %d\n', gamble{1}{2}(1), gamble{1}{2}(2), gamble{1}{2}(3), gamble{1}{2}(4) );
    fprintf(fid3, 'Pos 2 1 bottom left: %d %d %d %d\n', gamble{2}{1}(1), gamble{2}{1}(2), gamble{2}{1}(3), gamble{2}{1}(4) );
    fprintf(fid3, 'Pos 2 2 bottom right: %d %d %d %d\n', gamble{2}{2}(1), gamble{2}{2}(2), gamble{2}{2}(3), gamble{2}{2}(4) );
    fprintf(fid3, 'Pos 1 0 top mid: %d %d %d %d\n', surePos{1}(1), surePos{1}(2), surePos{1}(3), surePos{1}(4) );
    fprintf(fid3, 'Pos 2 0 top mid: %d %d %d %d\n', surePos{2}(1), surePos{2}(2), surePos{2}(3), surePos{2}(4) );

    %  PRINT HEADER OF OUTPUT FILE
    fprintf(fid1,'trial trialType sure1 sure2 mag1 mag2 choice gamble coinsWon P1_image P1 P2_image P2 vert topRight sideMag sideSure fixTime presTime decisionTime respTime confirm_delayTime outcomeTime RT choiceCount sure1_fix sure2_fix mag1_fix mag2_fix condition\n');

end


% turn off keyboard
% ListenChar(2); ListenChar(0);






% ================================================================
%                                  BEGIN STIMULUS PRESENTATION LOOP
% ================================================================


% INITIALIZE WINNINGS VARIABLES
total_winnings = 0; 
winnings = 0;

% PRESENT STARTING SCREEN
BlankSCREEN = SCREEN('OpenOffSCREENwindow', window, bgd, fullScreen);
SCREEN('TextSize', BlankSCREEN, 20);
SCREEN('DrawText', BlankSCREEN, 'PRESS "C" SET EYE POSITION', floor(.6*XMID), YMID, white, bgd, 1);
SCREEN('CopyWindow', BlankSCREEN, window, fullScreen, fullScreen);
SCREEN('Flip', window);

% WAIT FOR KEY PRESS
%choose if you want to redo the calibration
%disp('Press space to resume calibration or q to exit calibration and continue tracking');
begin_cals = 0;
while (begin_cals == 0)
    tt= input('press "C" and "ENTER" to continue with eye track and calibration\n','s');

    if( strcmpi(tt,'C') || strcmpi(tt,'c') )
        begin_cals = 1;
    end
end



% ================================================================
% ================================================================
%
%   BEGIN EYETRACKER POSITIONING, CALIBRATION, AND RECORDING
%
% ================================================================
% ================================================================

if( EYETRACKER )
    
    % tracking off
    DRAW_EYES; % tracking on
    % tracking off
    WaitSecs(0.5);
    % PRESENT STARTING SCREEN
    BlankSCREEN = SCREEN('OpenOffSCREENwindow', window, bgd, fullScreen);
    SCREEN('TextSize', BlankSCREEN, 20);
    SCREEN('DrawText', BlankSCREEN, 'PRESS ANY KEY TO CALIBRATE', floor(.6*XMID), YMID, white, bgd, 1);
    SCREEN('CopyWindow', BlankSCREEN, window, fullScreen, fullScreen);
    SCREEN('Flip', window);
    
    % WAIT FOR KEY PRESS
    startTime = KbWait;
    [ keyIsDown, t, keyCode ] = KbCheck;

    % requires tracking off
    CALIBRATE; % loops until calibration is satisfactory
    
    talk2tobii('START_TRACKING');
    WaitSecs(0.05);
    talk2tobii('CLEAR_DATA');
    WaitSecs(0.05);
    talk2tobii('RECORD');    
    WaitSecs(0.05);
    talk2tobii('EVENT','Start', 0);
    fprintf('\n')
end


% AUDIO DATA FOR COIN SOUNDS
[winCoins, freq, nbits] = wavread(winAudioFile);
wonWaveData = winCoins';
winAudio = PsychPortAudio('Open', [], [], 0, freq, numChannels);

[youGet, freq, nbits] = wavread(youGetAudioFile);
youGetData = youGet';
GainAudio = PsychPortAudio('Open', [], [], 0, freq, numChannels);

[youLose, freq, nbits] = wavread(youLoseAudioFile);
youLoseData = youLose';
LossAudio = PsychPortAudio('Open', [], [], 0, freq, numChannels);



% ================================================================
% ================================================================
%
%
%                          BEGIN TRIAL LOOPS
%
%
% ================================================================
% ================================================================

%TEMP SETTING OF PROBABILITIES
P1 = 0.5; P2 = 0.5;

% PRESENT STARTING SCREEN
BlankSCREEN = SCREEN('OpenOffSCREENwindow', window, bgd, fullScreen);
SCREEN('TextSize', BlankSCREEN, 20);
SCREEN('DrawText', BlankSCREEN, 'PRESS ANY KEY TO BEGIN...', floor(.6*XMID), YMID, white, bgd, 1);
SCREEN('CopyWindow', BlankSCREEN, window, fullScreen, fullScreen);
SCREEN('Flip', window);

% WAIT UNTIL KEYBOARD PRESS TO BEGIN
if (~ DEBUG)
    startTime = KbWait;
else
    startTime = GetSecs();
end
[ keyIsDown, t, keyCode ] = KbCheck;


% GET NUMBER OF TRIALS FOR PRESENTATION LOOP
[nrows, ncols] = size(Param.data);

%fig_title = ['EYE POSITION: TRIAL ', num2str(trial)];
%fig = figure('Name',fig_title);

% trial listed at top of script
while trial <= nrows;
  
    trial_string = ['trial ', num2str(trial), '\n'];
    fprintf(trial_string);
      
    
    
    
    if DEBUG

        % CLOSE ALL OPEN TEXTURES AND OFF-SCREEN WINDOWS
        windowPtrs=Screen('Windows');
        if length(windowPtrs) > 1
            for openWindowPtr = windowPtrs(length(windowPtrs)):-1:windowPtrs(2)
                Screen('Close', openWindowPtr);
            end
        end
    end % if DEBUG
    
    recently_paused = 0; % clear paused flag
    
    % retrieve trial data
    trialType = Param.data(trial, 3);

    sure1 = Param.data(trial, 4);
    sure2 = Param.data(trial, 5);
    mag1 = Param.data(trial, 6);
    mag2 = Param.data(trial, 7);
    
    vert = Param.data(trial, 8); % 0: gamble on bottom; 1: gamble on top
    topRight = Param.data(trial, 9); % 0: respond left for top; 1: respond right for top
    side = Param.data(trial, 10); % 0: larger stim on RT; 1: larger stim on LT for Mag
    sideB = Param.data(trial, 11); % 0: larger stim on RT; 1: larger stim on LT for Sure (trial types 6&7)
    
    
    ISI1 = Param.data(trial, 12);
    ISI2 = Param.data(trial, 13);
    P1Image = Param.data(trial, 14);
    P2Image = Param.data(trial, 15);
    GLcondition = Param.data(trial, 16);
    
    
    %   SET COUNTER BALANCING VARIABLES
    if (side == 1), a = 1; b = 2; else a = 2; b = 1; end
    if (sideB == 1), A = 1; B = 2; else A = 2; B = 1; end
    if(vert ==1), vertA = 1; vertB = 2; else vertA =2; vertB = 1; end
    
    
    %   GAIN VS LOSS DOMAIN CONTINGENCIES
    if (GLcondition == 1)
        frameColor = isoLum{2};
        winnerFrameColor = isoLum{4};
        loserFrameColor = isoLum{5};
        sureFrameColor = isoLum{6};
        valence = 1;
    else
        frameColor = isoLum{1};
        winnerFrameColor = isoLum{5};
        loserFrameColor = isoLum{4};
        sureFrameColor = isoLum{6};
        valence = -1;
    end
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % ================================================================
    %
    %                          CREATE DISPLAYS
    %
    % ================================================================
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %
    %           INITIALIZE STIMULUS SCREENS & TIME BAR
    %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % STIMULUS SCREENS
    BlankSCREEN = SCREEN('OpenOffSCREENwindow', window, bgd, fullScreen);
    BlackScreen = SCREEN('OpenOffSCREENwindow', window, [0 0 0], fullScreen);
    FixStim = SCREEN('OpenOffSCREENwindow', window, bgd, fullScreen); % show first
    CurrentStim = SCREEN('OpenOffSCREENwindow', window, bgd, fullScreen); % show second
    ChoiceStim = SCREEN('OpenOffSCREENwindow', window, bgd, fullScreen); %  show third
    
    % FRAMED OUTCOME WINDOWS
    OutcomeStim = SCREEN('OpenOffSCREENwindow', window, bgd, fullScreen); % outcome 2
    OutcomeStim2 = SCREEN('OpenOffSCREENwindow', window, bgd, fullScreen); % outcome 2
    SureBetA = SCREEN('OpenOffSCREENwindow', window, bgd, fullScreen); % sure bet 1
    SureBetB = SCREEN('OpenOffSCREENwindow', window, bgd, fullScreen); % sure bet 2
    SureBetAPlain = SCREEN('OpenOffSCREENwindow', window, bgd, fullScreen); % sure bet 1
    SureBetBPlain = SCREEN('OpenOffSCREENwindow', window, bgd, fullScreen); % sure bet 2
    
    
    
    
    % TIME BAR (green time bar at top of screen gets drawn to each trial screen)
    time_left = ntrials_per_block - mod(trial - 1, ntrials_per_block);
    time_bar = get_bank_bar(time_left, ntrials_per_block, [0,0,time_bar_x, time_bar_y], 'x');
    
    % current stim
    Screen('FillRect', CurrentStim, black, [0,0,fullScreen(3),fullScreen(4)/60], lineWidth);
    Screen('FillRect', CurrentStim, timeBarColor, time_bar, lineWidth);
    % choice stim
    Screen('FillRect', ChoiceStim, black, [0,0,fullScreen(3),fullScreen(4)/60], lineWidth);
    Screen('FillRect', ChoiceStim, timeBarColor, time_bar, lineWidth);
    % fix stim
    Screen('FillRect', FixStim, black, [0,0,fullScreen(3),fullScreen(4)/60], lineWidth);
    Screen('FillRect', FixStim, timeBarColor, time_bar, lineWidth);
    
    % Black Screen
    Screen('FillRect', BlackScreen, black, [0,0,fullScreen(3),fullScreen(4)/60], lineWidth);
    Screen('FillRect', BlackScreen, timeBarColor, time_bar, lineWidth);
    
    
    
    
    
    
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %
    %           TRIAL TYPE 1: MAGNITUDE COMPARISON
    %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if (trialType == 1)
        
        % SHOULD RESULT IN THE FOLLOWING TEX IMAGES AND SCREENS
        %   tex3 : square display of sure1 coins
        %   tex4 : square display of sure2 coins
        %   tex_EmptyFrames : just the frames
        %   SCREEN CurrentStim : ChoiceFrames (blue) and Coins
        %   tex_CurrentStimImage : texture of CurrentStim (allows alpha
        %       blending)
        %   SCREEN ChoiceStim : ChoiceFrames (beige) and no Coins
        %   SCREEN SureBetA : Chose sure1 option
        %   SCREEN SureBetB: Chose sure2 option
        
        
        
        % tex3, tex4: COIN IMAGES
        drawCoins = sure1; %#ok<NASGU>
        DrawEightCoin_50; % function to draw the image 'currentimage'
        tex3 = SCREEN( 'MakeTexture', window, currentImage );
        
        drawCoins = sure2; %#ok<NASGU>
        DrawEightCoin_50;
        tex4 = SCREEN( 'MakeTexture', window, currentImage );
                
        
        
        % SCREEN CurrentStim part1
        % A
        SCREEN('FrameRect', CurrentStim, frameColor, sureFrame{vertA}, lineWidth);
        % B
        SCREEN('FrameRect', CurrentStim, frameColor, sureFrame{vertB}, lineWidth);
        
        % tex_EmptyFrames
        EmptyFramesArray = SCREEN('GetImage', CurrentStim, fullScreen);
        tex_EmptyFrames = SCREEN( 'MakeTexture', window, EmptyFramesArray);
        
        % SCREEN CurrentStim part2
        SCREEN('FillRect', CurrentStim, black, sureFrameBackground{vertA});
        SCREEN('FillRect', CurrentStim, black, sureFrameBackground{vertB});
        SCREEN( 'DrawTexture', CurrentStim, tex3, [], surePos{vertA});
        SCREEN( 'DrawTexture', CurrentStim, tex4, [], surePos{vertB});
        
        % tex_CurrentStimImage
        CurrentStimArray = SCREEN('GetImage', CurrentStim, fullScreen);
        tex_CurrentStimImage = SCREEN( 'MakeTexture', window, CurrentStimArray);
        
             
        % SCREEN ChoiceStim 
        SCREEN('FrameRect', ChoiceStim, FixColor3, sureFrame{vertA}, lineWidth);
        SCREEN('FrameRect', ChoiceStim, FixColor3, sureFrame{vertB}, lineWidth);
        

        
        % POSSIBLE OUTCOMES
        % A
        winAmount{vertA} = sure1; %#ok<AGROW>
        Screen('BlendFunction', SureBetA, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); % allows screen to apply transparency
        SCREEN( 'DrawTexture', SureBetA, tex_CurrentStimImage, [], fullScreen, [], [], alpha_setting ); % copy transparency

        SCREEN('FillRect', SureBetA, black, sureFrameBackground{vertA});
        SCREEN( 'DrawTexture', SureBetA, tex3, [], surePos{vertA});
        SCREEN('CopyWindow', SureBetA, SureBetAPlain, fullScreen, fullScreen);
        SCREEN('FrameRect', SureBetA, sureFrameColor, sureFrame{vertA}, lineWidth);
        % B
        winAmount{vertB}  = sure2; %#ok<NASGU>
        Screen('BlendFunction', SureBetB, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); % allows screen to apply transparency
        SCREEN( 'DrawTexture', SureBetB, tex_CurrentStimImage, [], fullScreen, [], [], alpha_setting ); % copy transparency

        SCREEN('FillRect', SureBetB, black, sureFrameBackground{vertB});
        SCREEN( 'DrawTexture', SureBetB, tex4, [], surePos{vertB});
        SCREEN('CopyWindow', SureBetB, SureBetBPlain, fullScreen, fullScreen);
        SCREEN('FrameRect', SureBetB, sureFrameColor, sureFrame{vertB}, lineWidth);

        
        
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %
    %           TRIAL TYPES 2-6: SURE THING VS GAMBLE
    %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        
    elseif (trialType > 1) && (trialType < 6)
        
        % WILL RESULT IN THE FOLLOWING TEX IMAGES AND SCREENS
        %   tex1 : square display of mag1 coins
        %   tex2 : square display of mag2 coins
        %   tex3 : square display of sure2 coins
        %   tex_EmptyFrames : just the frames
        %   SCREEN CurrentStim : ChoiceFrames (blue) and Coins
        %   tex_CurrentStimImage : texture of CurrentStim (allows alpha
        %       blending)
        %   SCREEN ChoiceStim : ChoiceFrames (beige) and no Coins
        %   SCREEN OutcomeStim : Chose gamble, outcome determined here 
        %   SCREEN SureBetB: Chose sure2 option
        
        
        
        %  COIN IMAGES
        drawCoins = mag1; %#ok<NASGU>
        DrawEightCoin_50; % draws the number of coins specified by "drawCoins" and creates "currentImage" screen
        tex1 = SCREEN( 'MakeTexture', window, currentImage );
        
        drawCoins = mag2; %#ok<NASGU>
        DrawEightCoin_50;
        tex2 = SCREEN( 'MakeTexture', window, currentImage );
        
        drawCoins = sure2;
        DrawEightCoin_50;
        tex3 = SCREEN( 'MakeTexture', window, currentImage );
        
        
        %pImageA = pPic{P1Image};
        %tex_pImageA = SCREEN( 'MakeTexture', window, pImageA );
        
        

        
        % SCREEN CurrentStim part1
        % Aa
        SCREEN('FrameRect', CurrentStim, frameColor, gambleFrame{vertA}{a}, lineWidth);
        % Ab
        SCREEN('FrameRect', CurrentStim, frameColor, gambleFrame{vertA}{b}, lineWidth);
        % B
        SCREEN('FrameRect', CurrentStim, frameColor, sureFrame{vertB}, lineWidth);
        
        % tex_EmptyFrames
        EmptyFramesArray = SCREEN('GetImage', CurrentStim, fullScreen);
        tex_EmptyFrames = SCREEN( 'MakeTexture', window, EmptyFramesArray);

        % SCREEN CurrentStim part2
        SCREEN('FillRect', CurrentStim, black, sureFrame{vertB});
        SCREEN( 'DrawTexture', CurrentStim, tex1, [], gamble{vertA}{a} );
        SCREEN( 'DrawTexture', CurrentStim, tex2, [], gamble{vertA}{b});
        SCREEN( 'DrawTexture', CurrentStim, tex3, [], surePos{vertB});
        
        %%% REDRAW FRAMES FOR OUTCOME STIM
        % Aa
        SCREEN('FrameRect', CurrentStim, frameColor, gambleFrame{vertA}{a}, lineWidth);
        % Ab
        SCREEN('FrameRect', CurrentStim, frameColor, gambleFrame{vertA}{b}, lineWidth);
        % B
        SCREEN('FrameRect', CurrentStim, frameColor, sureFrame{vertB}, lineWidth);        
        
      
        
        
        % tex CurrentStim
        CurrentStimArray = SCREEN('GetImage', CurrentStim, fullScreen);
        tex_CurrentStimImage = SCREEN( 'MakeTexture', window, CurrentStimArray);
        
        
        % COPY CHOICE STIM TO OUTCOME STIM AND DRAW A WINNER RECTANGLE,
        % IS ALL

        % SCREEN ChoiceStim 
        SCREEN('FrameRect', ChoiceStim, FixColor3, gambleFrame{vertA}{a}, lineWidth);
        SCREEN('FrameRect', ChoiceStim, FixColor3, gambleFrame{vertA}{b}, lineWidth);
        SCREEN('FrameRect', ChoiceStim, FixColor3, sureFrame{vertB}, lineWidth);
        
        
        
        % POSSIBLE OUTCOME (1 = left frame, 2 = right frame)
        % A
        Screen('BlendFunction', OutcomeStim, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); % allows screen to apply transparency
        SCREEN( 'DrawTexture', OutcomeStim, tex_CurrentStimImage, [], fullScreen, [], [], alpha_setting ); % copy image with transparency
        
        if (rand < P1)
            % Aa: winner
            winAmount{vertA} = mag1; %#ok<AGROW>
            SCREEN( 'DrawTexture', OutcomeStim, tex1, [], gamble{vertA}{a} );
            SCREEN('FrameRect', OutcomeStim, winnerFrameColor, gambleFrame{vertA}{a}, lineWidth);
        else
            % Ab: loser
            winAmount{vertA} = mag2; %#ok<AGROW>
            SCREEN( 'DrawTexture', OutcomeStim, tex2, [], gamble{vertA}{b});
            SCREEN('FrameRect', OutcomeStim, loserFrameColor, gambleFrame{vertA}{b}, lineWidth);
        end
        
        
        % B is outcome
        winAmount{vertB} = sure2; %#ok<AGROW>
        Screen('BlendFunction', SureBetB, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); % allows screen to apply transparency
        SCREEN( 'DrawTexture', SureBetB, tex_CurrentStimImage, [], fullScreen, [], [], alpha_setting ); % copy transparency
        SCREEN('FillRect', SureBetB, black, sureFrameBackground{vertB});
        SCREEN( 'DrawTexture', SureBetB, tex3, [], surePos{vertB});
        SCREEN('FrameRect', SureBetB, sureFrameColor, sureFrame{vertB}, lineWidth);
        
        % TIME BARS TO OUTCOMES
        Screen('FillRect', OutcomeStim, black, [0,0,fullScreen(3),fullScreen(4)/60], lineWidth);
        Screen('FillRect', OutcomeStim, timeBarColor, time_bar, lineWidth);
        Screen('FillRect', SureBetB, black, [0,0,fullScreen(3),fullScreen(4)/60], lineWidth);
        Screen('FillRect', SureBetB, timeBarColor, time_bar, lineWidth);
        
        
           
        
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %
    %           TRIAL TYPE 6: GAMBLE VS GAMBLE
    %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
    elseif (trialType >= 6) % gamble vs. gamble trials
        
        
        
        
        %  COIN IMAGES
        drawCoins = mag1; %#ok<NASGU>
        DrawEightCoin_50; % draws the number of coins specified by "drawCoins" and creates "currentImage" screen
        tex1 = SCREEN( 'MakeTexture', window, currentImage );
        
        drawCoins = mag2; %#ok<NASGU>
        DrawEightCoin_50;
        tex2 = SCREEN( 'MakeTexture', window, currentImage );
        
        drawCoins = sure1; %#ok<NASGU>
        DrawEightCoin_50;
        tex3 = SCREEN( 'MakeTexture', window, currentImage );
        
        drawCoins = sure2; %#ok<NASGU>
        DrawEightCoin_50;
        tex4 = SCREEN( 'MakeTexture', window, currentImage );
        
         %{
        % NOT GOING TO DRAW PROBABILITY IMAGES
         pImageA = pPic{P1Image};
        tex_pImageA = SCREEN( 'MakeTexture', window, pImageA );
        
        pImageB = pPic{P2Image};
        tex_pImageB = SCREEN( 'MakeTexture', window, pImageB );
        %}
        
        % SCREEN CurrentStim part1
        % Aa
        SCREEN('FrameRect', CurrentStim, frameColor, gambleFrame{vertA}{a}, lineWidth);
        % Ab
        SCREEN('FrameRect', CurrentStim, frameColor, gambleFrame{vertA}{b}, lineWidth);
        
        % BA
        SCREEN('FrameRect', CurrentStim, frameColor, gambleFrame{vertB}{A}, lineWidth);
        % BB
        SCREEN('FrameRect', CurrentStim, frameColor, gambleFrame{vertB}{B}, lineWidth);

        %{
        % pImageA
        SCREEN('FrameRect', CurrentStim, pImageColor, PimageLoc{vertA}, lineWidth);
        % pImageB
        SCREEN('FrameRect', CurrentStim, pImageColor, PimageLoc{vertB}, lineWidth);
        %}
        
        % tex_EmptyFrames
        EmptyFramesArray = SCREEN('GetImage', CurrentStim, fullScreen);
        tex_EmptyFrames = SCREEN( 'MakeTexture', window, EmptyFramesArray);
        
       
        % Aa
        SCREEN( 'DrawTexture', CurrentStim, tex1, [], gamble{vertA}{a});
        % Ab
        SCREEN( 'DrawTexture', CurrentStim, tex2, [], gamble{vertA}{b});
        % BA
        SCREEN( 'DrawTexture', CurrentStim, tex3, [], gamble{vertB}{A});
        % BB
        SCREEN( 'DrawTexture', CurrentStim, tex4, [], gamble{vertB}{B})
        % SCREEN( 'DrawTexture', CurrentStim, tex_pImageA, [], PimageLoc{vertA});
        % SCREEN( 'DrawTexture', CurrentStim, tex_pImageB, [], PimageLoc{vertB});
        
        % Create texture of current stim image for copying with transparency
        CurrentStimArray = SCREEN('GetImage', CurrentStim, fullScreen);
        tex_CurrentStimImage = SCREEN( 'MakeTexture', window, CurrentStimArray);
        
        % RESPONSE
        SCREEN('FrameRect', ChoiceStim, FixColor3, gambleFrame{vertA}{a}, lineWidth);
        SCREEN('FrameRect', ChoiceStim, FixColor3, gambleFrame{vertA}{b}, lineWidth);
        SCREEN('FrameRect', ChoiceStim, FixColor3, gambleFrame{vertB}{A}, lineWidth);
        SCREEN('FrameRect', ChoiceStim, FixColor3, gambleFrame{vertB}{B}, lineWidth);
        
        
       
        
        % POSSIBLE OUTCOMES (1 = left frame, 2 = right frame)
        % A
        Screen('BlendFunction', OutcomeStim, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); % allows screen to apply transparency
        SCREEN( 'DrawTexture', OutcomeStim, tex_CurrentStimImage, [], fullScreen, [], [], alpha_setting ); % copy transparency
        %SCREEN( 'DrawTexture', OutcomeStim, tex_pImageA, [], PimageLoc{vertA}); % copy pImage for reinforcement
        %SCREEN('FrameRect', OutcomeStim, pImageColor, PimageLoc{vertA}, lineWidth);
        
        if (rand < P1)
            % Aa: winner
            winAmount{vertA} = mag1; %#ok<AGROW>
            SCREEN( 'DrawTexture', OutcomeStim, tex1, [], gamble{vertA}{a} );
            SCREEN('FrameRect', OutcomeStim, winnerFrameColor, gambleFrame{vertA}{a}, lineWidth);
        else
            % Ab: loser
            winAmount{vertA} = mag2; %#ok<AGROW>
            SCREEN( 'DrawTexture', OutcomeStim, tex2, [], gamble{vertA}{b});
            SCREEN('FrameRect', OutcomeStim, loserFrameColor, gambleFrame{vertA}{b}, lineWidth);
            
        end
        
        % B
        Screen('BlendFunction', OutcomeStim2, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); % allows screen to apply transparency
        SCREEN( 'DrawTexture', OutcomeStim2, tex_CurrentStimImage, [], fullScreen, [], [], alpha_setting ); % copy transparency
        %SCREEN( 'DrawTexture', OutcomeStim2, tex_pImageB, [], PimageLoc{vertB}); % copy pImage for reinforcement
        %SCREEN('FrameRect', OutcomeStim2, pImageColor, PimageLoc{vertB}, lineWidth);
        
        if (rand < P2)
            % BA: winner
            winAmount{vertB} = sure1; %#ok<AGROW>
            SCREEN( 'DrawTexture', OutcomeStim2, tex3, [], gamble{vertB}{A} );
            SCREEN('FrameRect', OutcomeStim2, winnerFrameColor, gambleFrame{vertB}{A}, lineWidth);
        else
            % BB: loser
            winAmount{vertB} = sure2; %#ok<AGROW>
            SCREEN( 'DrawTexture', OutcomeStim2, tex4, [], gamble{vertB}{B});
            SCREEN('FrameRect', OutcomeStim2, loserFrameColor, gambleFrame{vertB}{B}, lineWidth);
        end
        
        
        % TIME BARS TO OUTCOMES
        Screen('FillRect', OutcomeStim, black, [0,0,fullScreen(3),fullScreen(4)/60], lineWidth);
        Screen('FillRect', OutcomeStim, timeBarColor, time_bar, lineWidth);
        Screen('FillRect', OutcomeStim2, black, [0,0,fullScreen(3),fullScreen(4)/60], lineWidth);
        Screen('FillRect', OutcomeStim2, timeBarColor, time_bar, lineWidth);
       
        
    end % if trialType
    
    
    if (~ DEBUG )
        CurrentImageName = [png_path, 'trial', num2str(trial), '.png'];
        imwrite(CurrentStimArray, CurrentImageName, 'png', 'bitdepth', 8);
    end
    
    
    % SET UP MASKING TEX FOR MASK DISPLAY
    
    % We create a Luminance+Alpha matrix for use as transparency mask:
    % Layer 1 (Luminance) is filled with 'backgroundcolor'.
    maskRadius = YMAX/3; % 300 on a 1440x900 display
    transLayer = 2; %
    [x,y] = meshgrid(-maskRadius:maskRadius, -maskRadius:maskRadius);
    maskblob = ones(floor(2*maskRadius+1), floor(2*maskRadius+1), transLayer) * grayIndex;
    % Layer 2 (Transparency aka Alpha) is filled with gaussian transparenc mask.
    xsd = maskRadius/2.5;
    ysd = maskRadius/2.5;
    maskblob(:, :, transLayer) = round(255 - exp(-((x/xsd).^2)-((y/ysd).^2))*255);
    
    % Build a single transparency mask texture:
    masktex=Screen('MakeTexture', window, maskblob);

    
    
    
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % ================================================================
    %
    %                       END CREATE DISPLAYS
    %                       -------------------
    %                       BEGIN DISPLAYS 
    %
    % ================================================================
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %
    %                   BEGIN FIXATIONS
    %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
          
    
    % GROW DECISION FIXATION
    fixPosition = CENTER;
    when = GetSecs + ifi;
    for i = STARTGROWTH:MAXGROWTH
        fixPosition = fixPosition + GROWTHRATE*EXPAND;
        %SCREEN('FillOval', FixStim, FixColor2, fixPosition);
        %SCREEN('CopyWindow', FixStim, window, fullScreen, fullScreen);
        %pause(.05);
        %flipTime = SCREEN('Flip', window, when);
        %when = flipTime + ifi + 0.02;
    end
    

    SCREEN('FillOval', FixStim, frameColor, fixPosition);
    SCREEN('CopyWindow', FixStim, window, fullScreen, fullScreen);
    flipTime = SCREEN('Flip', window);
    fixTime = flipTime - startTime;
    
    WaitSecs(.05); % wait 50 ms so that the SCREEN change doesn't seem too abrupt
    
    
    
    % wait for eyes to be centered at fixation
    sample = 0; xPositions = XMID*ones(track_avg_pnts,1); yPositions = YMID*ones(track_avg_pnts,1);
    eyes_centered = 0;
    while (eyes_centered == 0)
    
        WaitSecs(.005); % wait 5 ms to keep processors from overloading

        
        if (EYETRACKER)
            eyeTrack = talk2tobii('GET_SAMPLE');
            x = (eyeTrack(1)+eyeTrack(3))*XMAX/2;
            y = (eyeTrack(2)+eyeTrack(4))*YMAX/2;
            
            
            try
                [keyIsDown, secs, keyCode] = KbCheck;
                if keyIsDown
                    if (find(keyCode) == d_key) % draw eyes

                        
                        talk2tobii('EVENT','Recalibration_on', trial);

                        BlankSCREEN = SCREEN('OpenOffSCREENwindow', window, bgd, fullScreen);
                        SCREEN('TextSize', BlankSCREEN, 20);
                        SCREEN('DrawText', BlankSCREEN, 'PRESS ANY KEY TO REDO EYE POSITION & CALIBRATION', floor(.6*XMID), YMID, white, bgd, 1);
                        SCREEN('CopyWindow', BlankSCREEN, window, fullScreen, fullScreen);
                        SCREEN('Flip', window);


                        KbWait;
                            
                        DRAW_EYES;
                    
                        
                        % PRESENT STARTING SCREEN
                        BlankSCREEN = SCREEN('OpenOffSCREENwindow', window, bgd, fullScreen);
                        SCREEN('TextSize', BlankSCREEN, 20);
                        SCREEN('DrawText', BlankSCREEN, 'press "C" and "ENTER" to continue with calibration', floor(.6*XMID), YMID, white, bgd, 1);
                        SCREEN('CopyWindow', BlankSCREEN, window, fullScreen, fullScreen);
                        SCREEN('Flip', window);
                        
                        
                        % WAIT FOR KEY PRESS
                        %choose if you want to redo the calibration
                        %disp('Press space to resume calibration or q to exit calibration and continue tracking');
                        begin_cals = 0;
                        while (begin_cals == 0)
                            tt= input('press "C" and "ENTER" to continue with calibration\n','s');
                            if( strcmpi(tt,'C') || strcmpi(tt,'c') )
                                begin_cals = 1;
                            end
                        end

                        talk2tobii('STOP_RECORD');
                        talk2tobii('STOP_TRACKING');
                        

                        CALIBRATE;

                        SCREEN('CopyWindow', FixStim, window, fullScreen, fullScreen);
                        flipTime = SCREEN('Flip', window, when);
                        
                        
                        
                        talk2tobii('EVENT','Recalibration_off', trial);
                        
                        talk2tobii('START_TRACKING');
                        WaitSecs(0.05);
                        talk2tobii('RECORD');
                        WaitSecs(0.05);
                        
                    elseif (find(keyCode) == esc_key) || (find(keyCode) == q_key) %q or esc to quit win(81;27) mac(41;20)
                        ShowCursor;
                        clear SCREEN;
                        fclose(fid1)
                        fclose('all')
                        if EYETRACKER
                            talk2tobii('STOP_RECORD')
                            WaitSecs(0.05);
                            talk2tobii('STOP_TRACKING');
                            WaitSecs(0.05);
                            talk2tobii('SAVE_DATA', trackFileName, eventFileName, 'TRUNK');
                            WaitSecs(0.5);
                            talk2tobii('DISCONNECT');
                            WaitSecs(0.5);
                            [status,history] = talk2tobii('GET_STATUS')
                        end
                        return
                        
                    end
                end
            end
            
            
            
            
            if (eyeTrack(11) < 4) && (eyeTrack(12) < 4) % don't add to average if eye's are not present
            
                xPositions(mod(sample, track_avg_pnts) + 1) = x;
                yPositions(mod(sample, track_avg_pnts) + 1) = y;
                sample=sample+1;
                
            end
            %fprintf('%1.2f %1.2f\n', x, y);
        else
            [x,y,buttons] = GetMouse(1);
            xPositions(mod(sample, track_avg_pnts) + 1) = x;
            yPositions(mod(sample, track_avg_pnts) + 1) = y;
            sample=sample+1;
        end    
        
        xMean = mean(xPositions);
        yMean = mean(yPositions);

        
        
        
        % -----------------------------------------------------
        %               DRAW EYE/CURSOR POSITION
        % -----------------------------------------------------
        
        
        % plot eye or mouse positions
        xMean = mean(xPositions);
        yMean = mean(yPositions);
%         scatter(xMean, yMean,'xb');
%         axis([0 1440 0 900]);
%         set(gca,'YDir','rev');
%         pause(0.001);  % this pause allows Matlab to escape the main thread long enough to plot data
%         
        
        
        EyeFixPosition = fixPosition + add_to_fix_pos*[-1 -1 1 1];
        if ( IsBounded(xMean, yMean, EyeFixPosition) )
            eyes_centered = 1;
        end
    
    end
    
    
    
    
    
    
    
        
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %
    %   ALL TRIAL TYPES: ADD FIX TO CHOICESTIM, MAKE TEX
    %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
    
    
    % tex ChoiceStim - done here so that fixPosition is the large rather
    % than small circle
    %SCREEN('FillOval', ChoiceStim, FixColor3, fixPosition);
    ChoiceStimImage = SCREEN('GetImage', ChoiceStim, fullScreen);
    tex_ChoiceStim = SCREEN( 'MakeTexture', window, ChoiceStimImage );
    
    
    
    
    
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %
    %                  BEGIN PRESENTATION DISPLAY
    %                  AND WAIT FOR INITIAL RESPONSE
    %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

  
    presTime = GetSecs() - startTime;
    while KbCheck; end % Wait until all keys are released.
    confirm = 0;
    
    % for mouse tracking
    sample = 0; xPositions = XMID*ones(track_avg_pnts,1); yPositions = YMID*ones(track_avg_pnts,1);
    
    sure1_fix_count = 0; sure2_fix_count = 0;
    mag1_fix_count = 0; mag2_fix_count = 0;
    flipCount = 0; waitCount = 0;
    
    if( EYETRACKER )
        talk2tobii('EVENT','TrialOnset', trial);
    end

    
    
    while (confirm == 0)      %(currentTime + 2 > GetSecs)
        
        
        
        % EYE-POSITION PART
        SCREEN('DrawTexture', window, tex_EmptyFrames, fullScreen, fullScreen)
        SCREEN('FillOval', window, FixColor2, fixPosition);
        

        
        % -----------------------------------------------------
        %               EYE/CURSOR POSITION CHECK
        % -----------------------------------------------------
              
        
        if (EYETRACKER)
            eyeTrack = talk2tobii('GET_SAMPLE');
            x = (eyeTrack(1)+eyeTrack(3))*XMAX/2;
            y = (eyeTrack(2)+eyeTrack(4))*YMAX/2;
            
            if (eyeTrack(11) < 4) && (eyeTrack(12) < 4) % don't add to average if eye's are not present
            
                xPositions(mod(sample, track_avg_pnts) + 1) = x;
                yPositions(mod(sample, track_avg_pnts) + 1) = y;
                sample=sample+1;
                
            end
            %fprintf('%1.2f %1.2f\n', x, y);
        else
            [x,y,buttons] = GetMouse(1);
            xPositions(mod(sample, track_avg_pnts) + 1) = x;
            yPositions(mod(sample, track_avg_pnts) + 1) = y;
            sample=sample+1;
        end
        

        
        % -----------------------------------------------------
        %               DRAW EYE/CURSOR POSITION
        % -----------------------------------------------------
        
                
        % plot eye or mouse positions
        xMean = mean(xPositions);
        yMean = mean(yPositions);
        scatter(xMean, yMean,'xb');
        axis([0 1440 0 900]);
        set(gca,'YDir','rev');
        pause(0.001);  % this pause allows Matlab to escape the main thread long enough to plot data
        
        
        
        %{
        % -----------------------------------------------------
        %               CREATE BLENDING IMAGES
        % -----------------------------------------------------
                         
        
        maskRect = [ xMean-maskRadius, yMean-maskRadius, xMean+maskRadius+1, yMean+maskRadius+1]; % center dRect on current eye or mouse position
        
        
        % Step 1: Draw the alpha-mask into the backbuffer. It
        % defines the aperture for foveation: The center of gaze
        % has zero alpha value. Alpha values increase with distance from
        % center of gaze according to a gaussian function and
        % approach 255 at the border of the aperture...
        Screen('BlendFunction', window, GL_ONE, GL_ZERO);
        Screen('DrawTexture', window, masktex, [], maskRect);
        
        % Step 2: Draw peripheral image. It is only drawn where
        % the alpha-value in the backbuffer is 255 or high, leaving
        % the foveated area (low or zero alpha values) alone:
        % This is done by weighting each color value of each pixel
        % with the corresponding alpha-value in the backbuffer
        % (GL_DST_ALPHA).
        Screen('BlendFunction', window, GL_DST_ALPHA, GL_ZERO);
        Screen('DrawTexture', window, tex_EmptyFrames, fullScreen, fullScreen);
        
        % Step 3: Draw foveated image, but only where the
        % alpha-value in the backbuffer is zero or low: This is
        % done by weighting each color value with one minus the
        % corresponding alpha-value in the backbuffer
        % (GL_ONE_MINUS_DST_ALPHA).
        Screen('BlendFunction', window, GL_ONE_MINUS_DST_ALPHA, GL_ONE);
        Screen('DrawTexture', window, tex_CurrentStimImage, fullScreen, fullScreen);
        %}

        
        % IF NOT USING BLENDING SCHEME
        Screen('DrawTexture', window, tex_EmptyFrames, fullScreen, fullScreen);
        if MEMORY
            alternatives_window = window;
        else
            alternatives_window = tex_EmptyFrames;
        end
        
        %SCREEN('DrawTexture', window, tex_EmptyFrames, [], fullScreen)
        if (trialType == 1)
            
            if ( IsBounded(xMean, yMean, sureFrame{vertA}) )
                
                SCREEN('FillRect', alternatives_window, black, sureFrameBackground{vertA});
                SCREEN( 'DrawTexture', alternatives_window, tex3, [], surePos{vertA});
                sure1_fix_count = sure1_fix_count + 1;
                
            elseif ( IsBounded(xMean, yMean, sureFrame{vertB}) )
                
                SCREEN('FillRect', alternatives_window, black, sureFrameBackground{vertB});
                SCREEN( 'DrawTexture', alternatives_window, tex4, [], surePos{vertB});
                sure2_fix_count = sure2_fix_count + 1;
                
            end
            
        elseif (trialType > 1) && (trialType < 6)
            
            if ( IsBounded(xMean, yMean, gambleFrame{vertA}{a}) )
                
                SCREEN( 'DrawTexture', alternatives_window, tex1, [], gamble{vertA}{a});
                mag1_fix_count = mag1_fix_count + 1;
                
            elseif ( IsBounded(xMean, yMean, gambleFrame{vertA}{b}) )
            
                SCREEN( 'DrawTexture', alternatives_window, tex2, [], gamble{vertA}{b});
                mag2_fix_count = mag2_fix_count + 1;
                
            elseif ( IsBounded(xMean, yMean, sureFrame{vertB}) )
                
                SCREEN('FillRect', alternatives_window, black, sureFrameBackground{vertB});
                SCREEN( 'DrawTexture', alternatives_window, tex3, [], surePos{vertB});
                sure2_fix_count = sure2_fix_count + 1;
                
            end
          
        elseif (trialType >= 6)
        
            % A b
            if ( IsBounded(xMean, yMean, gambleFrame{vertA}{a}) )
                
                SCREEN( 'DrawTexture', alternatives_window, tex1, [], gamble{vertA}{a});
                mag1_fix_count = mag1_fix_count + 1;
                
            % A b
            elseif ( IsBounded(xMean, yMean, gambleFrame{vertA}{b}) )
                
                SCREEN( 'DrawTexture', alternatives_window, tex2, [], gamble{vertA}{b});
                mag2_fix_count = mag2_fix_count + 1;
                
            % B A
            elseif ( IsBounded(xMean, yMean, gambleFrame{vertB}{A}) )
                
                SCREEN( 'DrawTexture', alternatives_window, tex3, [], gamble{vertB}{A});
                sure1_fix_count = sure1_fix_count + 1;
                
            % B B
            elseif ( IsBounded(xMean, yMean, gambleFrame{vertB}{B}) )

                SCREEN( 'DrawTexture', alternatives_window, tex4, [], gamble{vertB}{B});
                sure2_fix_count = sure2_fix_count + 1;
        
            end

        end
        


        
        % -----------------------------------------------------
        %               KEYBOARD CHECK
        % -----------------------------------------------------
        
        
        try % KEYBOARD ENTRY
            [keyIsDown, secs, keyCode] = KbCheck;
            if keyIsDown
                if (find(keyCode) == esc_key) || (find(keyCode) == q_key) %q or esc to quit win(81;27) mac(41;20)
                    ShowCursor;
                    clear SCREEN;
                    fclose(fid1)
                    fclose('all')

                    if EYETRACKER
                        talk2tobii('STOP_RECORD');
                        WaitSecs(0.05);
                        talk2tobii('STOP_TRACKING');
                        WaitSecs(0.05);
                        talk2tobii('SAVE_DATA', trackFileName, eventFileName, 'TRUNK');
                        WaitSecs(0.5);
                        talk2tobii('DISCONNECT');
                        WaitSecs(0.05);
                        [status,history] = talk2tobii('GET_STATUS');
                    end
                    return
                elseif (find(keyCode) == p_key)
                    pause = 1;
                    SCREEN('CopyWindow', BlankSCREEN, window, fullScreen, fullScreen);
                    SCREEN('DrawText', window, 'PAUSED', XMID, YMID, white, bgd, 1);
                    flipTime = SCREEN('Flip', window);
                    pauseTime = flipTime - startTime;
                        
                    if( EYETRACKER )
                        talk2tobii('EVENT','Pause', 0);
                    end


                    while (pause == 1)
                        [keyIsDown, secs, keyCode] = KbCheck;
                        if keyIsDown
                            if (find(keyCode) == r_key)
                                pause = 0;
                                recently_paused = 1;
                                releaseTime = GetSecs() - startTime;
                                pauseDuration = releaseTime - pauseTime;
                            end
                        end
                    end
                    
                    if( EYETRACKER )
                        talk2tobii('EVENT','endPause', 0);
                    end                    
                    
                    break
                elseif (find(keyCode) == up_key) || (find(keyCode) == return_key) || (find(keyCode) == space_key) %they chose up, space, or return
                    % FLIP TO CHOSEN BET DISPLAY
                    confirm=1;
                    decisionTime = GetSecs-startTime;
                    break
                end
            end
        end % try key is down
        
        
        

        % -----------------------------------------------------
        %               GAMBEPAD CHECK
        % -----------------------------------------------------        
        
        if USINGGAMEPAD
            if Gamepad('GetButton', gamepadIndex, two_jst) == 1
                confirm=1;
                decisionTime = GetSecs - startTime;
                break
            end
        end
        
        
        if ~ EYETRACKER
            eye_position = [xMean - 3, yMean - 3, xMean + 3, yMean + 3]; % circle of a 6px diamter
            SCREEN( 'FillOval', window, green, eye_position);
        end
        
        
        while (GetSecs < flipTime + ifi)
           waitCount = waitCount + 1; 
        end
        % FLIP TO CHOSEN BET DISPLAY
        flipTime = SCREEN('Flip', window);
        flipCount = flipCount + 1;
        
        
    end % while confirm == 0
    

    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %
    %                  END PRESENTATION DISPLAY
    %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    
        
    % SEND EVENT TO TOBII
    if( EYETRACKER )
        talk2tobii('EVENT','Decision', trial);
    end
   
       
    
    
    
    
    
        
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %
    %           CHECK FOR PAUSE/SEND EVENT TO TOBII
    %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
    
    
    if (recently_paused == 1)
        if (~ DEBUG)
            fprintf(fid3, '\npause_time: %1.3f\nrelease_time: %1.3f\npause_duration: %1.3f\npause_trial: %d\n', pauseTime, releaseTime, pauseDuration, trial)
        end
        startTime = startTime + pauseDuration;
        % CLOSE ALL OPEN TEXTURES AND OFF-SCREEN WINDOWS
        windowPtrs=Screen('Windows');
        if length(windowPtrs) > 1
            
            for openWindowPtr = windowPtrs(length(windowPtrs)):-1:windowPtrs(2)
                Screen('Close', openWindowPtr);
            end
        end
        continue
    end
    
    % DISPLAY CHOICE SCREENS
    Screen('BlendFunction', window, GL_ONE, GL_ZERO);
    SCREEN( 'DrawTexture', window, tex_ChoiceStim, fullScreen, fullScreen); 
    SCREEN('Flip', window);
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %
    %           WAIT FOR EYE-CENTERING AGAIN
    %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
    
    %{
    Screen('FillOval', FixStim, FixColor2, fixPosition);
    Screen('CopyWindow', FixStim, window, fullScreen, fullScreen);
    flipTime = Screen('Flip', window);
    
    
    
    
    % wait for eyes to be centered at fixation again
    sample = 0; xPositions = XMID*ones(track_avg_pnts,1); yPositions = YMID*ones(track_avg_pnts,1);
    eyes_centered = 0;
    while (eyes_centered == 0)
    
        if (EYETRACKER)
            eyeTrack = talk2tobii('GET_SAMPLE');
            x = (eyeTrack(1)+eyeTrack(3))*XMAX/2;
            y = (eyeTrack(2)+eyeTrack(4))*YMAX/2;
            
            
            try
                [keyIsDown, secs, keyCode] = KbCheck;
                if keyIsDown
                    if (find(keyCode) == d_key) % draw eyes

                        
                        %%talk2tobii('EVENT','Recalibration_on', trial);

                        BlankScreen = Screen('OpenOffScreenwindow', window, bgd, fullScreen);
                        Screen('TextSize', BlankScreen, 20);
                        Screen('DrawText', BlankScreen, 'PRESS ANY KEY TO REDO EYE POSITION & CALIBRATION', floor(.6*XMID), YMID, white, bgd, 1);
                        Screen('CopyWindow', BlankScreen, window, fullScreen, fullScreen);
                        Screen('Flip', window);


                        KbWait;
                            
                        DRAW_EYES;
                    
                        
                        % PRESENT STARTING Screen
                        BlankScreen = Screen('OpenOffScreenwindow', window, bgd, fullScreen);
                        Screen('TextSize', BlankScreen, 20);
                        Screen('DrawText', BlankScreen, 'press "C" and "ENTER" to continue with calibration', floor(.6*XMID), YMID, white, bgd, 1);
                        Screen('CopyWindow', BlankScreen, window, fullScreen, fullScreen);
                        Screen('Flip', window);
                        
                        
                        % WAIT FOR KEY PRESS
                        %choose if you want to redo the calibration
                        %disp('Press space to resume calibration or q to exit calibration and continue tracking');
                        begin_cals = 0;
                        while (begin_cals == 0)
                            tt= input('press "C" and "ENTER" to continue with calibration\n','s');
                            if( strcmpi(tt,'C') || strcmpi(tt,'c') )
                                begin_cals = 1;
                            end
                        end

                        %talk2tobii('STOP_RECORD')
                        talk2tobii('STOP_TRACKING');
                        

                        CALIBRATE;

                        Screen('CopyWindow', FixStim, window, fullScreen, fullScreen);
                        flipTime = Screen('Flip', window, when);
                        
                        
                        
                        %%talk2tobii('EVENT','Recalibration_off', trial);
                        
                        talk2tobii('START_TRACKING');
                        WaitSecs(0.05);
                        %talk2tobii('RECORD')    
                        WaitSecs(0.05);
                        
                    elseif (find(keyCode) == esc_key) || (find(keyCode) == q_key) %q or esc to quit win(81;27) mac(41;20)
                    ShowCursor;
                    clear Screen;
                    %fclose(fid1)
                    fclose('all')
                    if EYETRACKER
                        %talk2tobii('STOP_RECORD')
                        WaitSecs(0.05);
                        talk2tobii('STOP_TRACKING');
                        WaitSecs(0.05);
                        %%talk2tobii('SAVE_DATA', trackFileName, eventFileName, 'TRUNK');
                        WaitSecs(0.5);
                        talk2tobii('DISCONNECT');
                        WaitSecs(0.5);
                        [status,history] = talk2tobii('GET_STATUS');
                    end
                    return
                    end
                end
            end
            
            
            
            
            if (eyeTrack(11) < 4) && (eyeTrack(12) < 4) % don't add to average if eye's are not present
            
                xPositions(mod(sample, track_avg_pnts) + 1) = x;
                yPositions(mod(sample, track_avg_pnts) + 1) = y;
                sample=sample+1;
                
            end
            %fprintf('%1.2f %1.2f\n', x, y);
        else
            [x,y,buttons] = GetMouse(1);
            xPositions(mod(sample, track_avg_pnts) + 1) = x;
            yPositions(mod(sample, track_avg_pnts) + 1) = y;
            sample=sample+1;
        end    
        
        xMean = mean(xPositions);
        yMean = mean(yPositions);

        
        
        
        % -----------------------------------------------------
        %               DRAW EYE/CURSOR POSITION
        % -----------------------------------------------------
        
        
        % plot eye or mouse positions
        xMean = mean(xPositions);
        yMean = mean(yPositions);
        scatter(xMean, yMean,'xb');
        axis([0 1440 0 900]);
        set(gca,'YDir','rev');
        pause(0.001);  % this pause allows Matlab to escape the main thread long enough to plot data
        
        
        
        EyeFixPosition = fixPosition + add_to_fix_pos*[-1 -1 1 1];
        if ( IsBounded(xMean, yMean, EyeFixPosition) )
            eyes_centered = 1;
        end
    
    end
    %}
    
     
    
    
    
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %
    %                  BEGIN CHOICE RESPONSE
    %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 


       
    
    % WAIT FOR RESPONSE TO BE MADE, THEN DRAW CHOICE
    % enter kb wait within a getsecs loop for response
    previousResponse=0;
    response = 0;
    confirm = 0; % CHANGE THIS VARIABLE TO 1 TO SKIP CHOICE: CHOICE WILL BE AUTOMATICALLY SELECTED
    choiceCount=0;
    while KbCheck; end % Wait until all keys are released.
    if USINGGAMEPAD
        while Gamepad('GetButton', gamepadIndex, two_jst) == 1; end % Wait until button 2 is released.
    end
    sample = 0; xPositions = XMID*ones(track_avg_pnts,1); yPositions = YMID*ones(track_avg_pnts,1);
    
    
    if( EYETRACKER )
        talk2tobii('EVENT','ChoiceOnset', trial);
    end
    %i=0; % for speed testing
    while (confirm == 0)
        %i = i + 1;
        Screen('BlendFunction', window, GL_ONE, GL_ZERO);
        SCREEN( 'DrawTexture', window, tex_ChoiceStim, fullScreen, fullScreen); 

        WaitSecs(.005); % wait 5 ms to keep processors from overloading


        
        % -----------------------------------------------------
        %               EYE/CURSOR POSITION CHECK
        % -----------------------------------------------------
        
        
        if (EYETRACKER)
            eyeTrack = talk2tobii('GET_SAMPLE');
            x = (eyeTrack(1)+eyeTrack(3))*XMAX/2;
            y = (eyeTrack(2)+eyeTrack(4))*YMAX/2;
            xPositions(mod(sample, track_avg_pnts) + 1) = x;
            yPositions(mod(sample, track_avg_pnts) + 1) = y;
            sample=sample+1;
        else
            [x,y,buttons] = GetMouse(1);
            xPositions(mod(sample, track_avg_pnts) + 1) = x;
            yPositions(mod(sample, track_avg_pnts) + 1) = y;
            sample=sample+1;
        end
        

        
        
        % -----------------------------------------------------
        %               DRAW EYE/CURSOR POSITION
        % -----------------------------------------------------
        
        
        % plot eye or mouse positions
        xMean = mean(xPositions);
        yMean = mean(yPositions);
        scatter(xMean, yMean,'xb');
        axis([0 1440 0 900]);
        set(gca,'YDir','rev');
        pause(0.001);  % this pause allows Matlab to escape the main thread long enough to plot data
        
        
        
        % CHECK BOUNDS OF EYE POSITION AND RECORD RESPONSE
        if ( IsBounded(xMean, yMean, sureFrameRect{1}) )
            
            SCREEN('FrameRect', window, confirmFrameColor, sureFrameRect{1}, lineWidth);
            if topRight; response = 2; else response = 1; end
            respTime = GetSecs-startTime;

            
        elseif ( IsBounded(xMean, yMean, sureFrameRect{2}) )
            
            SCREEN('FrameRect', window, confirmFrameColor, sureFrameRect{2}, lineWidth);
            if topRight; response = 1; else response = 2; end
            respTime = GetSecs-startTime;
            
        end
        
        
        
        
        if ~ EYETRACKER
            
            eye_position = [xMean - 3, yMean - 3, xMean + 3, yMean + 3]; % circle of a 6px diamter
            SCREEN( 'FillOval', window, green, eye_position);
            
        end
        
        
                % -----------------------------------------------------
        %               GAMBEPAD CHECK
        % -----------------------------------------------------        
        
        
        if USINGGAMEPAD
            
            if Gamepad('GetButton', gamepadIndex, left_jst) == 1 %they chose left
                
                response = 1;
                if response ~= previousResponse; choiceCount = choiceCount + 1; end
                %previousResponse = response;
                respTime = GetSecs-startTime;
                
            elseif Gamepad('GetButton', gamepadIndex, right_jst) == 1 %they chose right win(39) mac(79)
                
                response = 2;
                if response ~= previousResponse; choiceCount = choiceCount + 1; end
                %previousResponse = response;
                respTime = GetSecs-startTime;
                
            elseif Gamepad('GetButton', gamepadIndex, two_jst) == 1 %they pressed 2
                
                if response ~= 0
                    
                    % thest flips need to be within the loop, otherwise the break command will skip it.
                    if (previousResponse == 1) % selected left
                        
                        if (topRight == 0) % left is top
                            SCREEN('FrameRect', window, confirmFrameColor, sureFrameRect{1}, lineWidth);
                        else % left is bottom
                            SCREEN('FrameRect', window, confirmFrameColor, sureFrameRect{2}, lineWidth);
                        end
                        
                    elseif (previousResponse == 2) % selected right
                        
                        if (topRight == 0) % right is bottom
                            SCREEN('FrameRect', window, confirmFrameColor, sureFrameRect{2}, lineWidth);
                        else % right is top
                            SCREEN('FrameRect', window, confirmFrameColor, sureFrameRect{1}, lineWidth);
                        end
                        
                    end
                    
                    flipTime = SCREEN('Flip', window);
                    confirm=1;
                    break
                    
                end
                
            end
            
        end % if USINGGAMEPAD
        
        
        
        
        % -----------------------------------------------------
        %               KEYBOARD CHECK
        % -----------------------------------------------------
        
        
        try
            
            [keyIsDown, secs, keyCode] = KbCheck;
            if keyIsDown
                if (find(keyCode) == left_key)  %they chose left  win(37) mac(80)
                    
                    response = 1;
                    if response ~= previousResponse; choiceCount = choiceCount + 1; end
                    previousResponse = response;
                    respTime = GetSecs-startTime;
                                        
                elseif (find(keyCode) == right_key) %they chose right win(39) mac(79)
                    
                    response = 2;
                    if response ~= previousResponse; choiceCount = choiceCount + 1; end
                    previousResponse = response;
                    respTime = GetSecs-startTime;
                    
                elseif (find(keyCode) == q_key) %q or esc to quit win(81;27) mac(41;20) % ||(find(keyCode) == esc_key) % removed to simplify
                    
                    ShowCursor;
                    clear SCREEN;
                    fclose(fid1)
                    fclose('all')
                    rmpath(genpath(EXP_DIR));
                    if USINGGAMEPAD
                        Gamepad('Unplug')
                    end
                    if EYETRACKER
                        talk2tobii('STOP_RECORD');
                        WaitSecs(0.05);
                        talk2tobii('STOP_TRACKING');
                        WaitSecs(0.05);
                        talk2tobii('SAVE_DATA', trackFileName, eventFileName, 'TRUNK');
                        WaitSecs(0.5);
                        talk2tobii('DISCONNECT');
                        WaitSecs(0.05);
                        [status,history] = talk2tobii('GET_STATUS');
                    end
                    return
                    
                elseif (find(keyCode) == up_key) || (find(keyCode) == return_key) || (find(keyCode) == space_key) %they chose up, space, or return
                    
                    if response ~= 0
                        
                        % thest flips need to be within the loop, otherwise the break command will skip it.
                        if (previousResponse == 1) % selected left
                            
                            if (topRight == 0) % left is top
                                SCREEN('FrameRect', window, confirmFrameColor, sureFrameRect{1}, lineWidth);
                            else % left is bottom
                                SCREEN('FrameRect', window, confirmFrameColor, sureFrameRect{2}, lineWidth);
                            end
                            
                        elseif (previousResponse == 2) % selected right
                            
                            if (topRight == 0) % right is bottom
                                SCREEN('FrameRect', window, confirmFrameColor, sureFrameRect{2}, lineWidth);
                            else % right is top
                                SCREEN('FrameRect', window, confirmFrameColor, sureFrameRect{1}, lineWidth);
                            end
                            
                        end
                        
                        flipTime = SCREEN('Flip', window);
                        confirm=1;
                        break
                    
                    end
                    
                end
            end
            
        end % try keyboard

       

        % -----------------------------------------------------
        %               DRAW RED SELECTION FRAME
        % -----------------------------------------------------        

        if (response ~= previousResponse) % only draw and flip if choice has changed to avoid flicker during tracking (cheap workaround)
            
            if (response == 1) % selected left
                
                if (topRight == 0) % left is top
                    SCREEN('FrameRect', window, chosenFrameColor, sureFrameRect{1}, lineWidth);
                else % left is bottom
                    SCREEN('FrameRect', window, chosenFrameColor, sureFrameRect{2}, lineWidth);
                end
                
            elseif (response == 2) % selected right
                
                if (topRight == 0) % right is bottom
                    SCREEN('FrameRect', window, chosenFrameColor, sureFrameRect{2}, lineWidth);
                else % right is top
                    SCREEN('FrameRect', window, chosenFrameColor, sureFrameRect{1}, lineWidth);
                end
                
            end
            
        previousResponse = response;    
        
        % FLIP TO CHOSEN BET DISPLAY
        flipTime = SCREEN('Flip', window);
        
        end

    
        %fprintf([int2str(i), '\n']);
        
    end % while confirm == 0
    
    
    
    
    if EYETRACKER
        talk2tobii('EVENT','ChoiceOff', trial);
    end
    
    
    
    
    
    
   
    
    
    if (response == 0)
        ShowCursor;
        clear SCREEN;
        if USINGGAMEPAD
            Gamepad('Unplug')
        end
        if EYETRACKER
            talk2tobii('STOP_RECORD');
            WaitSecs(0.05);
            talk2tobii('STOP_TRACKING');
            WaitSecs(0.05);
            talk2tobii('SAVE_DATA', trackFileName, eventFileName, 'TRUNK');
            WaitSecs(0.5);
            talk2tobii('DISCONNECT');
            WaitSecs(0.05);
            [status,history] = talk2tobii('GET_STATUS');
        end
        rmpath(genpath(EXP_DIR));
        return
    end
    
    
       
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %
    %                  BEGIN DELAY PERIOD
    %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
    

    %SCREEN('CopyWindow', BlankSCREEN, window, fullScreen, fullScreen);
    %flipTime = SCREEN('Flip', window);
    delayTime = GetSecs-startTime;
    WaitSecs(ISI1);
    
    
    if (GLcondition == 1)
        PsychPortAudio('FillBuffer', GainAudio, youGetData);
        PsychPortAudio('Start', GainAudio, 1, 0, 0);
    else
        PsychPortAudio('FillBuffer', LossAudio, youLoseData);
        PsychPortAudio('Start', LossAudio, 1, 0, 0);
    end
    
    
    WaitSecs(1);
    
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %
    %                  DETERMINE CHOICE
    %                  BEGIN OUTCOME DISPLAY
    %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
    
    
    
    % DRAW OUTCOME DISPLAY BASED UPON SELECTION AND OUTCOME CONTINGENCIES
    gamb=0; % temporarily specify gamb (1 means gambled or high risk gamble, 0 means safe or low risk gamble) outside range of values
    %
    %   RESPONSE
    %
    
    
    
    
    
    if ( (response - 1) == topRight) % selected top (response: 1 = L, 2 = R; topRight: 0 = leftArrowForTop, 1 = rightArrowForTop)
        
        
        if (vertA == 1) % top is either SureBetA or gamble; selected SureBetA or gambleA (from mag columns)
            
            winnings = winnings + valence*winAmount{vertA} ;
            coinsWon = winAmount{vertA};
            
            if (trialType == 1) % selected SureBetA
                SCREEN('CopyWindow', SureBetA, window, fullScreen, fullScreen);
                gamb=99;
            else                % selected gambleA (from mag columns)
                SCREEN('CopyWindow', OutcomeStim, window, fullScreen, fullScreen);
                gamb=1;
            end
            
        else % top is SureBetB or is gambleB ; selected SureBetB or gambleB
            
            winnings = winnings + valence*winAmount{vertB} ;
            coinsWon = winAmount{vertB};
            
            if (trialType < 6)  % selected SureBetB
                SCREEN('CopyWindow', SureBetB, window, fullScreen, fullScreen);
                gamb=0;
                if (trialType == 1); gamb = 99; end; % set 99 for surebet trials
            else                % selected gambleB (from sure columns)
                SCREEN('CopyWindow', OutcomeStim2, window, fullScreen, fullScreen);
                gamb=2;
            end
            
        end
        
    else % selected bottom
        
        if (vertA == 1) % bottom is SureBetB or is gambleB ; selected SureBetB or gambleB
            
            winnings = winnings + valence*winAmount{vertB} ;
            coinsWon = winAmount{vertB};
            
            if (trialType < 6)  % selected SureBetB
                SCREEN('CopyWindow', SureBetB, window, fullScreen, fullScreen);
                gamb=0;
                if (trialType == 1); gamb = 99; end; % set 99 for surebet trials
            else                % selected gamble2 (from sure columns)
                SCREEN('CopyWindow', OutcomeStim2, window, fullScreen, fullScreen);
                gamb=2;
            end
            
        else % bottom is either SureBetA or gamble; selected SureBetA or gambleA
            
            winnings = winnings + valence*winAmount{vertA} ;
            coinsWon = winAmount{vertA};
            
            if (trialType == 1)
                SCREEN('CopyWindow', SureBetA, window, fullScreen, fullScreen);
                gamb=99;
            else
                SCREEN('CopyWindow', OutcomeStim, window, fullScreen, fullScreen);
                gamb=1;
            end
            
        end
        
    end
    
    
     
    
    flipTime = SCREEN('Flip', window);
    outcomeTime = flipTime-startTime; % start of the outcome display

    
    
    
    % PLAY  WINNING AUDIO
    if (GLcondition == 1)
        PsychPortAudio('Stop', GainAudio, 1);
    else
        PsychPortAudio('Stop', LossAudio, 1);
    end
    PsychPortAudio('FillBuffer', winAudio, wonWaveData);
    playing = 0; % the file is not playing
    if  (coinsWon > 0) % if coins have been won, begin playing audio
        PsychPortAudio('Start', winAudio, coinsWon, 0, 1);
        playing = 1; % the file is playing
    end
    
    
    
    % display outcome for n sec
    WaitSecs(3);
    
  %{  
    % show outcome display until participant escapes
    while KbCheck; end % Wait until all keys are released.
    confirm = 0;
    while (confirm == 0)      %(currentTime + 2 > GetSecs)
    
            % -----------------------------------------------------
        %               KEYBOARD CHECK
        % -----------------------------------------------------
        
        
        try % KEYBOARD ENTRY
            [keyIsDown, secs, keyCode] = KbCheck;
            if keyIsDown
                if  (find(keyCode) == up_key) || (find(keyCode) == return_key) || (find(keyCode) == space_key) %they chose up, space, or return
                    % FLIP TO CHOSEN BET DISPLAY
                    confirm=1;
                    endOutcomeTime = GetSecs-startTime;
                end
            end
        end % try key is down
        
        
        % -----------------------------------------------------
        %               GAMBEPAD CHECK
        % -----------------------------------------------------        
        
        if USINGGAMEPAD
            if Gamepad('GetButton', gamepadIndex, two_jst) == 1
                confirm=1;
                endOutcomeTime = GetSecs - startTime;
            end
        end
    
    end
    
    %}
    
    
    
    
    
    

        
    if (playing == 1)
        PsychPortAudio('Stop', winAudio, 1);
    end
    
    
    
    % CLOSE ALL OPEN TEXTURES AND OFF-SCREEN WINDOWS
    windowPtrs=Screen('Windows');
    if length(windowPtrs) > 1
        
        for openWindowPtr = windowPtrs(length(windowPtrs)):-1:windowPtrs(2)
            Screen('Close', openWindowPtr);
        end
    end
    
    BlankSCREEN = SCREEN('OpenOffSCREENwindow', window, bgd, fullScreen);
    Screen('FillRect', BlankSCREEN, black, [0,0,fullScreen(3),fullScreen(4)/60], lineWidth);
    Screen('FillRect', BlankSCREEN, timeBarColor, time_bar, lineWidth);
    Screen('CopyWindow', BlankSCREEN, window, fullScreen, fullScreen);
    SCREEN('Flip', window);
    
    
    
     
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % ================================================================
    %
    %                       END DISPLAY LOOP
    %                       -------------------
    %                       BEGIN WRITING OUTPUT
    %
    % ================================================================
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    
    
    
    total_winnings = total_winnings + valence*coinsWon;

    
    % gamb
    %   0 = no gamble
    %   1 = gamble from mag column (P1)
    %   2 = gamble from sure column (P2)
    %   99 = surebet vs. surebet trial
    % PRINT OUTPUT
       
    if DEBUG
    
        fprintf('trial = %d\ngamb = %d\ncoinsWon = %d\na = %d\nresp = %d\n', trial, gamb, coinsWon, a, response)
        fprintf('sure1 fix = %d\nsure2 fix= %d\nmag1 fix = %d\nmag2 fix = %d\n\n',sure1_fix_count, sure2_fix_count, mag1_fix_count, mag2_fix_count);
        
    else
        RT = decisionTime - presTime;
        % PRINT DATA TO OUTPUT FILE
        fprintf(fid1,'%1d %1d %1d %1d %1d %1d %1d ', trial, trialType, sure1, sure2, mag1, mag2, response);
        fprintf(fid1,'%1d %1d %1d %1.2f %1d %1.2f %1d %1d %1d %1d ', gamb, coinsWon, P1Image, P1, P2Image, P2, vert, topRight, side, sideB);
        fprintf(fid1, '%1.3f %1.3f %1.3f %1.3f %1.3f %1.3f %1.3f %1d ', fixTime, presTime, decisionTime, respTime, delayTime, outcomeTime, RT, choiceCount);
        fprintf(fid1, '%1d %1d %1d %1d %1d\n', sure1_fix_count, sure2_fix_count, mag1_fix_count, mag2_fix_count, GLcondition);
        % PRINT TO TOTAL WINNINGS FILE
        fprintf(fid2, '%1d\n', total_winnings);
    
    end
    
    
    WaitSecs(ISI2);
    
    

    
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %
    %                  BEGIN WINNINGS DISPLAY
    %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

    
    if (mod(trial, 29) == 0) %
        
        WinningsDisplay = SCREEN('OpenOffSCREENwindow', window, black, fullScreen);
        DrawWinnings; % function to draw the image 'currentimage'
        tex5 = SCREEN( 'MakeTexture', window, winImage );
        SCREEN( 'DrawTexture', WinningsDisplay, tex5, [], winningsBottom);
        SCREEN('FrameRect', WinningsDisplay, brown, winningsBox, 2*lineWidth);
        SCREEN('CopyWindow', WinningsDisplay, window, fullScreen, fullScreen);
        
        SCREEN('Flip', window);
        
        
        while KbCheck; end % Wait until all keys are released.
        response = 0;
        while (response == 0)
            
            [keyIsDown, secs, keyCode] = KbCheck;
            if keyIsDown
                if (find(keyCode) == h_key) || (find(keyCode) == up_key) || (find(keyCode) == right_key)  %press h or up arrow to break
                    response = 1;
                end
            end
            
            if USINGGAMEPAD
                if Gamepad('GetButton', gamepadIndex, two_jst) == 1
                    response = 1;
                end
            end
            
        end % while response == 0
    end % mod(trial, ntrials_per_block) == 0
    
    trial = trial + 1;
    
    
end % END TRIAL LOOP


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ================================================================
%
%                       END TRIAL LOOPS
%
% ================================================================
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% FINAL WINNINGS DISPLAY
%        WinningsDisplay = SCREEN('OpenOffSCREENwindow', window, black, fullScreen);
%        winnings = total_winnings; 
%        DrawWinnings; % function to draw the image 'currentimage'
%         tex5 = SCREEN( 'MakeTexture', window, winImage );
%         SCREEN( 'DrawTexture', WinningsDisplay, tex5, [], winningsBottom);
%         SCREEN('FrameRect', WinningsDisplay, brown, winningsBox, 2*lineWidth);
%         SCREEN('CopyWindow', WinningsDisplay, window, fullScreen, fullScreen);
%         
%         SCREEN('Flip', window);
%         
%         
%         while KbCheck; end % Wait until all keys are released.
%         response = 0;
%         while (response == 0)
%             
%             [keyIsDown, secs, keyCode] = KbCheck;
%             if keyIsDown
%                 if (find(keyCode) == h_key) || (find(keyCode) == up_key) || (find(keyCode) == right_key)  %press h or up arrow to break
%                     response = 1;
%                 end
%             end
%             
%             if USINGGAMEPAD
%                 if Gamepad('GetButton', gamepadIndex, two_jst) == 1
%                     response = 1;
%                 end
%             end
%             
%         end % while response == 0










%PsychPortAudio('Close', pickAudio);
%PsychPortAudio('Close', youGetAudio);
PsychPortAudio('Close', winAudio);



% FINISHED LOOP DISPLAY
ClosingSCREEN = SCREEN('OpenOffSCREENwindow', window, bgd, fullScreen);
SCREEN('TextSize', ClosingSCREEN, 32);
SCREEN('DrawText', ClosingSCREEN, 'FINISHED', XMID, YMID, white, bgd, 1);
SCREEN('CopyWindow', ClosingSCREEN, window, fullScreen, fullScreen);
SCREEN('Flip', window);
WaitSecs(.2);
ShowCursor;
clear SCREEN;
fclose('all')

if USINGGAMEPAD
    Gamepad('Unplug')
end

if EYETRACKER
    talk2tobii('STOP_RECORD');
    WaitSecs(0.05);
    talk2tobii('STOP_TRACKING');
    WaitSecs(0.05);
    talk2tobii('SAVE_DATA', trackFileName, eventFileName,'TRUNK');
    WaitSecs(0.05);
    talk2tobii('DISCONNECT');
    WaitSecs(0.05);
    [status,history] = talk2tobii('GET_STATUS');
end
%    clear dio
% reduce resolution of screen shots
process_images(subjectNumber);



return




