%   PSYCHTOOLBOX SCRIPT
%{

Trials are randomized so that each deck will not be offered more than twice in a row,
and that no more than 2 losses occur in a row


%}


Screen('Preference', 'SkipSyncTests',1); % keep active for test only

DEBUG = 0;
track_avg_pnts = 5;


if strcmp(computer, 'MACI')
    left_key = 80;
    right_key = 79;
    esc_key = 41;
    q_key = 20;
    up_key = 82;
    h_key = 11;
    return_key = 40;
else
    left_key = 37;
    right_key = 39;
    esc_key = 81;
    q_key = 27;
    up_key = 38;
    h_key = 72;
    return_key = 79;
end
left_jst = 5;
right_jst = 6;
two_jst = 2;



WaitSecs(0.5); % wait .5 seconds to give time for keyboard to clear
% CHECK FOR GAMEPAD CONNECTION






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
    fullScreen = [];
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



%	INITIALIZE AUDIO
InitializePsychSound(0); % initialize sound driver; (0) means ms timing is not crucial, (1) if it is
PsychPortAudio initialized. Will use CoreAudio for audio.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%    ENTER SUBJECT INFO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if ~ DEBUG
    subjectNumber = input('\n\nenter subject number\n');
    outputFileName1 = ['logs/', int2str(subjectNumber), '.txt'];
    while (exist(outputFileName1, 'file') > 0)
        subjectNumber = input('\nthat subject number already exists\ntry a new number\n');
        outputFileName1 = ['logs/goods', int2str(subjectNumber), '.txt'];
    end
        
    %   OPEN OUTPUT FILES
    fid1 = fopen(outputFileName1, 'w');
    fprintf(fid1, 'subjectNumber trial ChoiceStim\n');
    trackFileName = ['logs/Eye_tracking_goods', num2str(subjectNumber), '.txt'];
eventFileName = ['logs/Events_goods', num2str(subjectNumber), '.txt'];

end










%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%    INITIALIZE STIMULUS IMAGE INFO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


STIM_DIMS_PARAMS_GOODS;


% DEFINE IMAGE FILES
% ALL IMAGES GET RESIZED TO STIM_SIZE
stim_size = round([(fullScreen(3)-fullScreen(1))/4, (fullScreen(3)-fullScreen(1))/4]);
number_of_pairs = 23;
for pic = 1:number_of_pairs

    current_pica = ['Pics/ChoicePrefImages/', num2str(pic), 'a.jpg'];
    current_picb = ['Pics/ChoicePrefImages/', num2str(pic), 'b.jpg'];
    pPic{pic}{1} = imread(current_pica);
    pPic{pic}{2} = imread(current_picb);
    pPic{pic}{1} = imresize(pPic{pic}{1}, stim_size);
    pPic{pic}{2} = imresize(pPic{pic}{2}, stim_size);
    
    %imshow(pPic{pic}{1})
    %pause(0.5);  % this pause allows Matlab to escape the main thread long enough to plot data
    %imshow(pPic{pic}{2})
    %pause(0.5);  % this pause allows Matlab to escape the main thread long enough to plot data

end
% stim_size is first defined
nrows = 1;
ncols = 2;
[ stim_size, location_coords ] = create_location_coords( nrows, ncols, fullScreen, stim_size, h_margin, v_margin, h_gap, v_gap);

Pos{1} = location_coords(1,:); % LEFT POSITION
Pos{2} = location_coords(2,:); % RIGHT POSITION








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%    INITIALIZE TRIALS ARRAY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% SET UP ARRAY OF TRIALS SPECIFIED BY TRIAL TYPE
n_trial_types = 23; % number of trial types
n_repeats = 1; % number of times to repeat each trial per block
trials_list = zeros(n_trial_types*n_repeats, 1);
for trial = 1:n_trial_types
    for repeat = 1:n_repeats
        trials_list( (trial-1)*n_repeats + repeat) = trial;
    end
end





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%                 BEGIN TRAINING BLOCK LOOP
%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% WAIT FOR USER/EXPERIMENTER TO BEGIN PARADIGM
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




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%                 BEGIN TRIALS LOOP
%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% BEGIN EYETRACKER RECORDING
if( EYETRACKER )
    talk2tobii('CLEAR_DATA');
    WaitSecs(0.05);
    talk2tobii('RECORD');    
    WaitSecs(0.05);
    talk2tobii('EVENT','Start', 0);
end



for trial = 1:length(trials_list)
	

	CurrentStim = SCREEN('OpenOffSCREENwindow', window, bgd, fullScreen);
	

	if round(rand()) == 1
		LeftImage  = pPic{trial}{1};
		RightImage = pPic{trial}{2};
	else
		LeftImage  = pPic{trial}{2};
		RightImage = pPic{trial}{1};
	end
	
	
	
	%   PRESENT CHOICE STIMULI
	Screen('PutImage', CurrentStim, LeftImage , Pos{1});
	Screen('PutImage', CurrentStim, RightImage, Pos{2});
	Screen('CopyWindow', CurrentStim, window, fullScreen, fullScreen);
	stimTime = Screen(window,'Flip');
	
	
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%      BEGIN CHOICE SELECTION LOOP
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	
	while KbCheck; end % Wait until all keys are released.
	
    
    if( EYETRACKER )
        talk2tobii('EVENT','TrialOnset', trial);
    end

    
    
	confirm = 0;
	choice = 0; % reinitialize choice
    sample = 0; xPositions = XMID*ones(track_avg_pnts,1); yPositions = YMID*ones(track_avg_pnts,1);

	while (confirm == 0)

        
        
        
        % -----------------------------------------------------
        %               EYE/CURSOR POSITION CHECK
        % -----------------------------------------------------
              

        if (EYETRACKER)
            eyeTrack = talk2tobii('GET_sample');
            x = (eyeTrack(1)+eyeTrack(3))*XMAX/2;
            y = (eyeTrack(2)+eyeTrack(4))*YMAX/2;
            if (eyeTrack(11) < 4) && (eyeTrack(12) < 4) % don't add to average if eye's are not present
            
                xPositions(mod(sample, track_avg_pnts) + 1) = x;
                yPositions(mod(sample, track_avg_pnts) + 1) = y;
                sample=sample+1;
                
            end
            %fprintf('%1.2f %1.2f\n', x, y);
        else
            [x,y,buttons] = GetMouse();
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
        axis([fullScreen(1), fullScreen(3), fullScreen(2), fullScreen(4)]);
        set(gca,'YDir','rev');
        pause(0.001);  % this pause allows Matlab to escape the main thread long enough to plot data
        
        
        
        if ( IsBounded(xMean, yMean, Pos{1}) )
            
            choice = 1;
            
        elseif ( IsBounded(xMean, yMean, Pos{2}) )
            
            choice = 2;
            
        end
        
        if USINGGAMEPAD
            
            if Gamepad('GetButton', gamepadIndex, left_jst) == 1 %they chose left
                
%                 choice = 1;
%                 alternate = 2;
            elseif Gamepad('GetButton', gamepadIndex, right_jst) == 1 %they chose right win(39) mac(79)
                
%                 choice = 2;
%                 alternate = 1;
                
            elseif Gamepad('GetButton', gamepadIndex, two_jst) == 1 %they pressed 2
                
                if choice ~= 0
                    confirm = 1;
                end
            end
            
        end % if USINGGAMEPAD
        
     
        
        
		
        % WAIT AND CLASSIFY RESPONSE
%         try % KEYBOARD ENTRY
%             [keyIsDown, secs, keyCode] = KbCheck;
%             if keyIsDown
%                 
%                 %%%%%%%%%%%%%   POSITION 1 Left
%                 if (find(keyCode) == left_key)  %they chose left
%                     choice = 1;
%                     alternate = 2;
%                     %%%%%%%%%%%%%   POSITION 2 Right
%                 elseif (find(keyCode) == right_key) %they chose right
%                     choice = 2;
%                     alternate = 1;
%                 elseif (find(keyCode) == return_key) %they chose Return
%                     if choice ~= 0
%                         confirm = 1;
%                     end
%                 elseif (find(keyCode) == esc_key) || (find(keyCode) == q_key) %they chose Esc or q
%                     
%                     % PREPARE TO QUIT PROGRAM
%                     WaitSecs(1)
%                     ShowCursor;
%                     fclose(fid1);
%                     windowPtrs=Screen('Windows');
%                     Screen('Close', windowPtrs);
%                     clear Screen;
%                     return
%                     
%                 end % if keyCode
%             end % if key_is_down
%         end % try KEYBOARD ENTRY
		
        


        
        
		if (confirm == 0) % has not made a choice yet
			
            if (choice == 0)
            % REDRAW A NEW BOX FRAME ONTO PREVIOUS WINDOW
				
                Screen('CopyWindow', CurrentStim, window, fullScreen, fullScreen);
				
            %%%%%%%%%%%%%   POSITION 1 Left
            elseif (choice == 1)
				
				% REDRAW A NEW BOX FRAME ONTO PREVIOUS WINDOW
				Screen('CopyWindow', CurrentStim, window, fullScreen, fullScreen);
				frame_coords = get_frame_coords(Pos{1}, lineWidth);
				Screen('FrameRect', window, chooseFrameColor, frame_coords, lineWidth);
				
			%%%%%%%%%%%%%   POSITION 2 Right
			elseif (choice == 2)
				
				% REDRAW A NEW BOX FRAME ONTO PREVIOUS WINDOW
				Screen('CopyWindow', CurrentStim, window, fullScreen, fullScreen);
				frame_coords = get_frame_coords(Pos{2}, lineWidth);
				Screen('FrameRect', window, chooseFrameColor, frame_coords, lineWidth);
				
            end
			
            
            % draw mouse position if no eyetracker
            if ~ EYETRACKER
                eye_position = [xMean - 3, yMean - 3, xMean + 3, yMean + 3]; % circle of a 6px diamter
                SCREEN( 'FillOval', window, green, eye_position);
            end
            
            flipTime = Screen(window,'Flip');

            
            
		elseif (confirm == 1) % need to execute decision me
			
			
			%%%%%%%%%%%%%   EXECUTE CHOICE
			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			
            % SEND EVENT TO TOBII
            if ( EYETRACKER )
                talk2tobii('EVENT','Decision', trial);
            end
            
            
            
			% REDRAW A NEW BOX FRAME ONTO PREVIOUS WINDOW
			Screen('CopyWindow', CurrentStim, window, fullScreen, fullScreen);
			Screen('FrameRect', window, chosenFrameColor, frame_coords, lineWidth); % frame_coords are carried over from last position
			confirmTime = Screen(window,'Flip');
			responseTime = confirmTime - stimTime;
			WaitSecs(.5);
			
				
			
			%%%%%%%%%%%% DISPLAY INTERTRIAL INTERVAL
			blankScreen= SCREEN('OpenOffSCREENwindow', window, bgd, fullScreen);
			Screen('CopyWindow', blankScreen, window, fullScreen, fullScreen);
			Screen(window,'Flip');
			WaitSecs(1 + rand());
			
			
            
            
            
			
			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			%           PRINT TRIAL DATA
			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            
            % time_variables: startTime stimTime confirmTime responseTime(real RT)
            
			if ~ DEBUG % to avoid (un)commenting this section when subject is not defined
				fprintf(fid1, '%1d %1d %1d %1.5f %1.5f %1.3f\n', ...
                    subjectNumber, trial, choice, stimTime, confirmTime, responseTime);
			end
			
		end % confirm == 0 or 1
		
	end % while confirm
		
end % end trials loop
	

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

	


ShowCursor;
clear SCREEN;
fclose(fid1);
windowPtrs=Screen('Windows');
Screen('Close', windowPtrs);
    
    
    
    
    
    
    
