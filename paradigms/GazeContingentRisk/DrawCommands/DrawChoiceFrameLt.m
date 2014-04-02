
%   EMPTY FRAME 1
Screen('DrawLine', CurrentStim, chosenFrameColor, ... % top of frame
    frame1pos(1)-4, frame1pos(2)-4, frame1pos(3)+4, frame1pos(2)-4, lineWidth); 

Screen('DrawLine', CurrentStim, chosenFrameColor, ... % bottom of frame
    frame1pos(1)-4, frame1pos(4)+4, frame1pos(3)+4, frame1pos(4)+4, lineWidth); 

Screen('DrawLine', CurrentStim, chosenFrameColor, ... % left side of frame
    frame1pos(1)-4, frame1pos(2)-4, frame1pos(1)-4, frame1pos(4)+4, lineWidth); 

Screen('DrawLine', CurrentStim, chosenFrameColor, ... % right side of frame
    frame1pos(3)+4, frame1pos(2)-4, frame1pos(3)+4, frame1pos(4)+4, lineWidth); 
