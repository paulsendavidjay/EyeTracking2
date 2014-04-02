

% to check connection to Tobii; the second value in the array returned
% should be 1 if connected
%hostName = '169.254.225.234'; % psych
hostName = '169.254.7.31'; % fuqua

portName = '4455';
talk2tobii('CONNECT',hostName, portName);

[status,history] = talk2tobii('GET_STATUS')


talk2tobii('DISCONNECT',hostName);



fullSCREEN = [];
[window, fullSCREEN] = SCREEN('OpenWindow', 1, [100 100 100], fullSCREEN, 32);
res = [fullSCREEN(3) - fullSCREEN(1), fullSCREEN(4) - fullSCREEN(2)];




%% monitor/find eyes
talk2tobii('START_TRACKING');
[status,history] = talk2tobii('GET_STATUS')


gazeData=talk2tobii('GET_SAMPLE')

[status,history] = talk2tobii('GET_STATUS')


TobiiInit( hostName, portName, window, res)



talk2tobii('STOP_TRACKING');
WaitSecs(0.5);
talk2tobii('DISCONNECT',hostName);




%GazeContingentDemo_tobii(2, 200, 'konijntjes1024x768.jpg', IP_address, portName)


%find indexes for correspond keys
ESCAPE=KbName('Escape');
fig1 = figure();

flagNotBreak = 0; sample = 1;
xPositions = zeros(15,1); yPositions = zeros(15,1); 
while flagNotBreak == 0
	if( IsKey(ESCAPE) )
		flagNotBreak = 1;
	end
	[x,y,buttons] = GetMouse(1);
	xPositions(mod(sample,15) + 1) = x;
	yPositions(mod(sample,15) + 1) = y;	
	xMean = mean(xPositions);
	yMean = mean(yPositions);
	scatter(xMean, yMean,'xb');
	axis([0 1440 0 900]);
	set(gca,'YDir','rev');
	pause(0.001);
	sample=sample+1;
end









