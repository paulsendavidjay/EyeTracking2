% CONNECT TO TOBII

% intialize command variables
% hostName = '169.254.225.234'; % psych
hostName = '169.254.7.31'; % fuqua
portName = '4455';

[status,history] = talk2tobii('GET_STATUS');
if status(2) == 1
	display('tobii already connected');
	return
end

%% try to connect to the eyeTracker
talk2tobii('CONNECT',hostName, portName);


%check status of Tobii connection
cond_res = check_status(2, 5, 1, 1); % see if tobii is connected, wait 30 sec, 1 sec intervals, for code 1 in slot 2 (connected)
tmp = find(cond_res==0);
if( ~isempty(tmp) )
	display('tobii unable to connect');
	return
end

