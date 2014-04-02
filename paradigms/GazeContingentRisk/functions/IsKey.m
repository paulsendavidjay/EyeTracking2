function ctrl=IsKey(key)
    global KEYBOARD;
    [keyIsDown,secs,keyCode] = PsychHID('KbCheck', KEYBOARD);
    if ~isnumeric(key)
        kc = KbName(key);
    else
        kc = key;
    end;
    ctrl=keyCode(kc);
return
