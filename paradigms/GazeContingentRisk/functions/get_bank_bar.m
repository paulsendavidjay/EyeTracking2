function [ bank_bar ] = get_bank_bar( bank, bank_max, bank_bar_location, direction)
%get_bank_bar returns the proportion of the bank_bar_location rectangle
%filled by bank/bank_bar ratio in the x or y direction
%   Detailed explanation goes here

if bank > bank_max
    ratio = 1;
else
    ratio = bank/bank_max;
end

switch direction
    
    case 'x'
        
        bank_bar = floor([bank_bar_location(1), ...
            bank_bar_location(2), ...
            bank_bar_location(1) + ratio*(bank_bar_location(3) - bank_bar_location(1)), ...
            bank_bar_location(4)]);
        
    case 'y'
        
        bank_bar = floor([bank_bar_location(1), ...
            bank_bar_location(2), ...
            bank_bar_location(3), ...
            bank_bar_location(2) + ratio*(bank_bar_location(4) - bank_bar_location(2))]);
        
end

