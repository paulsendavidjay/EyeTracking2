function [ randomized_matrix ] = randomize_rows( input_matrix )
%randomize_rows Randomly arranges input_matrix rows

% transpose input_matrix columns to rows, add row of random numbers and
% transpose back
random_col_matrix = [input_matrix'; rand(1,size(input_matrix, 1))]';
% sort random_col_matrix by last column 
random_col_matrix = sortrows(random_col_matrix, size(random_col_matrix,2));
% return all but the final column
randomized_matrix = random_col_matrix(:,1:(size(random_col_matrix,2) - 1));

end

