function [subjectID] = process_images(subjectID) %#ok<*INUSD,FNDEF>


directory = ['./logs/', num2str(subjectID), '_pngs/'];
files = dir(fullfile(directory, '*.png'));


for i = 1:length(files)
    
    
    
    img=imread([directory, files(i).name], 'PNG');
    img2 = imresize(img, [256, 320]);
    imwrite(img2, [directory, files(i).name]);
    
end


end