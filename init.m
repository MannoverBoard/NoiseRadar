function init()
    paths_to_add = {'./utilities'};
    for pth = paths_to_add
        pth=pth{1};
        if ~exist(pth,'dir')
            warning('Could not find directory to add %s. Code may not function properly without it.',pth);
            continue
        end
        addpath(pth);
    end
end
