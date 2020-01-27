function [Y] = removeEmpties(X)
  if iscell(X)
    lgc = cellfun(@isempty,X);
  else
    lgc = arrayfun(@isempty,X);
  end
  Y = X(~lgc);
end