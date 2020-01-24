function [out] = cflat(X)
  %flattens a cell
  out = [X{:}];
end