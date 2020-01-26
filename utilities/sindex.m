function [Y] = sindex(X,s,cs)
  if nargin<2 || isempty(cs)
    Y = [X.(s)];
  else
    Y = cs2cell(X.(s));
  end
end