function [varargout] = cdeal(X)
  %deals out a cell like it should
  varargout = cell(size(X));
  [varargout{:}]=X{:};
end