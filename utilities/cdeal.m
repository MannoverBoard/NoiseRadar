function [varargout] = cdeal(X)
  %deals out a cell like it should
  varargout = cell(1,nargout);
  if nargin==0
    return;
  end
  N = min([nargout,numel(X)]);
  [varargout{1:N}]=X{1:N};
end