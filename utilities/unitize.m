function [Y] = unitize(X,varargin)
  nrm = rootSumSq(X,varargin{:});
  lgc = (nrm==0);
  Y = X./(nrm+lgc);
  if any(lgc,'all')
    Y(:,lgc)=[1;0;0].*ones(size(Y(:,lgc)));
  end
end