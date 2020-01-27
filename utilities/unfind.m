function [lgc] = unfind(idx,sz)
  lgc = false(sz);
  lgc(idx) = true;
end