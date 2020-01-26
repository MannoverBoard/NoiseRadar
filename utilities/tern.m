function [out] = tern(sw,tr,fs)
  if sw
    if isa(tr,'function_handle')
      out = tr();
    else
      out = tr;
    end
  else
    if isa(fs,'function_handle')
      out = fs();
    else
      out = fs();
    end
  end
end
    