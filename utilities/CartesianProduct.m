classdef CartesianProduct < handle
  properties (GetAccess=public,SetAccess=private)
    index;
  end
  properties (Access=private)
    data_;
    all_numeric;
  end
  properties (Dependent)
    data;
    overflows;
    num_total_items;
  end
  
  methods
    function [self] = CartesianProduct(varargin)
      self.data_ = varargin;
      self.all_numeric = all(cellfun(@isnumeric,self.data_));
      radix = cellfun(@(c)length(c(:)),self.data_);
      self.index = MultiIndex(radix);
    end
    
    function [data] = get.data(self)
      idx = self.index.index;
      data = arrayfun(@(i)self.data_{i}(idx(i)),1:length(idx),'Un',0);
      if self.all_numeric
        data = [data{:}].';
      else
        %no point in double nesting cell
        data = data.';
        for i=1:length(data)
          if iscell(data{i})
            data(i) = data{i};
          end
        end
      end
    end
    function [overflows] = get.overflows(self)
      overflows = self.index.overflows;
    end
    function [num_total_items] = get.num_total_items(self)
      num_total_items = prod(self.index.radix);
    end
    
    function [data] = prev(self)
      self.index.prev();
      data = self.data;
    end
    
    function [data] = next(self)
      self.index.next();
      data = self.data;
    end
    
    function [all_data] = getAllData(self)
      current_index = self.index.copy();
      
      N = prod(self.index.radix);
      self.index.startAtBegin();
      data0 = self.data;
      all_data = repmat(data0,[1,N]);
      for n=1:N
        all_data(:,n) = self.data;
        self.next();
      end
      %reset back to where index was
      self.index = current_index;
    end
  end
end
      
      