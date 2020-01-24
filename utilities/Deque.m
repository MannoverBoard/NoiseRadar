classdef Deque < handle
  % Deque structure - if MinSize is specified then this is optimized for 
  % (right) pushes and pops
  
  properties (GetAccess=private)
    min_size_
    data
    index %right edge
  end
  properties (Dependent)
    min_size
    values
  end
  
  methods
    function self = Deque(data,min_size)
      if nargin<1 || isempty(data)
        data = [];
      end
      if nargin<2 || isempty(min_size)
        min_size = 0;
      end
      self.index = 0;
      self.data = [];
      self.min_size = min_size;%trigger resize etc.
      for d = data
        self.append(d);
      end
    end
    
    function [out] = get.values(self)
      if self.min_size_==0
        out = self.data;
      else
        out = self.data(1:self.index);
      end
    end
    
    function [out] = get.min_size(self)
      out = self.min_size_;
    end
    
    function set.min_size(self,value)
      if value==self.min_size_
        return;
      end
      if value<0
        error('Deque:ValueError','min_size must be greater than or equal to 0');
      end
      self.min_size_ = value;
      if isrow(self.data)
        self.data = self.data.';
      end
      if length(self.data)>self.min_size_
        if self.index<self.min_size_
          %data is bigger, but index is not downsize
          self.data = self.data(1:self.min_size_);
        end
      else
        if iscell(self.data)
          self.data = [self.data;cell(self.min_size_-length(self.data),1)];
        else
          self.data = [self.data;zeros(self.min_size_-length(self.data),1)];
        end
      end
    end
    
    function [item] = pop(self)
      item = self.data(self.index);
      if self.index > self.min_size_
        %if bigger than it needs to be, then reduce size
        self.data = self.data(1:self.index-1);
      end
      self.index = self.index - 1;
    end
    function push(self,item)
      self.index = self.index + 1;
      if iscell(self.data)
        self.data{self.index} = item;
      else
        try
          self.data(self.index) = item;
        catch ME %#ok<NASGU>
          %new data cannot stack with others, make them all cells
          self.data = num2cell(self.data);
          self.data{self.index} = item;
        end
      end
    end
    function append(self,item)
      self.push(item);
    end
    function extend(self,values)
      for v=values(:).'
        self.push(v);
      end
    end
    function [item] = popleft(self)
      item = self.data(1);
      if self.index > self.min_size_
        %if bigger, then can reduce
        self.data = self.data(2:self.index);
      else
        %if smaller or same, just assign data
        self.data(1:end-1) = self.data(2:self.index);
      end
      self.index = self.index - 1;
    end
    function pushleft(self,item)
      smaller = self.index < self.min_size_;
      if iscell(self.data)
        if smaller
          self.data(2:self.index+1) = self.data(1:self.index);
          self.data{1} = item;
        else
          self.data = [{item};self.data];
        end
      elseif (self.index==1 && ischar(item))
        if smaller
          self.data(2:self.index+1) = self.data(1:self.index);
          self.data = num2cell(self.data);
          self.data{1} = item;
        else
          self.data = {item;self.data(:)};
        end
      else
        try
          if smaller
            self.data(2:self.index+1) = self.data(1:self.index);
            self.data(1) = item;
          else
            self.data = [item;self.data(:)];
          end
        catch ME %#ok<NASGU>
          %error appending, default to use cell array
          self.data = {item;self.data{:}};
        end
      end
      self.index = self.index + 1;
    end
    function appendleft(self,item)
      self.pushleft(item);
    end
    function extendleft(self,values)
      for v=values
        self.pushleft(v);
      end
    end

    function clear(self)
      self.index = 0;
      self.data = zeros(self.min_size_,1);
    end

    function out = isMember(self,value)
      try
        if iscell(value)
          out = cellfun(@(c)c==value,self.data(1:self.index));
        else
          out = any(value==self.data(1:self.index));
        end
      catch ME %#ok<NASGU>
        %object comparison failures
        out = false;
      end
    end
    
    function [out] = isempty(self)
      out = (self.index==0);
    end
  end
end