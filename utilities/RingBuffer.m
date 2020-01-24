classdef RingBuffer < handle
  % RingBuffer data structure - writes data in a circular queue overwriting
  % when full
  
  properties (GetAccess=private)
    head;
    tail;
    data;
  end
  
  methods
    function self = RingBuffer(num_elements)
      self.head = 1;
      self.tail = 1;
      self.data = zeros(num_elements,1);
    end
    
    function [item] = pop(self)
      self.head = self.head - 1;
      if self.head<1
        self.head = length(self.data);
      end
      if iscell(self.data)
        item = self.data{self.head};
      else
        item = self.data(self.head);
      end
    end
    function push(self,item)
      if iscell(self.data)
        self.data{self.head} = item;
      else
        try
          self.data(self.head) = item;
        catch ME %#ok<NASGU>
          %new data cannot stack with others, make them all cells
          self.data = num2cell(self.data);
          self.data{self.head} = item;
        end
      end
      self.head = self.head + 1;
      if self.head > length(self.data)
        self.head = 1;
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
      if iscell(self.data)
        item = self.data{self.tail};
      else
        item = self.data(self.tail);
      end
      self.tail = self.tail + 1;
      if self.tail > length(self.data)
        self.tail = 1;
      end
    end
    function pushleft(self,item)
      self.tail = self.tail - 1;
      if self.tail < 1
        self.tail = length(self.data);
      end
      if iscell(self.data)
        self.data{self.tail} = item;
      else
        try
          self.data(self.tail) = item;
        catch ME %#ok<NASGU>
          %new data cannot stack with others, make them all cells
          self.data = num2cell(self.data);
          self.data{self.tail} = item;
        end
      end
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
      self.head = 1;
      self.tail = 1;
    end
    
    function [item] = indexFromHead(self,index)
      index = mod(index+self.head-2,length(self.data))+1;
      if iscell(self.data)
        item = self.data{index};
      else
        item = self.data(index);
      end
    end
    function [item] = first(self)
      if iscell(self.data)
        item = self.data{self.head};
      else
        item = self.data(self.head);
      end
    end
    function [item] = last(self)
      index = self.head-1;
      if index<1
        index = length(self.data);
      end
      if iscell(self.data)
        item = self.data{index};
      else
        item = self.data(index);
      end
    end
    function [out] = length(self)
      out = length(self.data);
    end
    function [self] = resize(self,new_size)
      if new_size < length(self.data)
        self.data = self.data(1:new_size);
        self.head = min([self.head,length(self.data)]);
        self.tail = min([self.tail,length(self.data)]);
      else
        if iscell(self.data)
          self.data = [self.data;cell(new_size-length(self.data),1)];
        else
          self.data = [self.data;zeros(new_size-length(self.data),1)];
        end
      end
    end
  end
end
