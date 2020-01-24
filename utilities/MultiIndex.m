classdef MultiIndex < handle
  properties(Dependent)
    radix
    index;
    endian;
  end
  properties(GetAccess=public,SetAccess=private)
    overflows = 0;
  end
  properties(Access=private)
    radix_
    index_
    isbig_ = true;%big endian
  end
  
  methods
    function [self] = MultiIndex(varargin)
      if nargin==0
        error('Error constructing MultiIndex. Expected at least one argument.');
      end
      if nargin==1 && isa(varargin{1},'MultiIndex')
        rhs = varargin{1};
        self.overflows = rhs.overflows;
        self.radix_ = rhs.radix_;
        self.index_ = rhs.index_;
        self.isbig_ = rhs.isbig_;
        return;
      end
      self.radix_ = varargin{1};
      if self.isbig_
        self.radix_ = fliplr(self.radix_);
      end
      if nargin==2
        self.endian = varargin{2};
      end
      self.index_ = ones(size(self.radix));
      self.reset();
    end
    
    function [self] = next(varargin)
      self = varargin{1};
      if nargin==2
        N = varargin{2};
      else
        N = 1;
      end
      for n=1:N
        %increment index
        for i=1:length(self.index_)
          self.index_(i) = self.index_(i) + 1;
          if(mod(self.index_(i)-1,self.radix_(i))==0)
            %carry over to next
            self.index_(i) = 1;
          else
            break; %done!
          end
        end
        if all(self.index_==1)
          self.overflows = self.overflows+1;
        end
      end
    end
    
    function [self] = prev(varargin)
      %decrement index
      self = varargin{1};
      if nargin==2
        N = varargin{2};
      else
        N = 1;
      end
      for n=1:N
        for i=1:length(self.index_)
          self.index_(i) = self.index_(i) - 1;
          if(self.index_(i)<=0)
            %carry over to next
            self.index_(i) = self.radix_(i);
          else
            break; %done!
          end
        end
        if all(self.index_==self.radix_)
          self.overflows = self.overflows-1;
        end
      end
    end
    
    function [self] = reset(self)
      self.index_ = ones(size(self.index));
      self.overflows = 0;
    end
    
    function [self] = startAtBegin(self)
      self.index_ = ones(size(self.radix_));
      self.overflows = 0;
    end
    
    function [self] = startAtEnd(self)
      self.index_ = self.radix_;
      self.overflows = 0;
    end
    
    function [index] = get.index(self)
      if self.isbig_
        index = fliplr(self.index_);
      else
        index = self.index_;
      end
    end
    
    function [radix] = get.radix(self)
      if self.isbig_
        radix = fliplr(self.radix_);
      else
        radix = self.radix_;
      end
    end
    
    function [endian] = get.endian(self)
      if self.isbig_
        endian = 'big';
      else
        endian = 'little';
      end
    end
    
    function set.endian(self,value)
      if strcmpi(value,'big')
        isbig = true;
      else
        isbig = false;
      end
      if self.isbig_~=isbig
        self.isbig_ = isbig;
        self.radix_ = fliplr(self.radix_);
        self.index_ = fliplr(self.index_);
      end
    end
    
    function [S] = getSubstruct(self)
      S = struct('type','()','subs',0);
      S.subs = num2cell(self.index);
    end
    
    function [out] = copy(self)
      out = MultiIndex(self);
    end
  end
end
  