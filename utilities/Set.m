classdef Set < handle
  properties(Access=public)
    map;
  end
  properties(GetAccess=public,SetAccess=private)
    KeyType = 'int64';
  end
  methods
    function [self] = Set(obj,varargin)
      self.map = [];
      if nargin==0
        obj = [];
      end
      if nargin>=1 && ~isempty(obj) && isa(obj,'Set')
        self.map = obj.map;
        self.update(obj.values());
      end
      optional_args = {'KeyType'};
      for i=1:length(optional_args)
        arg = optional_args{i};
        matches = strcmpi(varargin,arg);
        num_matches = sum(matches);
        if num_matches>1
          error('Got multiple instances of argument %s',arg);
        elseif num_matches==1
          idx = find(matches,1,'first');
          if idx>=length(varargin)
            error('Given argument %s, but not supplied value.',arg);
          end
          value = varargin{idx+1};
          self.(arg) = value;
        end
      end
      if isempty(self.map)
        self.map = containers.Map('KeyType',self.KeyType,'ValueType','logical');
        if ~isempty(obj)
          self.update(obj);
        end
      end
    end
    
    function clear(self)
      self.map = containers.Map('KeyType',self.KeyType,'ValueType','logical');
    end
    
    function add(self,value)
      self.map(value)=1;
    end
    function remove(self,value)
      %Remove an element from a set; it must be a member.
      % If the element is not a member, raise a KeyError.
      if self.map.isKey(value)
        self.map.remove(value);
      else
        error('Set:KeyError','%s not in Set',num2str(value));
      end
    end
    function discard(self,value)
      %Remove an element from a set if it is a member.
      % If the element is not a member, do nothing.
      self.map.remove(value);
    end
    function [out] = pop(self)
      %Pops a value in the set
      out = self.map.keys();
      out = out{1};
      self.remove(out);      
    end
    function update(self,values)
      %Update a set with the union of itself and others.
      if isa(values,'Set')
        values = values.values();
      end
      if iscell(values)
        for v=values
          self.map(v{1})=1;
        end
      else
        for v=values
          self.map(v)=1;
        end
      end
    end
    function [out] = values(self)
      if strcmpi(self.KeyType,'char')
        out = self.map.keys();
      else
        out = self.map.keys();
        out = [out{:}];
      end
    end
    function [out] = numel(self)
      out = self.map.Count;
    end
    function [out] = length(self)
      out = self.map.Count;
    end
    function [out] = isMember(self,value)
      out = self.map.isKey(value);
    end
    function [out] = difference(self,rhs)
      rhs = Set.setify(rhs);
      if ~all(self.KeyType==rhs.KeyType)
        error('Set:KeyType','KeyTypes must match %s~=%s',self.KeyType,rhs.KeyType);
      end
      out = Set();
      for vc=self.map.keys()
        value = vc{1};
        if ~rhs.isMember(value)
          out.add(value);
        end
      end
    end
    function [out] = minus(lhs,rhs)
      [lhs,rhs] = Set.fixBinaryOperatorArgs(lhs,rhs);
      out = lhs.difference(rhs);
    end
    
    function [out] = intersection(self,rhs)
      %Return the intersection of two or more sets as a new set.
      % (i.e. elements that are common to all of the sets
      rhs = Set.setify(rhs);
      if ~all(self.KeyType==rhs.KeyType)
        error('Set:KeyType','KeyTypes must match %s~=%s',self.KeyType,rhs.KeyType);
      end
      out = Set();
      for vc=self.map.keys()
        value = vc{1};
        if rhs.isMember(value)
          out.add(value);
        end
      end
    end
    function [out] = and(lhs,rhs)
      %Return the intersection of two or more sets as a new set.
      % (i.e. elements that are common to all of the sets
      [lhs,rhs] = Set.fixBinaryOperatorArgs(lhs,rhs);
      out = lhs.intersection(rhs);
    end
    function [self] = intersectionUpdate(self,rhs)
      %Update a set with the intersection of itself and another.
      rhs = Set.setify(rhs);
      out = self.intersection(rhs);
      self.map = out.map;
    end
    
    function [out] = union(self,rhs)
      %Return the union of sets as a new set.
      % (i.e. all elements that are in either set.
      rhs = Set.setify(rhs);
      if ~all(self.KeyType==rhs.KeyType)
        error('Set:KeyType','KeyTypes must match %s~=%s',self.KeyType,rhs.KeyType);
      end
      out = Set(self);
      for vc=rhs.map.keys()
        value = vc{1};
        out.add(value);
      end
    end
    function [out] = or(lhs,rhs)
      out = lhs.union(rhs);
    end
    
    function [out] = symmetric_difference(self,rhs)
      %Return the symmetric difference of two sets as a new set.
      % (i.e. all elements that are in exactly one of the sets.
      rhs = Set.setify(rhs);
      out = (self-rhs)|(rhs-self);
    end
    function [self] = symmetric_difference_update(self,rhs)
      %Update a set with the symmetric difference of itself and another.
      rhs = Set.setify(rhs);
      tmp = self.symmetric_difference(rhs);
      self.map = tmp.map;
    end
    
    function [out] = eq(lhs,rhs)
      [lhs,rhs] = Set.fixBinaryOperatorArgs(lhs,rhs);
      if lhs.map.Count~=rhs.map.Count
        out = false;
        return;
      end
      tmp = lhs&rhs;
      out = lhs.map.Count==tmp.map.Count;
    end
    function [out] = isdisjoint(self,rhs)
      %Return True if two sets have a null intersection.
      tmp = self&rhs;
      out = (tmp.map.Count==0);
    end
    function [out] = isempty(self)
      out = self.map.Count==0;
    end
    
    function [out] = issubset(self,rhs)
      %Report whether another set contains this set.
      rhs = Set.setify(rhs);
      out = rhs.issuperset(self);
    end
    function [out] = issuperset(self,rhs)
      %Report whether this set contains another set.
      out = true;
      rhs = Set.setify(rhs);
      for vc = rhs.map.keys()
        if ~self.map.isKey(vc{1})
          out = false;
          break;
        end
      end
    end
  end
  
  methods (Static)
    function [out] = setify(input)
      if isa(input,'Set')
        out = input;
        return;
      end
      out = Set(input);
    end
    
    function [lhs,rhs] = fixBinaryOperatorArgs(lhs,rhs)
      lhs = Set.setify(lhs);
      rhs = Set.setify(rhs);
    end
  end
end
      
%containers.Map	Object that maps values to unique keys
%isKey	Determine if Map object contains key
%keys	Return keys of Map object
%remove	Delete key-value pairs from Map object
%values	Return values of Map object