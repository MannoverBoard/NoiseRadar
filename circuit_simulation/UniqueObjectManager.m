classdef UniqueObjectManager < handle
  properties (Constant,Hidden)
    uid_type = AtomicCounter.value_type;
  end
  properties(GetAccess=private)
    uid_manager;
  end
  properties(GetAccess=public,SetAccess=private)
    registry;
  end
  methods
    function self = UniqueObjectManager(varargin)
      self.uid_manager = AtomicCounter(1);
      self.registry = containers.Map('KeyType',self.uid_type,'ValueType','any');
    end
    
    function [uid] = register(self,object)
      %assumes object is UniqueId child
      uid = object.uid;
      if ~isempty(uid) && self.registry.isKey(uid)
        return;
      end
      uid = self.uid_manager.next;
      object.uid = uid;
      self.registry(uid) = object;
    end
    function unregister(self,object)
      if self.registry.isKey(object.uid)
        self.registry.remove(object.uid);
      end
      object.uid = [];
    end
  end
end