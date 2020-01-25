classdef UniqueId < handle
  properties(GetAccess=public,SetAccess=?UniqueObjectManager,Hidden)
    uid = [];
    h_unique_object_manager = [];
  end
  methods
    function [self] = UniqueId(h_unique_object_manager,varargin)
      self.h_unique_object_manager = h_unique_object_manager;
      if ~isempty(self.h_unique_object_manager)
        self.h_unique_object_manager.register(self);
      end
    end
    function setObjectManager(self,h_unique_object_manager)
      if self.h_unique_object_manager==h_unique_object_manager
        %ignore self sets
        return;
      end
      if ~isempty(self.h_unique_object_manager)
        self.h_unique_object_manager.unregister(self);
      end
      self.h_unique_object_manager = h_unique_object_manager;
      if isempty(self.h_unique_object_manager)
        return;
      end
      self.h_unique_object_manager.register(self);
    end
    function unregister(self)
      self.setObjectManager([]);
    end
  end
end