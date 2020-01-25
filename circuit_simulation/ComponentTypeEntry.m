classdef ComponentTypeEntry < UniqueName
    properties(GetAccess=public,SetAccess=?CircuitEnvironment)
      short_name = '';
    end
    properties(GetAccess=public,SetAccess=private)
      id_manager = IdManager(1);
      uid2typeid = containers.Map('KeyType',UniqueObjectManager.key_type,'ValueType',IdManager.value_type);
      typeid2uid = containers.Map('KeyType',IdManager.value_type,'ValueType',UniqueObjectManager.key_type);
    end
    methods
      function [typeid] = register(self,component)
        component_uid = component.uid;
        if self.uid2typeid.isKey(component_uid)
          typeid = self.uid2typeid(component_uid);
          return;
        end
        typeid = self.id_manager.next;
        self.uid2typeid(component_uid) = typeid;
        self.uid2typeid(typeid) = component_uid;
      end
      
      function unregister(self,component)
        component_uid = component.uid;
        if ~self.uid2typeid.isKey(component_uid)
          return;
        end
        typeid = self.uid2typeid(component_uid);
        self.id_manager.unregister(typeid);
        self.uid2typeid.remove(component_uid);
        self.uid2typeid.remove(typeid);
      end
      
      function [name] = getDefaultName(self,component)
         typeid = self.register(component);
         name = sprintf('%s%d',self.short_name,typeid);
      end
      
      function [self] = reset(self)
        self.id_manager.reset();
        self.uid2typeid = containers.Map('KeyType',UniqueObjectManager.key_type,'ValueType',IdManager.value_type);
        self.typeid2uid = containers.Map('KeyType',IdManager.value_type,'ValueType',UniqueObjectManager.key_type);
      end
      
    end
end