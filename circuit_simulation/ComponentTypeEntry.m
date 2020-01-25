classdef ComponentTypeEntry < UniqueName
    properties
        short_name = [];
        components = Set();
        id_manager = IdManager(1);
    end
end