package org.erlide.core.erlang;

import java.util.Set;

public interface IErlModelMap {

    void putEdited(String path);

    void removeEdited(String path);

    void put(String path, IErlModuleInternal moduleInternal);

    IErlModuleInternal get(IErlModule module);

    void putModule(IErlModule module);

    void removeModule(IErlModule module);

    Set<IErlModule> getModulesByName(String moduleName);

}