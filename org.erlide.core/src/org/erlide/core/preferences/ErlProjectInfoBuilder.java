package org.erlide.core.preferences;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.osgi.service.prefs.BackingStoreException;

public class ErlProjectInfoBuilder {

    public ErlProjectInfoBuilder() {
    }

    public ErlProjectInfo loadFromPreferences(
            final IEclipsePreferences root) throws BackingStoreException {
        // TODO implement!
        return null;
    }

    public void storeToPreferences(ErlProjectInfo info,
            final IEclipsePreferences root) throws BackingStoreException {
        // TODO implement!
    }

}
