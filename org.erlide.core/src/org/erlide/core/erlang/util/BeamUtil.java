package org.erlide.core.erlang.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Collection;
import java.util.Enumeration;
import java.util.List;

import org.eclipse.core.internal.runtime.Activator;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IContributor;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.RegistryFactory;
import org.eclipse.osgi.framework.internal.core.BundleURLConnection;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlProject;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.ErlideBackend;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.google.common.collect.Lists;

@SuppressWarnings("restriction")
public class BeamUtil {

    public static OtpErlangBinary getBeamBinary(final String moduleName,
            final URL beamPath) {
        try {
            final FileInputStream s = (FileInputStream) beamPath.openStream();
            final int sz = (int) s.getChannel().size();
            final byte[] buf = new byte[sz];
            try {
                s.read(buf);
                return new OtpErlangBinary(buf);
            } finally {
                s.close();
            }
        } catch (final IOException e) {
            ErlLogger.warn(e);
            return null;
        }
    }

    public static OtpErlangBinary getBeamBinary(final String moduleName,
            final IPath beamPath) {
        try {
            final FileInputStream s = new FileInputStream(
                    beamPath.toPortableString());
            final int sz = (int) s.getChannel().size();
            final byte[] buf = new byte[sz];
            try {
                s.read(buf);
                return new OtpErlangBinary(buf);
            } finally {
                s.close();
            }
        } catch (final IOException e) {
            ErlLogger.warn(e);
            return null;
        }
    }

    @SuppressWarnings("restriction")
    public static Collection<String> getPaths(final String name, final Bundle b) {
        final List<String> result = Lists.newArrayList();
        final String entryName = name.replace(" ", "%20");
        URL entry = b.getEntry(entryName);
        if (entry != null) {
            final String aPath = getPathFromUrl(entry);
            if (aPath != null) {
                result.add(aPath);
            }
        }

        final Activator activator = Activator.getDefault();
        if (activator != null) {
            final Bundle[] fragments = activator.getFragments(b);
            if (fragments != null) {
                for (int i = 0; i < fragments.length; i++) {
                    entry = fragments[i].getEntry(entryName);
                    if (entry != null) {
                        final String aPath = getPathFromUrl(entry);
                        result.add(aPath);
                    }
                }
            }
        }
        return result;
    }

    @SuppressWarnings("restriction")
    private static String getPathFromUrl(final URL entry) {
        URLConnection connection;
        try {
            connection = entry.openConnection();
            if (connection instanceof BundleURLConnection) {
                final URL fileURL = ((BundleURLConnection) connection)
                        .getFileURL();
                final URI uri = new URI(fileURL.toString().replace(" ", "%20"));
                final String path = new File(uri).getAbsolutePath();
                return path;
            }
        } catch (final IOException e) {
            ErlLogger.warn(e.getMessage());
        } catch (final URISyntaxException e) {
            ErlLogger.warn(e.getMessage());
        }
        return null;
    }

    private BeamUtil() {
    }

    /**
     * @noreference This method is not intended to be referenced by clients.
     */
    public static void unpackBeamFiles(final Bundle b, final String location) {
        if (location == null) {
            ErlLogger.warn("Could not find 'ebin' in bundle %s.",
                    b.getSymbolicName());
            return;
        }
        final File ebinDir = new File(location + "/ebin");
        ebinDir.mkdirs();
        for (final String fn : ebinDir.list()) {
            if (fn.charAt(0) == '.') {
                continue;
            }
            final File f = new File(fn);
            f.delete();
        }

        ErlLogger.debug("unpacking plugin " + b.getSymbolicName() + " in "
                + location);

        // TODO Do we have to also check any fragments?
        // see FindSupport.findInFragments

        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        final IConfigurationElement[] els = reg.getConfigurationElementsFor(
                ErlangPlugin.PLUGIN_ID, "codepath");
        for (final IConfigurationElement el : els) {
            final IContributor c = el.getContributor();
            if (c.getName().equals(b.getSymbolicName())) {
                final String dir_path = el.getAttribute("path");
                final Enumeration<?> e = b.getEntryPaths(dir_path);
                if (e == null) {
                    ErlLogger.debug("* !!! error loading plugin "
                            + b.getSymbolicName());
                    return;
                }
                while (e.hasMoreElements()) {
                    final String s = (String) e.nextElement();
                    final Path path = new Path(s);
                    if (path.getFileExtension() != null
                            && "beam".compareTo(path.getFileExtension()) == 0) {
                        final String m = path.removeFileExtension()
                                .lastSegment();
                        final URL url = b.getEntry(s);
                        ErlLogger.debug(" unpack: " + m);
                        final File beam = new File(ebinDir, m + ".erl");
                        try {
                            beam.createNewFile();
                            final FileOutputStream fs = new FileOutputStream(
                                    beam);
                            try {
                                final OtpErlangBinary bin = getBeamBinary(m,
                                        url);
                                fs.write(bin.binaryValue());
                            } finally {
                                fs.close();
                            }
                        } catch (final IOException e1) {
                            ErlLogger.warn(e1);
                        }
                    }
                }
            }
        }

    }

    public static void loadModuleViaInput(final ErlideBackend b,
            final IProject project, final String module)
            throws ErlModelException, IOException {
        final IErlProject p = ErlangCore.getModel().findProject(project);
        final IPath outputLocation = project.getFolder(p.getOutputLocation())
                .getFile(module + ".beam").getLocation();
        final OtpErlangBinary bin = getBeamBinary(module, outputLocation);
        if (bin != null) {
            final String fmt = "code:load_binary(%s, %s, %s).\n";
            final StringBuffer strBin = new StringBuffer();
            strBin.append("<<");
            for (final byte c : bin.binaryValue()) {
                strBin.append(c).append(',');
            }
            strBin.deleteCharAt(strBin.length() - 1);
            strBin.append(">>");
            final String cmd = String.format(fmt, module, module,
                    strBin.toString());
            b.input(cmd);
        }
    }

}