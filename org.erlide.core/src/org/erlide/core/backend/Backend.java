/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.backend;

import java.io.File;
import java.io.IOException;
import java.net.Socket;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchesListener2;
import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.IStreamMonitor;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.erlide.core.ErlangCore;
import org.erlide.core.backend.console.BackendShell;
import org.erlide.core.backend.console.BackendShellManager;
import org.erlide.core.backend.console.IoRequest.IoRequestKind;
import org.erlide.core.backend.events.EventDaemon;
import org.erlide.core.backend.events.LogEventHandler;
import org.erlide.core.backend.internal.BackendUtil;
import org.erlide.core.backend.internal.BackendUtils;
import org.erlide.core.backend.internal.CodeBundleImpl;
import org.erlide.core.backend.internal.CodeManager;
import org.erlide.core.backend.internal.RpcResultImpl;
import org.erlide.core.backend.manager.BackendManager;
import org.erlide.core.backend.rpc.RpcException;
import org.erlide.core.backend.rpc.RpcFuture;
import org.erlide.core.backend.rpc.RpcHelper;
import org.erlide.core.backend.rpc.RpcResult;
import org.erlide.core.backend.runtimeinfo.RuntimeInfo;
import org.erlide.core.backend.runtimeinfo.RuntimeVersion;
import org.erlide.core.common.BeamUtil;
import org.erlide.core.common.IDisposable;
import org.erlide.core.model.erlang.ErlModelException;
import org.erlide.core.model.erlang.IErlProject;
import org.erlide.core.model.erlang.util.CoreUtil;
import org.erlide.core.model.erlang.util.ErlideUtil;
import org.erlide.jinterface.ErlLogger;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.OtpNodeStatus;
import com.ericsson.otp.erlang.SignatureException;

public class Backend extends OtpNodeStatus implements RpcCallSite, IDisposable,
        IStreamListener, ILaunchesListener2 {

    public static final String[] SUPPORTED_VERSIONS = new String[] { "",
            "R12B-1", "R12B-2", "R12B-3", "R12B-4", "R12B-5", "R13B", "R14A" };
    public static final String[] SUPPORTED_MAIN_VERSIONS = new String[] { "",
            "R12B", "R13B", "R14A" };
    public static final String DEFAULT_VERSION = "R13B";

    private static final String COULD_NOT_CONNECT_TO_BACKEND = "Could not connect to backend! Please check runtime settings.";
    private static final int EPMD_PORT = 4369;
    private static final int RETRY_DELAY = Integer.parseInt(System.getProperty(
            "erlide.connect.delay", "300"));
    public static int DEFAULT_TIMEOUT;
    {
        final String t = System.getProperty("erlide.rpc.timeout", "9000");
        if ("infinity".equals(t)) {
            DEFAULT_TIMEOUT = RpcHelper.INFINITY;
        } else {
            DEFAULT_TIMEOUT = Integer.parseInt(t);
        }
    }

    private boolean available = false;
    private String currentVersion;
    private OtpMbox eventBox;
    private int exitStatus = -1;
    private boolean fDebug;
    private final RuntimeInfo fInfo;
    private OtpNode fNode;
    private final String fPeer;
    private int restarted = 0;
    private boolean stopped = false;
    private EventDaemon eventDaemon;
    private boolean monitor = false;
    private boolean watch = true;
    private BackendShellManager shellManager;
    private boolean logCalls = false;
    private final CodeManager codeManager;
    private IStreamsProxy proxy;
    private ILaunch launch;
    private boolean managed = false;

    protected Backend(final RuntimeInfo info, final String peer)
            throws BackendException {
        if (info == null) {
            throw new BackendException(
                    "Can't create backend without runtime information");
        }
        fInfo = info;
        fPeer = peer;
        codeManager = new CodeManager(this);
    }

    public Backend(final RuntimeInfo info) throws BackendException {
        this(info, BackendUtil.buildLocalNodeName(info.getNodeName(), true));
    }

    /**
     * typed RPC
     * 
     */
    public RpcResult call_noexception(final String m, final String f,
            final String signature, final Object... a) {
        return call_noexception(DEFAULT_TIMEOUT, m, f, signature, a);
    }

    /**
     * typed RPC with timeout
     * 
     * @throws ConversionException
     */
    public RpcResult call_noexception(final int timeout, final String m,
            final String f, final String signature, final Object... args) {
        try {
            final OtpErlangObject result = makeCall(timeout, m, f, signature,
                    args);
            return new RpcResultImpl(result);
        } catch (final RpcException e) {
            return RpcResultImpl.error(e.getMessage());
        } catch (final SignatureException e) {
            return RpcResultImpl.error(e.getMessage());
        }
    }

    public RpcFuture async_call(final String m, final String f,
            final String signature, final Object... args)
            throws BackendException {
        try {
            return makeAsyncCall(m, f, signature, args);
        } catch (final RpcException e) {
            throw new BackendException(e);
        } catch (final SignatureException e) {
            throw new BackendException(e);
        }
    }

    public void async_call_cb(final RpcCallback cb, final String m,
            final String f, final String signature, final Object... args)
            throws BackendException {
        try {
            makeAsyncCbCall(cb, m, f, signature, args);
        } catch (final RpcException e) {
            throw new BackendException(e);
        } catch (final SignatureException e) {
            throw new BackendException(e);
        }
    }

    public void cast(final String m, final String f, final String signature,
            final Object... args) throws BackendException {
        try {
            makeCast(m, f, signature, args);
        } catch (final RpcException e) {
            throw new BackendException(e);
        } catch (final SignatureException e) {
            throw new BackendException(e);
        }
    }

    public OtpErlangObject call(final String m, final String f,
            final String signature, final Object... a) throws BackendException {
        return call(DEFAULT_TIMEOUT, m, f, signature, a);
    }

    /**
     * typed RPC with timeout, throws Exception
     * 
     * @throws ConversionException
     */
    public OtpErlangObject call(final int timeout, final String m,
            final String f, final String signature, final Object... a)
            throws BackendException {
        return call(timeout, new OtpErlangAtom("user"), m, f, signature, a);
    }

    public OtpErlangObject call(final int timeout,
            final OtpErlangObject gleader, final String m, final String f,
            final String signature, final Object... a) throws BackendException {
        try {
            return makeCall(timeout, gleader, m, f, signature, a);
        } catch (final RpcException e) {
            throw new BackendException(e);
        } catch (final SignatureException e) {
            throw new BackendException(e);
        }
    }

    public void send(final OtpErlangPid pid, final Object msg) {
        if (!available) {
            return;
        }
        try {
            RpcHelper.send(getNode(), pid, msg);
        } catch (final SignatureException e) {
            // shouldn't happen
            ErlLogger.warn(e);
        }
    }

    public void send(final String name, final Object msg) {
        if (!available) {
            return;
        }
        try {
            RpcHelper.send(getNode(), getFullNodeName(), name, msg);
        } catch (final SignatureException e) {
            // shouldn't happen
            ErlLogger.warn(e);
        }
    }

    public OtpErlangObject receiveEvent(final long timeout)
            throws OtpErlangExit, OtpErlangDecodeException {
        if (eventBox == null) {
            return null;
        }
        return eventBox.receive(timeout);
    }

    private synchronized void checkAvailability() throws RpcException {
        if (!available) {
            if (exitStatus >= 0 && restarted < 3) {
                restart();
            } else {
                final String msg = "could not restart backend %s (exitstatus=%d restarted=%d)";
                throw new RpcException(String.format(msg, getInfo(),
                        exitStatus, restarted));
            }
        }
    }

    public void connect() {
        final String label = getName();
        ErlLogger.debug(label + ": waiting connection to peer...");
        try {
            wait_for_epmd();

            eventBox = getNode().createMbox("rex");
            int tries = 20;
            while (!available && tries > 0) {
                ErlLogger.debug("# ping...");
                available = getNode().ping(getFullNodeName(),
                        RETRY_DELAY + (20 - tries) * RETRY_DELAY / 5);
                tries--;
            }
            if (available) {
                available = waitForCodeServer();
            }

            if (available) {
                ErlLogger.debug("connected!");
            } else {
                ErlLogger.error(COULD_NOT_CONNECT_TO_BACKEND);
            }

        } catch (final BackendException e) {
            ErlLogger.error(e);
            available = false;
            ErlLogger.error(COULD_NOT_CONNECT_TO_BACKEND);
        }
    }

    public void dispose() {
        try {
            if (launch != null) {
                launch.terminate();
            }
        } catch (final DebugException e) {
            e.printStackTrace();
        }
        dispose(false);
    }

    public void dispose(final boolean restart) {
        ErlLogger.debug("disposing backend " + getName());
        if (shellManager != null) {
            shellManager.dispose();
        }

        if (getNode() != null) {
            getNode().close();
        }
        if (eventDaemon != null) {
            eventDaemon.stop();
        }
        if (restart) {
            return;
        }
    }

    public String getCurrentVersion() {
        if (currentVersion == null) {
            try {
                currentVersion = getScriptId();
            } catch (final Exception e) {
            }
        }
        return currentVersion;
    }

    private OtpMbox getEventBox() {
        return eventBox;
    }

    public OtpErlangPid getEventPid() {
        final OtpMbox box = getEventBox();
        if (box == null) {
            return null;
        }
        return box.self();
    }

    public RuntimeInfo getInfo() {
        return fInfo;
    }

    public String getJavaNodeName() {
        return getNode().node();
    }

    public String getName() {
        if (fInfo == null) {
            return "<not_connected>";
        }
        return fInfo.getNodeName();
    }

    public String getFullNodeName() {
        synchronized (fPeer) {
            return fPeer;
        }
    }

    private synchronized OtpNode getNode() {
        return fNode;
    }

    private String getScriptId() throws BackendException {
        OtpErlangObject r;
        r = call("init", "script_id", "");
        if (r instanceof OtpErlangTuple) {
            final OtpErlangObject rr = ((OtpErlangTuple) r).elementAt(1);
            if (rr instanceof OtpErlangString) {
                return ((OtpErlangString) rr).stringValue();
            }
        }
        return "";
    }

    private boolean init(final OtpErlangPid jRex, final boolean aMonitor,
            final boolean aWatch) {
        try {
            // reload(backend);
            call("erlide_kernel_common", "init", "poo", jRex, aMonitor, aWatch);
            // TODO should use extension point!
            call("erlide_kernel_builder", "init", "");
            call("erlide_kernel_ide", "init", "");
            return true;
        } catch (final Exception e) {
            ErlLogger.error(e);
            return false;
        }
    }

    public synchronized void initErlang(final boolean doMonitor,
            final boolean aWatch) {
        final boolean inited = init(getEventPid(), doMonitor, aWatch);
        if (!inited) {
            setAvailable(false);
        }
        monitor = doMonitor;
        watch = aWatch;
        initEventDaemon();
        ErlangCore.getBackendManager().addBackendListener(getEventDaemon());
    }

    private void initEventDaemon() {
        eventDaemon = new EventDaemon(this);
        eventDaemon.start();
        eventDaemon.addHandler(new LogEventHandler());
    }

    public void initializeRuntime() throws IOException {
        dispose(true);
        shellManager = new BackendShellManager(this);

        final String cookie = getInfo().getCookie();
        if (cookie == null) {
            fNode = new OtpNode(BackendUtil.createJavaNodeName());
        } else {
            fNode = new OtpNode(BackendUtil.createJavaNodeName(), cookie);
        }
        final String nodeCookie = fNode.cookie();
        final int len = nodeCookie.length();
        final String trimmed = len > 7 ? nodeCookie.substring(0, 7)
                : nodeCookie;
        ErlLogger.debug("using cookie '%s...'%d (info: '%s')", trimmed, len,
                cookie);
    }

    public boolean isDebug() {
        return fDebug;
    }

    public boolean isStopped() {
        return stopped;
    }

    private RpcFuture makeAsyncCall(final OtpErlangObject gleader,
            final String module, final String fun, final String signature,
            final Object... args0) throws RpcException, SignatureException {
        checkAvailability();
        return RpcHelper.sendRpcCall(getNode(), getFullNodeName(), logCalls,
                gleader, module, fun, signature, args0);
    }

    protected RpcFuture makeAsyncCall(final String module, final String fun,
            final String signature, final Object... args0) throws RpcException,
            SignatureException {
        return makeAsyncCall(new OtpErlangAtom("user"), module, fun, signature,
                args0);
    }

    protected void makeAsyncCbCall(final RpcCallback cb, final int timeout,
            final String module, final String fun, final String signature,
            final Object... args) throws RpcException, SignatureException {
        makeAsyncCbCall(cb, timeout, new OtpErlangAtom("user"), module, fun,
                signature, args);
    }

    protected void makeAsyncCbCall(final RpcCallback cb, final String module,
            final String fun, final String signature, final Object... args)
            throws RpcException, SignatureException {
        makeAsyncCbCall(cb, DEFAULT_TIMEOUT, new OtpErlangAtom("user"), module,
                fun, signature, args);
    }

    private void makeAsyncCbCall(final RpcCallback cb, final int timeout,
            final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args)
            throws RpcException, SignatureException {
        checkAvailability();

        final RpcFuture future = RpcHelper.sendRpcCall(fNode, fPeer, logCalls,
                gleader, module, fun, signature, args);
        final Runnable target = new Runnable() {
            public void run() {
                OtpErlangObject result;
                try {
                    result = future.get(timeout);
                    cb.run(result);
                } catch (final RpcException e) {
                    // TODO do we want to treat a timeout differently?
                    ErlLogger.error("Could not execute RPC " + module + ":"
                            + fun + " : " + e.getMessage());
                }
            }
        };
        // We can't use jobs here, it's an Eclipse dependency
        final Thread thread = new Thread(target);
        thread.setDaemon(true);
        thread.setName("async " + module + ":" + fun);
        thread.start();
    }

    protected OtpErlangObject makeCall(final int timeout,
            final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args0)
            throws RpcException, SignatureException {
        checkAvailability();
        final OtpErlangObject result = RpcHelper.rpcCall(getNode(),
                getFullNodeName(), logCalls, gleader, module, fun, timeout,
                signature, args0);
        return result;
    }

    protected OtpErlangObject makeCall(final int timeout, final String module,
            final String fun, final String signature, final Object... args0)
            throws RpcException, SignatureException {
        return makeCall(timeout, new OtpErlangAtom("user"), module, fun,
                signature, args0);
    }

    protected void makeCast(final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args0)
            throws SignatureException, RpcException {
        checkAvailability();
        RpcHelper.rpcCast(getNode(), getFullNodeName(), logCalls, gleader,
                module, fun, signature, args0);
    }

    protected void makeCast(final String module, final String fun,
            final String signature, final Object... args0)
            throws SignatureException, RpcException {
        makeCast(new OtpErlangAtom("user"), module, fun, signature, args0);
    }

    public synchronized void registerStatusHandler(final OtpNodeStatus handler) {
        if (getNode() != null) {
            getNode().registerStatusHandler(handler);
            getNode().registerStatusHandler(this);
        }
    }

    public synchronized void restart() {
        exitStatus = -1;
        if (available) {
            return;
        }
        restarted++;
        ErlLogger.info("restarting runtime for %s", toString());
        if (getNode() != null) {
            getNode().close();
            fNode = null;
        }
        try {
            initializeRuntime();
            connect();
            initErlang(monitor, watch);
        } catch (final IOException e) {
            ErlLogger.error(e);
        }
        codeManager.reRegisterBundles();
        // initErlang();
        // fixme eventdaemon
    }

    public synchronized void setAvailable(final boolean up) {
        available = up;
    }

    public void setDebug(final boolean b) {
        fDebug = b;
    }

    public synchronized void setExitStatus(final int v) {
        exitStatus = v;
    }

    protected void setRemoteRex(final OtpErlangPid watchdog) {
        try {
            getEventBox().link(watchdog);
        } catch (final OtpErlangExit e) {
        }
    }

    public void stop() {
        stopped = true;
    }

    protected void wait_for_epmd() throws BackendException {
        // If anyone has a better solution for waiting for epmd to be up, please
        // let me know
        int tries = 50;
        boolean ok = false;
        do {
            Socket s;
            try {
                s = new Socket("localhost", EPMD_PORT);
                s.close();
                ok = true;
            } catch (final IOException e) {
            }
            try {
                Thread.sleep(100);
                // ErlLogger.debug("sleep............");
            } catch (final InterruptedException e1) {
            }
            tries--;
        } while (!ok && tries > 0);
        if (!ok) {
            final String msg = "Couldn't contact epmd - erlang backend is probably not working\n"
                    + "  Possibly your host's entry in /etc/hosts is wrong.";
            ErlLogger.error(msg);
            throw new BackendException(msg);
        }
    }

    private boolean waitForCodeServer() {
        try {
            OtpErlangObject r;
            int i = 10;
            do {
                r = call("erlang", "whereis", "a", "code_server");
                try {
                    Thread.sleep(200);
                } catch (final InterruptedException e) {
                }
                i--;
            } while (!(r instanceof OtpErlangPid) && i > 0);
            if (!(r instanceof OtpErlangPid)) {
                ErlLogger.error("code server did not start in time for %s",
                        getInfo().getName());
                return false;
            }
            ErlLogger.debug("code server started");
            return true;
        } catch (final Exception e) {
            ErlLogger.error("error starting code server for %s: %s", getInfo()
                    .getName(), e.getMessage());
            return false;
        }
    }

    public EventDaemon getEventDaemon() {
        if (eventDaemon == null) {
            initEventDaemon();
        }
        return eventDaemon;
    }

    public boolean isAvailable() {
        return available;
    }

    public void input(final String string) throws IOException {
        if (!isStopped()) {
            if (proxy != null) {
                proxy.write(string);
            } else {
                ErlLogger
                        .warn("Could not load module on backend %s, stream proxy is null",
                                getInfo());
            }
        }
    }

    public OtpMbox createMbox() {
        return getNode().createMbox();
    }

    public OtpMbox createMbox(final String name) {
        return getNode().createMbox(name);
    }

    @Override
    public void remoteStatus(final String node, final boolean up,
            final Object info) {
        if (node.equals(getFullNodeName())) {
            // final String dir = up ? "up" : "down";
            // ErlLogger.debug(String.format("@@: %s %s %s", node, dir, info));
            setAvailable(up);
        }
    }

    @Override
    public void connAttempt(final String node, final boolean incoming,
            final Object info) {
        final String direction = incoming ? "in" : "out";
        ErlLogger.info(String.format("Connection attempt: %s %s: %s", node,
                direction, info));
    }

    public BackendShell getShell(final String id) {
        final BackendShell shell = shellManager.openShell(id);
        if (proxy != null) {
            final IStreamMonitor errorStreamMonitor = proxy
                    .getErrorStreamMonitor();
            errorStreamMonitor.addListener(new IStreamListener() {
                public void streamAppended(final String text,
                        final IStreamMonitor aMonitor) {
                    shell.add(text, IoRequestKind.STDERR);
                }
            });
            final IStreamMonitor outputStreamMonitor = proxy
                    .getOutputStreamMonitor();
            outputStreamMonitor.addListener(new IStreamListener() {
                public void streamAppended(final String text,
                        final IStreamMonitor aMonitor) {
                    shell.add(text, IoRequestKind.STDOUT);
                }
            });
        }
        return shell;
    }

    public void setLogCalls(final boolean logCalls) {
        this.logCalls = logCalls;
    }

    public void removePath(final String path) {
        codeManager.removePath(path);
    }

    public void addPath(final boolean usePathZ, final String path) {
        codeManager.addPath(usePathZ, path);
    }

    public void register(final CodeBundleImpl bundle) {
        codeManager.register(bundle);
    }

    public void unregister(final Bundle b) {
        codeManager.unregister(b);
    }

    public void streamAppended(final String text, final IStreamMonitor aMonitor) {
        if (aMonitor == proxy.getOutputStreamMonitor()) {
            // System.out.println(getName() + " OUT " + text);
        } else if (aMonitor == proxy.getErrorStreamMonitor()) {
            // System.out.println(getName() + " ERR " + text);
        } else {
            // System.out.println("???" + text);
        }
    }

    public ILaunch getLaunch() {
        return launch;
    }

    public void setLaunch(final ILaunch launch2) {
        launch = launch2;
    }

    public boolean isDistributed() {
        return !getInfo().getNodeName().equals("");
    }

    public void launchesTerminated(final ILaunch[] launches) {
        for (final ILaunch aLaunch : launches) {
            if (aLaunch == launch) {
                stop();
            }
        }
    }

    public void launchesAdded(final ILaunch[] launches) {
    }

    public void launchesChanged(final ILaunch[] launches) {
    }

    public void launchesRemoved(final ILaunch[] launches) {
    }

    public void setStreamsProxy(final IStreamsProxy streamsProxy) {
        proxy = streamsProxy;
        if (proxy != null) {
            final IStreamMonitor errorStreamMonitor = proxy
                    .getErrorStreamMonitor();
            errorStreamMonitor.addListener(this);
            final IStreamMonitor outputStreamMonitor = proxy
                    .getOutputStreamMonitor();
            outputStreamMonitor.addListener(this);
        }
    }

    public void addProjectPath(final IProject project) {
        final IErlProject erlProject = ErlangCore.getModel().getErlangProject(
                project);
        final String outDir = project.getLocation()
                .append(erlProject.getOutputLocation()).toOSString();
        if (outDir.length() > 0) {
            ErlLogger.debug("backend %s: add path %s", getName(), outDir);
            if (isDistributed()) {
                final boolean accessible = ErlideUtil
                        .isAccessible(this, outDir);
                if (accessible) {
                    addPath(false/* prefs.getUsePathZ() */, outDir);
                } else {
                    loadBeamsFromDir(project, outDir);
                }
            } else {
                final File f = new File(outDir);
                final BackendManager backendManager = ErlangCore
                        .getBackendManager();
                for (final File file : f.listFiles()) {
                    String name = file.getName();
                    if (!name.endsWith(".beam")) {
                        continue;
                    }
                    name = name.substring(0, name.length() - 5);
                    try {
                        CoreUtil.loadModuleViaInput(this, project, name);
                        backendManager.moduleLoaded(this, project, name);
                    } catch (final ErlModelException e) {
                        e.printStackTrace();
                    } catch (final IOException e) {
                        e.printStackTrace();
                    }
                }
            }
        }
    }

    public void removeProjectPath(final IProject project) {
        final IErlProject erlProject = ErlangCore.getModel().getErlangProject(
                project);
        final String outDir = project.getLocation()
                .append(erlProject.getOutputLocation()).toOSString();
        if (outDir.length() > 0) {
            ErlLogger.debug("backend %s: remove path %s", getName(), outDir);
            if (isDistributed()) {
                removePath(outDir);
            } else {
                ErlLogger.warn("didn't remove project path for %s from %s",
                        project.getName(), getName());
            }
        }
    }

    private void loadBeamsFromDir(final IProject project, final String outDir) {
        final File dir = new File(outDir);
        if (dir.isDirectory()) {
            final BackendManager backendManager = ErlangCore
                    .getBackendManager();
            for (final File f : dir.listFiles()) {
                final Path beamPath = new Path(f.getPath());
                final String beamModuleName = BackendUtils
                        .getBeamModuleName(beamPath);
                if (beamModuleName != null) {
                    try {
                        boolean ok = false;
                        final OtpErlangBinary bin = BeamUtil.getBeamBinary(
                                beamModuleName, beamPath);
                        if (bin != null) {
                            ok = ErlBackend.loadBeam(this, beamModuleName, bin);
                        }
                        if (!ok) {
                            ErlLogger
                                    .error("Could not load %s", beamModuleName);
                        }
                        backendManager.moduleLoaded(this, project,
                                beamModuleName);
                    } catch (final Exception ex) {
                        ErlLogger.warn(ex);
                    }
                }
            }
        }
    }

    public boolean isManaged() {
        return managed;
    }

    public void setManaged(final boolean b) {
        managed = b;
    }

    public boolean doLoadOnAllNodes() {
        return getInfo().loadOnAllNodes();
    }

    public boolean isCompatibleWithProject(final IProject project) {
        final IErlProject erlProject = ErlangCore.getModel().getErlangProject(
                project);
        final RuntimeVersion projectVersion = erlProject.getRuntimeVersion();
        return getInfo().getVersion().isCompatible(projectVersion);
    }

}
