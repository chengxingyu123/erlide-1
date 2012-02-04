package org.erlide.ui.views.objectlist;

import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.IRpcCallSite;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class Objectlist {
    public static final String MODULE_NAME = "erlide_objectlist";

    public static void objectListInit(final IRpcCallSite b) {
        if (b == null) {
            return;
        }
        try {
            b.call(MODULE_NAME, "object_list_init", "");
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public static OtpErlangList getObjectList(final IRpcCallSite b) {
        if (b == null) {
            return new OtpErlangList();
        }
        try {
            final OtpErlangObject result = b.call(MODULE_NAME, "object_list",
                    "");
            return (OtpErlangList) result;
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
        return new OtpErlangList();
    }

    public static OtpErlangObject getObjectInfo(final IRpcCallSite b,
            final OtpErlangPid pid) {
        if (b == null) {
            return new OtpErlangAtom("error");
        }
        try {
            return b.call(MODULE_NAME, "get_object_info", "p", pid);
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
        return new OtpErlangAtom("error");
    }

}
