// TODO: integrate this into erlide

package com.ericsson.otp.erlang;

import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.commons.pool.ObjectPool;
import org.apache.commons.pool.impl.StackObjectPool;

public class OtpInterfaceImpl implements OtpInterface {
	private final Log log = LogFactory.getLog(OtpInterfaceImpl.class);
	private final static ObjectPool pool = new StackObjectPool(new OtpPoolFactory("master","erlide","java"));
	
	public Object call(String module,String function, List<Object> param)
			throws Exception {
		long timer = System.currentTimeMillis();
		{
			OtpConnection conn = null;
			try{
				conn = (OtpConnection) pool.borrowObject();
				conn.sendRPC(module, function, (OtpErlangList)OtpConverter.Object2OtpErlangObject((Object)param));
				OtpErlangObject retobj = conn.receiveRPC();
				return OtpConverter.OtpErlangObject2Object(retobj);
			}finally{
				pool.returnObject(conn);
				log.debug(module + ":" + function + "  --> " + param + "  timelong=" +  (System.currentTimeMillis() - timer) + "mms");
			}
		}
	}

}
