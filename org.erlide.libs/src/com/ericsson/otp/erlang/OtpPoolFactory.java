package com.ericsson.otp.erlang;

import java.util.List;
import java.util.Random;
import java.util.concurrent.CopyOnWriteArrayList;
import java.net.InetAddress;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.commons.pool.BasePoolableObjectFactory;

import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;


public class OtpPoolFactory extends BasePoolableObjectFactory {
	private static final Log loger = LogFactory.getLog(OtpPoolFactory.class);
	
	public String self_node_prefix = "java";
	public String cookie = "erlide";
	public String remotenode = "master";
	
	private static OtpPeer other = null;
	private static final List<String> keys = new CopyOnWriteArrayList<String>();
	
    public OtpPoolFactory(String remotenode,String cookie,String self_node_prefix)
    {
    	this.remotenode = remotenode;
    	this.cookie = cookie;
    	this.self_node_prefix = self_node_prefix;
    }
	
	@Override
	public synchronized Object makeObject() throws Exception {
		String nodename = getNextSelfNodeName();

		OtpSelf self = new OtpSelf(nodename);
		self.setCookie(cookie);

		if (other == null ) {
			other = (remotenode.indexOf("@") > 0) ? new OtpPeer(remotenode) : new OtpPeer(remotenode + "@" + InetAddress.getLocalHost().getHostName());
		}	
		OtpConnection conn = self.connect(other);
		loger.info("makeObject nodename=" + nodename);
		keys.add(nodename);
		return conn;
	}
	
	private String getNextSelfNodeName() throws Exception{
		
		Random random = new Random(System.currentTimeMillis());
		while(true){
			String key =  self_node_prefix + "_" + random.nextInt();
			if (keys.contains(key))continue;
			return key;
		}
	}

	@Override
	public boolean validateObject(Object obj) {
		if (obj == null) return false;
		OtpConnection conn = (OtpConnection) obj;
	    return conn.isConnected();
	}
	
	@Override
	public void destroyObject(Object obj) throws Exception {
		if (obj == null) return;
		OtpConnection conn = (OtpConnection) obj;
		if (conn.isConnected()){
			conn.close();
		}
	}
}
