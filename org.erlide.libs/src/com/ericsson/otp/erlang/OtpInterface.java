package com.ericsson.otp.erlang;

import java.util.List;


public interface OtpInterface {	
	public Object call(String module,String function,List<Object> params) throws Exception;
}
