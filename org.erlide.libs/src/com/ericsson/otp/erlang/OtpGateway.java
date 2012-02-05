package com.ericsson.otp.erlang;

public class OtpGateway {
	private static OtpInterface otpIf = null;
	
	public static OtpInterface getOtpInterface() throws Exception
	{
		if (otpIf == null){
			otpIf = (OtpInterface) (new OtpInterfaceImpl());
		}
		return otpIf;
	}

}
