package com.siteview.object;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.ericsson.otp.erlang.OtpConverter;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpGateway;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpMsg;
import com.ericsson.otp.erlang.OtpNode;


public class ErlObject
{
	public final static String module = ErlObject.class.getName();
	public String name = null;
	public String type = null;

    public ErlObject()
    {
    }
    
    public ErlObject(String Name)
    {
    	this.name = Name;
    }
    
    public ErlObject(String Type, String Name)
    {
    	this.name = Name;
    	this.type = Type;
    }
    
    public void set(String  AttributeName,String AttributeValue) throws Exception
    {
		List<Object> list = new ArrayList<Object>();
		list.add(new OtpErlangAtom(name));
		list.add(new OtpErlangAtom(AttributeName));
		list.add(new OtpErlangAtom(AttributeValue));
    	OtpGateway.getOtpInterface().call("object", "set", list);
    }

    public Object get(String AttributeName) throws Exception
    {
		List<Object> list = new ArrayList<Object>();
		list.add(new OtpErlangAtom(name));
		list.add(new OtpErlangAtom(AttributeName));
    	return OtpGateway.getOtpInterface().call("object", "get", list);
    }
    
    public Object call(String Method, List<Object> params) throws Exception {
		List<Object> list = new ArrayList<Object>();
		list.add(new OtpErlangAtom(name));   	
		list.add(new OtpErlangAtom(Method));   	
		list.add(OtpConverter.Object2OtpErlangObject(params));
    	return OtpGateway.getOtpInterface().call("object", "call", list);
    }


    public List<String> get_defined_attrs() throws Exception
    {
		List<Object> list = new ArrayList<Object>();
		list.add(new OtpErlangAtom(name));
    	return (List<String>) OtpGateway.getOtpInterface().call("object", "get_defined_attrs", list);
    }
    
    public List<String> get_system_attrs() throws Exception
    {
		List<Object> list = new ArrayList<Object>();
    	return (List<String>) OtpGateway.getOtpInterface().call("object", "get_system_attrs", list);
    }
    
}