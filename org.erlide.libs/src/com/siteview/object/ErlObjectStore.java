//TODO: add junit test, learning from org.erlide.jinterface.tests

package com.siteview.object;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javolution.util.FastList;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpGateway;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;


public class ErlObjectStore
{
	public final static String module = ErlObjectStore.class.getName();
	public final OtpNode myNode = null;
	public final OtpMbox myMbox = null;

    public ErlObjectStore()
    {
    	OtpNode myNode;
		try {
			myNode = new OtpNode("objectstore", "erlide");
			OtpMbox myMbox = myNode.createMbox("java_mail_box");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    }

    public List<String> get_all_name() throws Exception
    {
		List<Object> list = new ArrayList<Object>();
		Long size = (Long) OtpGateway.getOtpInterface().call("object", "get_size", list);
		
		if (size == 0)   return null;
	
    	return (List<String>) OtpGateway.getOtpInterface().call("object", "get_all_name", list);
    }
    
    public List<ErlObject> get_all() throws Exception
    {
		List<Object> list = new ArrayList<Object>();
		List<String> rtnList = new ArrayList<String>();
		List<ErlObject> erlObjList = FastList.newInstance();
		Long size = (Long) OtpGateway.getOtpInterface().call("object", "get_size", list);
		
		if (size == 0)   return null;
	
		rtnList = (List<String>) OtpGateway.getOtpInterface().call("object", "get_all_name", list);
		
    	for(String ObjName :  rtnList) {
    		erlObjList.add(new ErlObject(ObjName));
    	}
    	return erlObjList;    	
    }
    
    public void delete_all() throws Exception
    {
		List<Object> list = new ArrayList<Object>();
    	OtpGateway.getOtpInterface().call("object", "delete_all", list);
    }
    
    public void delete(String Name) throws Exception
    {
		List<Object> list = new ArrayList<Object>();
		list.add(new OtpErlangAtom(Name));
    	OtpGateway.getOtpInterface().call("object", "delete", list);
    }
    
    public void delete(List<String> NameList) throws Exception
    {
		List<Object> list = new ArrayList<Object>();
		List<OtpErlangAtom> paramList = new ArrayList<OtpErlangAtom>();
		
		for(String objName : NameList) {
			paramList.add(new OtpErlangAtom(objName));		
		}
		list.add(paramList);
		
    	OtpGateway.getOtpInterface().call("object", "delete", list);
    }
    
    
    public ErlObject create(String Type, String Name) throws Exception{
		List<Object> list = new ArrayList<Object>();
		list.add(new OtpErlangAtom(Type)); 
		List<Object> paramList = new ArrayList<Object>();
		paramList.add(new OtpErlangAtom(Name));
		list.add(paramList);
		String rtnObj = (String) OtpGateway.getOtpInterface().call("object", "create", list);
    	return new ErlObject(Type,rtnObj);
    }
    
    public ErlObject get_by_name(String Name) throws Exception{
		return new ErlObject(Name);		
    }
    
    public List<ErlObject> get_by_attr(String AttributeName, String AttributeValue) throws Exception{
		List<Object> list = new ArrayList<Object>();
		List<ErlObject> objList = new ArrayList<ErlObject>();
		
		list.add(AttributeName);
		list.add(AttributeValue);
		
		List<String> rtnList = (List<String>) OtpGateway.getOtpInterface().call("object", "jget_by_attr", list);		
		
		for(String objname : rtnList) {
			objList.add(new ErlObject(objname));			
		}
		
		return objList;
    }

    public List<ErlObject> get_by_attr(String AttributeName, List<String> AttributeValueList) throws Exception{
		List<Object> list = new ArrayList<Object>();
		List<OtpErlangAtom> paramList = new ArrayList<OtpErlangAtom>();
		List<ErlObject> objList = new ArrayList<ErlObject>();
		
		for(String AttributeValue : AttributeValueList) {
			paramList.add(new OtpErlangAtom(AttributeValue));		
		}
		
		list.add(AttributeName);
		list.add(paramList);
		
		List<String> rtnList = (List<String>) OtpGateway.getOtpInterface().call("object", "jget_by_attr", list);		
		
		for(String objname : rtnList) {
			objList.add(new ErlObject(objname));			
		}
		
		return objList;
    }
    
    public static void main(String args[]) throws Exception
    {
    	ErlObjectStore store = new ErlObjectStore();
    	List<String> list = new ArrayList<String>();
    	List<ErlObject> objlist = new ArrayList<ErlObject>();
    	store.create("point","point99");
    	list  = store.get_all_name();
    	objlist  = store.get_all();
    	store.delete_all();

    	objlist  = store.get_all();
    	System.out.println(list.toString());
    	System.out.println(objlist.toString());
    }
    
}