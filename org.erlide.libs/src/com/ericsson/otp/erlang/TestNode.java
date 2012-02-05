/*
 * %CopyrightBegin%
 * 
 * Copyright SiteView.com 2011. All Rights Reserved.
 * 
 * The contents of this file are subject to the Eclipse Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. 
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd%
 */
package com.ericsson.otp.erlang;

import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

import javolution.util.FastList;
import javolution.util.FastMap;

public class TestNode
{
	public final static String module = TestNode.class.getName();

    public TestNode()
    {
    }

    public static void main(String args[])
        throws Exception
    {

    	OtpNode myNode = new OtpNode("javanode", "cookie");
        OtpMbox myMbox = myNode.createMbox("java_mail_box");
        Integer counter = 0;
        OtpErlangAtom myAtom = new OtpErlangAtom("ok");
        OtpErlangObject erlobject;
        OtpErlangObject myMsg=null;
        OtpErlangPid from =null;

        
        OtpConverter.setAtomName(Arrays.asList("id", "monitorId", "groupId"));

		do
            try
            {
            	System.out.println("my node="+myNode.node()+",cookie="+myNode.cookie+",mailbox="+myMbox.getName());
            	OtpMsg msg = myMbox.receiveMsg();
            	erlobject = msg.getMsg();

				if (erlobject instanceof OtpErlangTuple) {
    				OtpErlangTuple t = (OtpErlangTuple) erlobject;
    				from = (OtpErlangPid)t.elementAt(0);
    				myMsg = t.elementAt(t.arity()-1);
    			}
                System.out.println(myMsg.toString());
                
//                Map<String,Object> map = (Map<String,Object>)OtpConverter.OtpErlangObject2Object(myMsg);
                Object map= OtpConverter.OtpErlangObject2Object(myMsg);
                System.out.println(map.toString());

				List<Object> myList = FastList.newInstance();
                Foo bar = new Foo(10,"bar");
                Foo bar2  = new Foo(20,"bar2");  
                FooSon barson = new FooSon(30,"barson");
				FooNested barNested = new FooNested(40);
//				barNested.setFoo(bar);
                barson.setProp3("new barson3");
                barson.setProp2("new bar son2");
                barson.setProp1(2000000);
//                myList.add(bar);
//                myList.add(bar2);
//                myList.add(barson);
//				myList.add(barNested);
				myList.add(map);
				
                java.util.Date today = new java.util.Date();
                java.sql.Timestamp now = new java.sql.Timestamp(today.getTime());
//                myList.add(now);
                
                Map<String,Object> myMap = FastMap.newInstance();
                myMap.put("listValue", myList);
                
                OtpErlangObject erlObj = OtpConverter.Object2OtpErlangObject(map);
//                OtpErlangList erlList = (OtpErlangList)OtpConverter.Object2OtpErlangObject(myList);
                
                List<Object> tupleList = FastList.newInstance();
                tupleList.add(myMbox.self());
                tupleList.add(erlObj);
                OtpErlangTuple erlMap = new OtpErlangTuple(tupleList.toArray(new OtpErlangObject[tupleList.size()]));
                System.out.println(erlMap.toString());
                
                myMbox.send(from, erlMap);
               
                counter++;
            }
            catch(OtpErlangExit e)
            {
                return;
            }
        while(true);
    }
}