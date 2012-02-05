//TODO: add junit test, learning from org.erlide.jinterface.tests

package com.ericsson.otp.erlang;


import java.lang.reflect.Array;
import java.util.Arrays;
import java.sql.Timestamp;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import javolution.util.FastMap;
import javolution.util.FastSet;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.beanutils.PropertyUtils;

/*
JInterface OtpErlang.jar library provides a wrap of the native Java data type.  Developers have to convert 
between Java native type and OtpObject.  JInterface2 provides transparent conversion between Erlang and Java 
data type, including the nested list of tuple.

A new class OtpConverter.java is added, its OtpErlangObject2Object and Object2OtpErlangObject methods can 
be used to convert between Erlang and Java type.  Java Bean class can be passed-in as Erlang message and it's
 automatically converted to Erlang List.

It is straightforward to use the new JInterface2 and it only depends on two Apache Common packages (beanutils 
and logging).  It is fully backward compatible and can be used as a drop-in replacement of the original OtpErlang.jar.

 */

public final class OtpConverter
{

	public static final OtpErlangAtom NULL_ATOM = new OtpErlangAtom("null");
	public static final OtpErlangString NULL_STRING = new OtpErlangString("");
    public static final String module = OtpConverter.class.getClass().getName();
    private static List<String> atomNameList;
    private static Set<String> atomValueList=FastSet.newInstance();
    
    public OtpConverter()
    {
    }
    
    public static void setAtomName(List<String> atomNameList1) {
    	atomNameList = atomNameList1;
    }
    
    public static List<String> getAtomName() {
    	return atomNameList ;
    }
    
    public static void setatomValueList(Set<String> atomValueList1) {
    	atomValueList = atomValueList1;
    }
    
    public static Set<String> getatomValueList() {
    	return atomValueList ;
    }

    public static Object OtpErlangObject2Object(OtpErlangObject elem)
    {
        OtpErlangList subList;
        try
        {
            if(elem == null) return null;
            String type =elem.getClass().getName(); 
            if (!(type.contains("OtpErlang"))) return (Object) elem;
            

	        if(elem instanceof OtpErlangTuple)
	        {
	            OtpErlangTuple subTuple = (OtpErlangTuple)elem;
	            Timestamp ts = subTuple.checkDateTime();
	            if (ts.getTime()>0) return ts; 
	            return subTuple.isMap() ? subTuple.convertMap() : subTuple.convertArray();
	        } else if(elem instanceof OtpErlangList) {          
				subList = (OtpErlangList)elem;
				String tempStr = subList.stringValue2();
				if(tempStr != null) return tempStr;				
				return subList.isMap() ? subList.convertMap() : subList.convertList();
			} 
	        else if(elem instanceof OtpErlangString)  return ((OtpErlangString)elem).stringValue();
	        else if(elem instanceof OtpErlangLong)  return ((OtpErlangLong)elem).longValue();
	        else if(elem instanceof OtpErlangAtom) {
	        	String atomValue = ((OtpErlangAtom)elem).atomValue();
	        	atomValueList.add(atomValue); 
	        	return atomValue;
	        }
	        else if(elem instanceof OtpErlangPid)  return (OtpErlangPid)elem;
	        else if(elem instanceof OtpErlangInt)  return ((OtpErlangInt)elem).intValue();
	        else if(elem instanceof OtpErlangDouble)  return ((OtpErlangDouble)elem).doubleValue();
	        else if(elem instanceof OtpErlangFloat)  return ((OtpErlangFloat)elem).floatValue();
	        else if(elem instanceof OtpErlangBoolean)  return ((OtpErlangBoolean)elem).booleanValue();
	        else if(elem instanceof OtpErlangChar)  return ((OtpErlangChar)elem).charValue();
	        else if(elem instanceof OtpErlangByte)  return ((OtpErlangByte)elem).byteValue();
	        else if(elem instanceof OtpErlangShort)  return ((OtpErlangShort)elem).shortValue();
	        else if(elem instanceof OtpErlangUShort)  return ((OtpErlangUShort)elem).shortValue();
	        else if(elem instanceof OtpErlangUInt)  return ((OtpErlangUInt)elem).shortValue();
	        else if(elem instanceof OtpErlangBinary)  return ((OtpErlangBinary)elem).binaryValue();
	        else if(elem instanceof OtpErlangBitstr)  return ((OtpErlangBitstr)elem).binaryValue();
	        else  {
	        	System.out.println((new StringBuilder()).append("Could not convert the erlang type [").append(elem.getClass().getName()).append("] to java type.").toString());
	            return null;
			}
        } catch(Exception e)
        {
        	System.out.println((new StringBuilder()).append("Could not convert the erlang type [").append(elem.getClass().getName()).append("] to java type.").toString());
        	return null;
        }
    }

    public static OtpErlangObject Object2OtpErlangObject(Object elem)
    {
        try
        {
            if(elem == null) return NULL_ATOM;
            String type =elem.getClass().getName(); 
            if (type.contains("OtpErlang")) return (OtpErlangObject) elem;
            
            String valueType;
            if((elem instanceof String) && ((String) elem).isEmpty()) return NULL_STRING;
        
			if(elem instanceof Map)
			{
				Map map = (Map)elem;
				return ((OtpErlangObject) (map.size() != 1 ? new OtpErlangList(map) : new OtpErlangTuple(map)));
			} 
//			else if(elem instanceof Object[]) {
//				return 	new OtpErlangList((Object[]) elem) ;
//			}
			else if(elem instanceof List) {
				return 	new OtpErlangList((List<Object>) elem) ;			
			}
			else if(elem instanceof Array) {
              return  new OtpErlangTuple((Array) elem) ;            
            }
//			else if(elem instanceof Object[]) {
//				return 	new OtpErlangList((Object[]) elem) ;			
//			}
			else if (elem.getClass().isArray()) {  
			  final int len = Array.getLength(elem);
              final OtpErlangObject[] vv = new OtpErlangObject[len];
              for (int i = 0; i < len; i++) {
                  vv[i] = Object2OtpErlangObject(Array.get(elem, i));
              }
              return new OtpErlangTuple(vv);		
			}
	         else if (elem instanceof Set) {  
               return new OtpErlangTuple((Set<Object>)elem);        
             }
			//simple values
			else if(elem instanceof String) {
				return atomValueList.contains((String)elem) ? 
						new OtpErlangAtom((String)elem) : new OtpErlangString((String)elem) ;
			}
			else if(elem instanceof Long) return new OtpErlangLong((Long)elem);
			else if(elem instanceof Integer) return new OtpErlangInt((Integer)elem);
			else if(elem instanceof Float) return new OtpErlangFloat((Float)elem);
			else if((elem instanceof Date) || (elem instanceof Timestamp) || (elem instanceof java.sql.Date)) {
				Calendar cal=Calendar.getInstance();
				cal.setTime((Date) elem); 
				
				OtpErlangInt[] erldate = new OtpErlangInt[3]; 
				erldate[0]=new OtpErlangInt(cal.get(Calendar.YEAR));
				erldate[1]=new OtpErlangInt(cal.get(Calendar.MONTH)+1);
				erldate[2]=new OtpErlangInt(cal.get(Calendar.DAY_OF_MONTH));
				
				OtpErlangInt[] erltime = new OtpErlangInt[3];
				erltime[0]=new OtpErlangInt(cal.get(Calendar.HOUR_OF_DAY));
				erltime[1]=new OtpErlangInt(cal.get(Calendar.MINUTE));
				erltime[2]=new OtpErlangInt(cal.get(Calendar.SECOND));
				
				OtpErlangTuple[] erldatetime = new OtpErlangTuple[2];
				erldatetime[0]= new OtpErlangTuple(erldate);
				erldatetime[1]= new OtpErlangTuple(erltime);

				return new OtpErlangTuple(erldatetime);  
			}
			else if(elem instanceof Double) return new OtpErlangDouble((Double)elem);
			else if(elem instanceof Short) return new OtpErlangShort((Short)elem);
			else if(elem instanceof Boolean) return new OtpErlangBoolean((Boolean)elem);
			else if(elem instanceof Byte) return new OtpErlangByte((Byte)elem);
			else if(elem instanceof Character) return new OtpErlangChar((Character)elem);
	        else if(elem instanceof OtpErlangPid)  return (OtpErlangPid)elem;
			else //java objects other than list, map and simple value
			{/* if the POJO is Bean complied, obtain the value with the help of beanutil tool*/

				Map<String,Object> classMap =  FastMap.newInstance();
				Map<String,Object> elemMap = BeanUtils.describe(elem);
//				PropertyDescriptor[] pd =  PropertyUtils.getPropertyDescriptors(elem);
				
				for (Map.Entry<String, ?> entry: elemMap.entrySet()) {
					classMap.put(entry.getKey(),entry.getKey().endsWith("class")?
							entry.getValue():PropertyUtils.getProperty(elem,entry.getKey()));
				}
//				System.out.println("Convert ["+type+"] to "+ classMap.toString());
				return Object2OtpErlangObject(classMap);
			}
    
		} catch(Exception e)
        {
			System.out.println((new StringBuilder()).append("Could not convert the erlang type [").append(elem.getClass().getName()).append("] to erlang type.").toString());
            return null;
        }
    }
}
