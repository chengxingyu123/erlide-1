/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2000-2009. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd%
 */
package com.ericsson.otp.erlang;

import java.io.Serializable;
import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.util.Arrays;

import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import javolution.util.FastList;
import javolution.util.FastMap;
import javolution.util.FastSet;
/**
 * Provides a Java representation of Erlang tuples. Tuples are created from one
 * or more arbitrary Erlang terms.
 * 
 * <p>
 * The arity of the tuple is the number of elements it contains. Elements are
 * indexed from 0 to (arity-1) and can be retrieved individually by using the
 * appropriate index.
 */
public class OtpErlangTuple extends OtpErlangObject implements Serializable,
	Cloneable {
    // don't change this!
    static final long serialVersionUID = 9163498658004915935L;

    private static final OtpErlangObject[] NO_ELEMENTS = new OtpErlangObject[0];

    private OtpErlangObject[] elems = NO_ELEMENTS;

    /**
     * Create a unary tuple containing the given element.
     * 
     * @param elem
     *                the element to create the tuple from.
     * 
     * @exception java.lang.IllegalArgumentException
     *                    if the element is null.
     */
    public OtpErlangTuple(final OtpErlangObject elem) {
	if (elem == null) {
	    throw new java.lang.IllegalArgumentException(
		    "Tuple element cannot be null");
	} else {
	    elems = new OtpErlangObject[] { elem };
	}
    }

    /**
     * Create a tuple from an array of terms.
     * 
     * @param elems
     *                the array of terms to create the tuple from.
     * 
     * @exception java.lang.IllegalArgumentException
     *                    if the array is empty (null) or contains null
     *                    elements.
     */
    public OtpErlangTuple(final OtpErlangObject[] elems) {
	this(elems, 0, elems.length);
    }

    /**
     * Create a 2 element tuple from an Map<String, Object>.
     * 
     * @param map
     *                the map to create the tuple from.
     * 
     * @exception java.lang.IllegalArgumentException
     *                    if the array is empty (null) or contains null
     *                    elements.
     */
    public OtpErlangTuple(Map<String,Object>map) {  
    	List<Object> erlList = FastList.newInstance();
    	try {
    		for (Map.Entry<String, ?> entry: map.entrySet()) {
    			String key = (String)entry.getKey();
    			if (key.startsWith("\"") && key.endsWith("\"")) {
    				erlList.add(new OtpErlangString((String) key.subSequence(1, key.length()-1)));
    			}
    			else erlList.add(new OtpErlangAtom(key));
    			
	    		if (key.endsWith("Atom")) {
	    			erlList.add(new OtpErlangAtom(entry.getValue().toString()));
	    			} 
	    		else {
	    			erlList.add(OtpConverter.Object2OtpErlangObject(entry.getValue()));
	    		}
	    		
    		}
    	} catch(Exception e) {
    		System.out.println(e);
    	}	

    	if (map.size() < 1) {
    		elems = NO_ELEMENTS;
    	} else {
    	    this.elems = erlList.toArray(new OtpErlangObject[2]);
    	}
    	
    }

    public OtpErlangTuple(Set<Object> set) {  
      List<Object> erlList = FastList.newInstance();
      try {
           Iterator iter = set.iterator();
           while (iter.hasNext()) {                 
               erlList.add(OtpConverter.Object2OtpErlangObject(iter.next()));
           }
      } catch(Exception e) {
          System.out.println(e);
      }   

      if (set.size() < 1) {
          elems = NO_ELEMENTS;
      } else {
          this.elems = erlList.toArray(new OtpErlangObject[erlList.size()]);
      }
      
    }
    
    public OtpErlangTuple(Array obj) {  
      final int len = Array.getLength(obj);
      final OtpErlangObject[] vv = new OtpErlangObject[len];
      for (int i = 0; i < len; i++) {
          vv[i] = OtpConverter.Object2OtpErlangObject(Array.get(obj, i));
      }
      this.elems = vv;
//      List<Object> erlList = FastList.newInstance();
//      try {
//           Iterator iter = set.iterator();
//           while (iter.hasNext()) {                 
//               erlList.add(OtpConverter.Object2OtpErlangObject(iter.next()));
//           }
//      } catch(Exception e) {
//          System.out.println(e);
//      }   
//
//      if (set.size() < 1) {
//          elems = NO_ELEMENTS;
//      } else {
//          this.elems = erlList.toArray(new OtpErlangObject[erlList.size()]);
//      }
      
    }
    
    /**
     * Create a tuple from an array of terms.
     * 
     * @param elems
     *                the array of terms to create the tuple from.
     * @param start
     *                the offset of the first term to insert.
     * @param count
     *                the number of terms to insert.
     * 
     * @exception java.lang.IllegalArgumentException
     *                    if the array is empty (null) or contains null
     *                    elements.
     */
    public OtpErlangTuple(OtpErlangObject[] elems, final int start,
	    final int count) {
	if (elems == null) {
	    throw new java.lang.IllegalArgumentException(
		    "Tuple content can't be null");
	} else if (count < 1) {
	    elems = NO_ELEMENTS;
	} else {
	    this.elems = new OtpErlangObject[count];
	    for (int i = 0; i < count; i++) {
		if (elems[start + i] != null) {
		    this.elems[i] = elems[start + i];
		} else {
		    throw new java.lang.IllegalArgumentException(
			    "Tuple element cannot be null (element"
				    + (start + i) + ")");
		}
	    }
	}
    }

    /**
     * Create a tuple from a stream containing an tuple encoded in Erlang
     * external format.
     * 
     * @param buf
     *                the stream containing the encoded tuple.
     * 
     * @exception OtpErlangDecodeException
     *                    if the buffer does not contain a valid external
     *                    representation of an Erlang tuple.
     */
    public OtpErlangTuple(final OtpInputStream buf)
	    throws OtpErlangDecodeException {
	final int arity = buf.read_tuple_head();

	if (arity > 0) {
	    elems = new OtpErlangObject[arity];

	    for (int i = 0; i < arity; i++) {
		elems[i] = buf.read_any();
	    }
	} else {
	    elems = NO_ELEMENTS;
	}
    }

    /**
     * Get the arity of the tuple.
     * 
     * @return the number of elements contained in the tuple.
     */
    public int arity() {
	return elems.length;
    }
	
	/**
     * Check is a tuple is a {K,V} map
     * 
     * @return the map or null.
     */
	public Map<String,Object> convertMap() throws OtpErlangException {
//    	Map<String,Object> map = FastMap.newInstance(); 
		final int a = arity();
		Map<String,Object> map = FastMap.newInstance();
    	
    	try {
			if (a==2 && (elementAt(0) instanceof OtpErlangAtom)) {
//				String key = elementAt(0).toString();
				map.put(elementAt(0).toString(),OtpConverter.OtpErlangObject2Object(elementAt(1)));
			} 
			else {
				for(int i=0;i<a;i++) {
					OtpErlangTuple tuple = (OtpErlangTuple) elementAt(i);  
					map.put(tuple.elementAt(0).toString(),OtpConverter.OtpErlangObject2Object(tuple.elementAt(1)));
				}
			}
			
			
    	} catch (Exception e) {
			return null;
        }  
		return map;		
    }
	
	
	
	public List<Object> convertList() throws OtpErlangException {
		final int a = arity();
//    	List<Object> list = FastList.newInstance(); 
    	List<Object> list = FastList.newInstance();
    	
    	try {
        	for(OtpErlangObject elem : elems){  	
				list.add(OtpConverter.OtpErlangObject2Object(elem));
	    	}
    	} catch (Exception e) {
			return null;
        }
		return list;
	}

   
    public Set<Object> convertSet() throws OtpErlangException {
      final int a = arity();
//    List<Object> list = FastList.newInstance(); 
      //FIXME: using set to represent tuple, do not allow redundant elements
      //convert it to vector
      FastSet<Object> set = FastSet.newInstance();
      
      try {
          for(OtpErlangObject elem : elems){      
              set.add(OtpConverter.OtpErlangObject2Object(elem));
          }
      } catch (Exception e) {
          return null;
      }
      return set;
  }
 
    
    public Object convertArray() throws OtpErlangException {
      final int a = arity();
      
      Object array = null;
      try
      {
        array = Array.newInstance(Class.forName("java.lang.Object"),a);
        for (int i = 0; i < a; i++) {
          Array.set(array, i,OtpConverter.OtpErlangObject2Object(elems[i]));
      }
      }
      catch (NegativeArraySizeException e1)
      {
        // TODO Auto-generated catch block
        e1.printStackTrace();
      }
      catch (ClassNotFoundException e1)
      {
        // TODO Auto-generated catch block
        e1.printStackTrace();
      }
      return array;
    }
		/**
     * Check is a tuple is a {{K,V},{K,V}, ...,{K,V}} map
     * 
     * @return the map or null.
     */
	public final boolean isMap() {
		final int a = arity();
		if ((a == 2) && ((elementAt(0) instanceof OtpErlangAtom) || (elementAt(0) instanceof OtpErlangString))) {
			return true;
		}
		for(int i=0;i<a;i++) {			
			if (!(elementAt(i) instanceof OtpErlangTuple)) return false; 
			OtpErlangTuple tuple=(OtpErlangTuple) elementAt(i);
			if (!((tuple.arity() == 2) && 
				((tuple.elementAt(0) instanceof OtpErlangAtom)) || (tuple.elementAt(0) instanceof OtpErlangString))) 
				return false;			
		}
		return true;
	}
	
	/**
     * Check is a tuple is a {{yyyy,mm,dd},{hh,mm,ss}} calendar
     * 
     * @return the map or null.
     */
	public final Timestamp checkDateTime() {
		final int a = arity();	
		Timestamp datetime = new Timestamp(0); 
		if (a != 2) return datetime;		
		if (!((elementAt(0) instanceof OtpErlangTuple))) return datetime;
		if (!((elementAt(1) instanceof OtpErlangTuple))) return datetime;
		OtpErlangTuple elem0= (OtpErlangTuple)elementAt(0); if(elem0.arity() !=3) return datetime;
		OtpErlangTuple elem1= (OtpErlangTuple)elementAt(1); if(elem1.arity() !=3) return datetime;
		int year,month,day,hour,minute,second;
		try {
			 year = Integer.parseInt(elem0.elementAt(0).toString()); if (year > 3000  || year < 1970) return datetime; 
			 month = Integer.parseInt(elem0.elementAt(1).toString()); if (month < 0  || month > 12) return datetime;
			 day = Integer.parseInt(elem0.elementAt(2).toString()); if (day < 0  || day > 31) return datetime;
			 hour = Integer.parseInt(elem1.elementAt(0).toString()); if (hour < 0  || hour > 24) return datetime;
			 minute = Integer.parseInt(elem1.elementAt(1).toString());if (minute< 0  || minute> 60) return datetime;
			 second = Integer.parseInt(elem1.elementAt(2).toString());if (second< 0  || second> 60) return datetime;
		} catch (NumberFormatException e){
			return datetime;
		}
		
		String datetimeStr = year + "-" +month+"-"+day+" "+hour+":"+minute +":"+second;
		DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		Timestamp ts ;
		try {
			ts = new Timestamp( df.parse( datetimeStr ).getTime() );
		} catch(Exception e) {
			return datetime;
		}
		
		return ts;
	}


    /**
     * Get the specified element from the tuple.
     * 
     * @param i
     *                the index of the requested element. Tuple elements are
     *                numbered as array elements, starting at 0.
     * 
     * @return the requested element, of null if i is not a valid element index.
     */
    public OtpErlangObject elementAt(final int i) {
	if (i >= arity() || i < 0) {
	    return null;
	}
	return elems[i];
    }

    /**
     * Get all the elements from the tuple as an array.
     * 
     * @return an array containing all of the tuple's elements.
     */
    public OtpErlangObject[] elements() {
	final OtpErlangObject[] res = new OtpErlangObject[arity()];
	System.arraycopy(elems, 0, res, 0, res.length);
	return res;
    }

    /**
     * Get the string representation of the tuple.
     * 
     * @return the string representation of the tuple.
     */
    @Override
    public String toString() {
	int i;
	final StringBuffer s = new StringBuffer();
	final int arity = elems.length;

	s.append("{");

	for (i = 0; i < arity; i++) {
	    if (i > 0) {
		s.append(",");
	    }
	    if(elems[i] != null) s.append(elems[i].toString());
	}

	s.append("}");

	return s.toString();
    }

    /**
     * Convert this tuple to the equivalent Erlang external representation.
     * 
     * @param buf
     *                an output stream to which the encoded tuple should be
     *                written.
     */
    @Override
    public void encode(final OtpOutputStream buf) {
	final int arity = elems.length;

	buf.write_tuple_head(arity);

	for (int i = 0; i < arity; i++) {
	    buf.write_any(elems[i]);
	}
    }

    /**
     * Determine if two tuples are equal. Tuples are equal if they have the same
     * arity and all of the elements are equal.
     * 
     * @param o
     *                the tuple to compare to.
     * 
     * @return true if the tuples have the same arity and all the elements are
     *         equal.
     */
    @Override
    public boolean equals(final Object o) {
	if (!(o instanceof OtpErlangTuple)) {
	    return false;
	}

	final OtpErlangTuple t = (OtpErlangTuple) o;
	final int a = arity();

	if (a != t.arity()) {
	    return false;
	}

	for (int i = 0; i < a; i++) {
	    if (!elems[i].equals(t.elems[i])) {
		return false; // early exit
	    }
	}

	return true;
    }
    
    protected int doHashCode() {
	OtpErlangObject.Hash hash = new OtpErlangObject.Hash(9);
	final int a = arity();
	hash.combine(a);
	for (int i = 0; i < a; i++) {
	    hash.combine(elems[i].hashCode());
	}
	return hash.valueOf();
    }
    
    @Override
    public Object clone() {
	final OtpErlangTuple newTuple = (OtpErlangTuple) super.clone();
	newTuple.elems = elems.clone();
	return newTuple;
    }
}
