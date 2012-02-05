package org.erlide.jinterface.util;

import java.util.Map;

import junit.framework.Assert;

import org.junit.Test;

import com.siteview.object.ErlObject;
import com.siteview.object.ErlObjectStore;

public class ObjectTest {
	
    @Test
    public void createObjTest() throws Throwable {
    	ErlObjectStore.delete_all();
    	
    	ErlObject pointTest = ErlObjectStore.create("point","pointTest");
    	
        final String expected = "pointTest";
        final String actual = pointTest.getName();
        ErlObjectStore.delete(pointTest);
        Assert.assertEquals(expected, actual);
    }

    @Test
    public void deleteAllTest() throws Throwable {
    	ErlObjectStore.delete_all();
        final Long expected = 0L;
        final Long actual = ErlObjectStore.size();
        Assert.assertEquals(expected, actual);
    }
    
    @Test
    public void deleteObjTest() throws Throwable {
    	ErlObjectStore.delete_all();
       	ErlObject pointTest = ErlObjectStore.create("point","pointTest");
       	ErlObjectStore.delete(pointTest);    	
    	
        Assert.assertFalse(ErlObjectStore.isValidName("pointTest"));
    }
    
    @Test
    public void attrTest() throws Throwable {
    	ErlObjectStore.delete_all();
       	ErlObject pointTest = ErlObjectStore.create("point","pointTest");
    	pointTest.set("X", 10);
    	   	
        final int  expected = 10;
        final int actual = Integer.parseInt(pointTest.get("X").toString());

        ErlObjectStore.delete("pointTest"); 
        Assert.assertEquals(expected, actual);
    }
    
    @Test
    public void get_defined_attrs() throws Throwable {
    	ErlObjectStore.delete_all();
       	ErlObject pointTest = ErlObjectStore.create("point","pointTest");
    	pointTest.set("X", 10);
    	pointTest.set("Y", 100);
    	
    	Map<String,Object> attrs = pointTest.get_defined_attrs();
    	   	
        final int  expected = 100;
        final int actual = Integer.parseInt(attrs.get("Y").toString());

        ErlObjectStore.delete("pointTest"); 
        Assert.assertEquals(10, Integer.parseInt(attrs.get("X").toString()));
        Assert.assertEquals(100, Integer.parseInt(attrs.get("Y").toString()));
    }
    
    @Test
    public void get_system_attrs() throws Throwable {
    	ErlObjectStore.delete_all();
    	String className = "point";
    	String objName = "pointTest";
       	ErlObject pointTest = ErlObjectStore.create(className,objName);
    	
    	Map<String,Object> attrs = pointTest.get_system_attrs();
    	   	
        ErlObjectStore.delete(objName); 
        
        Assert.assertEquals(className, attrs.get("class").toString());
        Assert.assertEquals(objName, attrs.get("name").toString());
    }
}
