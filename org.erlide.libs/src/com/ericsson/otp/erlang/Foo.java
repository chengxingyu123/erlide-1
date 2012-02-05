package com.ericsson.otp.erlang;

public class Foo {
	Foo (Integer i, String str) {prop1 = i;prop2 =str;}
	Integer prop1;
	String prop2;
	public Integer getProp1 (){
		 return prop1;
	 }
	public void setProp1 (Integer prop1){
		 this.prop1=prop1;
	 }
	
	public String getProp2 (){
		 return prop2;
	 }
	public void setProp2 (String prop2){
		 this.prop2=prop2;
	 }


}
