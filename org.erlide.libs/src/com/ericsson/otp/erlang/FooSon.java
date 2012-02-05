package com.ericsson.otp.erlang;

public class FooSon extends Foo {
	FooSon(int i, String str) {super(i,str);}
	String prop3; 
	public String getProp3 (){
		 return prop3;
	 }
	public void setProp3 (String prop3){
		 this.prop3=prop3;
	 }


}
