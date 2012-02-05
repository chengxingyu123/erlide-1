package com.ericsson.otp.erlang;

public class FooNested {
	FooNested (Integer i) {prop1 = i;}
	Integer prop1;
	Foo foo;
	public Integer getProp1 (){
		 return prop1;
	 }
	public void setProp1 (Integer prop1){
		 this.prop1=prop1;
	 } 

	public Foo getFoo (){
		 return foo;
	 }
	public void setFoo (Foo foo){
		 this.foo=foo;
	 }


}
