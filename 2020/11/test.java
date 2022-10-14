class A {
	A  self() { 
		return this;
	}
	
	public void printA () {
		System.out.println(“In A”);
	}
}

class B extends A {
	public void printB() {
		System.out.println(“In B”);
	}
}

class WontCompile{
public static void main(String args[])
{
	new B().self().printB;
}
}