
public class runme {

  static {
    try {
	System.loadLibrary("simple_java");
    } catch (UnsatisfiedLinkError e) {
      System.err.println("Native code library failed to load. See the chapter on Dynamic Linking Problems in the SWIG Java documentation for help.\n" + e);
      System.exit(1);
    }
  }

  public static void main(String argv[]) {
    // Call our gcd() function
    
    int x = 42;
    int y = 105;
    int g = simple_module.gcd(x,y);
    System.out.println("The gcd of " + x + " and " + y + " is " + g);
    
    // Manipulate the Foo global variable
    
    // Output its current value
    System.out.println("Foo = " + simple_module.getFoo());
    
    // Change its value
    simple_module.setFoo(3.1415926);
    
    // See if the change took effect
    System.out.println("Foo = " + simple_module.getFoo());
  }
}
