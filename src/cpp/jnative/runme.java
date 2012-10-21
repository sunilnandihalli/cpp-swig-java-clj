
public class runme {

  static {
    try {
	System.loadLibrary("jnative_java");
    } catch (UnsatisfiedLinkError e) {
      System.err.println("Native code library failed to load. See the chapter on Dynamic Linking Problems in the SWIG Java documentation for help.\n" + e);
      System.exit(1);
    }
  }

  public static void main(String argv[]) {
    SWIGTYPE_p_Point p = jnative_module.point_create(1, 2);
    System.out.println("auto wrapped  : " + jnative_module.point_toString1(p));
    System.out.println("manual wrapped: " + " not done ... "/*jnative_module.point_toString2(p)*/);
    jnative_module.free(new SWIGTYPE_p_void(SWIGTYPE_p_Point.getCPtr(p), false)); //clean up c allocated memory
  }
}
