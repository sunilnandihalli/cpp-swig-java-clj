
public class runme {

  static {
    try {
	System.loadLibrary("native_java");
    } catch (UnsatisfiedLinkError e) {
      System.err.println("Native code library failed to load. See the chapter on Dynamic Linking Problems in the SWIG Java documentation for help.\n" + e);
      System.exit(1);
    }
  }

  public static void main(String argv[]) {
    SWIGTYPE_p_Point p = native_module.point_create(1, 2);
    System.out.println("auto wrapped  : " + example.point_toString1(p));
    System.out.println("manual wrapped: " + example.point_toString2(p));
    native_module.free(new SWIGTYPE_p_void(SWIGTYPE_p_Point.getCPtr(p), false)); //clean up c allocated memory
  }
}
