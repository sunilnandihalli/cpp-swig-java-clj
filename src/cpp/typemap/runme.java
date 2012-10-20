
public class runme {

  static {
    try {
	System.loadLibrary("typemap_java");
    } catch (UnsatisfiedLinkError e) {
      System.err.println("Native code library failed to load. See the chapter on Dynamic Linking Problems in the SWIG Java documentation for help.\n" + e);
      System.exit(1);
    }
  }

  public static void main(String argv[]) {
    String s = "brave new world";
    typemap_module.f1(s);
    System.out.println("f1(String): " + s);

    byte b[] = new byte[25];
    typemap_module.f2(b);
    System.out.println("f2(byte[]): " + new String(b));

    StringBuffer sb = new StringBuffer(20);
    typemap_module.f3(sb);
    System.out.println("f3(StringBuffer): " + sb);
  }
}
