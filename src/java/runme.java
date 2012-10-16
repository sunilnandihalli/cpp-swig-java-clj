// runme.java

public class runme {
  static {
   System.loadLibrary("java_interop");
  }

  public static void main(String argv[]) {
    System.out.println(misc_utils.fact(4));
  }
}
