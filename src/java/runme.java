// runme.java

public class runme {
  static {
   System.loadLibrary("misc_utils_java");
  }

  public static void main(String argv[]) {
    System.out.println(misc_utils.fact(4));
  }
}
