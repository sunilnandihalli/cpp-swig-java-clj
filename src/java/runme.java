// runme.java

public class runme {
  static {
   System.loadLibrary("misc_utils");
  }

  public static void main(String argv[]) {
    System.out.println(misc_utils.fact(4));
  }
}
