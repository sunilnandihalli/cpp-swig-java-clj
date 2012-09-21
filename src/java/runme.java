// runme.java

public class runme {
  static {
   System.loadLibrary("test");
  }

  public static void main(String argv[]) {
    System.out.println(test.fact(4));
  }
}
