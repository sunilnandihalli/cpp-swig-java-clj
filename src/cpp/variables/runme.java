// This example illustrates global variable access from Java.

import java.lang.reflect.*;

public class runme {
  static {
    try {
        System.loadLibrary("variables_java");
    } catch (UnsatisfiedLinkError e) {
      System.err.println("Native code library failed to load. See the chapter on Dynamic Linking Problems in the SWIG Java documentation for help.\n" + e);
      System.exit(1);
    }
  }

  public static void main(String argv[]) {

// Try to set the values of some global variables

    variables_module.setIvar(42);
    variables_module.setSvar((short)-31000);
    variables_module.setLvar(65537);
    variables_module.setUivar(123456);
    variables_module.setUsvar(61000);
    variables_module.setUlvar(654321);
    variables_module.setScvar((byte)-13);
    variables_module.setUcvar((short)251);
    variables_module.setCvar('S');
    variables_module.setFvar((float)3.14159);
    variables_module.setDvar(2.1828);
    variables_module.setStrvar("Hello World");
    variables_module.setIptrvar(variables_module.new_int(37));
    variables_module.setPtptr(variables_module.new_Point(37,42));
    variables_module.setName("Bill");

    // Now print out the values of the variables

    System.out.println( "Variables (values printed from Java)" );

    System.out.println( "ivar      =" + variables_module.getIvar() );
    System.out.println( "svar      =" + variables_module.getSvar() );
    System.out.println( "lvar      =" + variables_module.getLvar() );
    System.out.println( "uivar     =" + variables_module.getUivar() );
    System.out.println( "usvar     =" + variables_module.getUsvar() );
    System.out.println( "ulvar     =" + variables_module.getUlvar() );
    System.out.println( "scvar     =" + variables_module.getScvar() );
    System.out.println( "ucvar     =" + variables_module.getUcvar() );
    System.out.println( "fvar      =" + variables_module.getFvar() );
    System.out.println( "dvar      =" + variables_module.getDvar() );
    System.out.println( "cvar      =" + (char)variables_module.getCvar() );
    System.out.println( "strvar    =" + variables_module.getStrvar() );
    System.out.println( "cstrvar   =" + variables_module.getCstrvar() );
    System.out.println( "iptrvar   =" + Long.toHexString(SWIGTYPE_p_int.getCPtr(variables_module.getIptrvar())) );
    System.out.println( "name      =" + variables_module.getName() );
    System.out.println( "ptptr     =" + Long.toHexString(SWIGTYPE_p_Point.getCPtr(variables_module.getPtptr())) + variables_module.Point_print(variables_module.getPtptr()) );
    System.out.println( "pt        =" + Long.toHexString(SWIGTYPE_p_Point.getCPtr(variables_module.getPt())) + variables_module.Point_print(variables_module.getPt()) );

    System.out.println( "\nVariables (values printed from C)" );

    variables_module.print_vars();

    System.out.println( "\nNow I'm going to try and modify some read only variables" );

    System.out.println( "     Trying to set 'path'" );
    try {
        Method m = variables_module.class.getDeclaredMethod("setPath", new Class[] {String.class});
        m.invoke(variables_module.class, new Object[] {"Whoa!"} );
        System.out.println( "Hey, what's going on?!?! This shouldn't work" );
    }
    catch (NoSuchMethodException e) {
        System.out.println( "Good." );
    }
    catch (Throwable t) {
        System.out.println( "You shouldn't see this!" );
    }

    System.out.println( "     Trying to set 'status'" );
    try {
        Method m = variables_module.class.getDeclaredMethod("setStatus", new Class[] {Integer.class});
        m.invoke(variables_module.class, new Object[] {new Integer(0)} );
        System.out.println( "Hey, what's going on?!?! This shouldn't work" );
    }
    catch (NoSuchMethodException e) {
        System.out.println( "Good." );
    }
    catch (Throwable t) {
        System.out.println( "You shouldn't see this!" );
    }

    System.out.println( "\nI'm going to try and update a structure variable.\n" );

    variables_module.setPt(variables_module.getPtptr());

    System.out.println( "The new value is" );
    variables_module.pt_print();
    System.out.println( "You should see the value" + variables_module.Point_print(variables_module.getPtptr()) );
  }
}
