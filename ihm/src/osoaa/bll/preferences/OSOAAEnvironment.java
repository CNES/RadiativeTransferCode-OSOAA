package osoaa.bll.preferences;

public class OSOAAEnvironment
{
    public static String getRoot()
    {
        String root = System.getenv("OSOAA_ROOT");
        return root;
    }

    public static String getUIHome()
    {
        String home = System.getenv("OSOAA_UI_HOME");
        return home;
    }
}
