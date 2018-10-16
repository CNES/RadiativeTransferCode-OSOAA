package osoaa.common.properties;

import org.apache.log4j.Logger;

import java.util.Enumeration;
import java.util.Properties;

public class PropertiesUtils {

    private static Logger log = Logger.getLogger("osoaa.common.properties.PropertiesUtils");

	 /**
	   * Copy a set of properties from one Property to another.
	   * <p>
	   *
	   * @param src_prop  Source set of properties to copy from.
	   * @param dest_prop Dest Properties to copy into.
	   *
	   **/
	  public static void copyProperties(Properties src_prop, Properties dest_prop, boolean overwrite)
	  {
	      for (Enumeration<?> propertyNames = src_prop.propertyNames();
	           propertyNames.hasMoreElements(); )
	      {
	          Object key = propertyNames.nextElement();
	          if(dest_prop.containsKey(key))
	          {
	        	  if(overwrite)
	        	  {
                      if(!(src_prop.get(key).equals(dest_prop.get(key))))
                      {
                          String message = "Key : [" + key + "] + New: [" + src_prop.get(key) + "] + Previous: [" + dest_prop.get(key) + "]";
                          log.warn(message);
                          dest_prop.put(key, src_prop.get(key));
                      }
	        	  }
	          }
	          else
	          {
	        	  dest_prop.put(key, src_prop.get(key));
	          }
	      }
	  }
}
