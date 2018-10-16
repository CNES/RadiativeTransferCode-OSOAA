package osoaa.common.string;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;

public class StringUtils {

	public static String asStringValue(Object obj_){
		return obj_ == null ? null : obj_.toString();
	}
	
	public static String getStackTrace(Throwable aThrowable) {
	    final Writer result = new StringWriter();
	    final PrintWriter printWriter = new PrintWriter(result);
	    aThrowable.printStackTrace(printWriter);
	    return result.toString();
	  }
}
