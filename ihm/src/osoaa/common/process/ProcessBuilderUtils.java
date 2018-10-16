package osoaa.common.process;

public class ProcessBuilderUtils {

	public static final String toString(ProcessBuilder pb_){
		StringBuilder sb = new StringBuilder();
		for(String s : pb_.command()){
			sb.append(s);
			sb.append(" ");
		}
		return sb.toString();
	}
}
