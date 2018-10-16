package osoaa.usl.common.ui.time;

public class TimeUtils {

	public final static long ONE_SECOND = 1000;
	public final static long SECONDS = 60;

	public final static long ONE_MINUTE = ONE_SECOND * 60;
	public final static long MINUTES = 60;

	public final static long ONE_HOUR = ONE_MINUTE * 60;
	public final static long HOURS = 24;

	public final static long ONE_DAY = ONE_HOUR * 24;

	private TimeUtils() {
	}

	/**
	 * converts time (in milliseconds) to human-readable format
	 * "<w> days, <x> hours, <y> minutes and (z) seconds"
	 */
	public static String millisToLongDHMS(long duration) {
		StringBuffer res = new StringBuffer();
		long temp = 0;
		if (duration >= ONE_SECOND) {
			temp = duration / ONE_DAY;
			if (temp > 0) {
				duration -= temp * ONE_DAY;
				res.append(temp).append(" day").append(temp > 1 ? "s" : "")
						.append(duration >= ONE_MINUTE ? ", " : "");
			}

			temp = duration / ONE_HOUR;
			if (temp > 0) {
				duration -= temp * ONE_HOUR;
				res.append(temp).append(" hour").append(temp > 1 ? "s" : "")
						.append(duration >= ONE_MINUTE ? ", " : "");
			}

			temp = duration / ONE_MINUTE;
			if (temp > 0) {
				duration -= temp * ONE_MINUTE;
				res.append(temp).append(" minute").append(temp > 1 ? "s" : "");
			}

			if (!res.toString().equals("") && duration >= ONE_SECOND) {
				res.append(" and ");
			}

			temp = duration / ONE_SECOND;
			if (temp > 0) {
				res.append(temp).append(" second").append(temp > 1 ? "s" : "");
			}
			return res.toString();
		} else {
			return "0 second";
		}
	}

	/**
	 * converts time (in milliseconds) to human-readable format "<dd:>hh:mm:ss"
	 */
	public static String millisToShortDHMS(long duration) {
		String res = "";
		duration /= ONE_SECOND;
		int seconds = (int) (duration % SECONDS);
		duration /= SECONDS;
		int minutes = (int) (duration % MINUTES);
		duration /= MINUTES;
		int hours = (int) (duration % HOURS);
		int days = (int) (duration / HOURS);
		if (days == 0) {
			res = String.format("%02d:%02d:%02d", hours, minutes, seconds);
		} else {
			res = String.format("%dd%02d:%02d:%02d", days, hours, minutes,
					seconds);
		}
		return res;
	}
}
