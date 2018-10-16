package osoaa.bll.preferences;

import osoaa.dal.preferences.AbstractPreferences;

public interface IAdminConfiguration {

	String getOsoaaFic();
	String getOsoaaExe();
	String getOsoaaLog();
	AbstractPreferences getPreferences();
}
