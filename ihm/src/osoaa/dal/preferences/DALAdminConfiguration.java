package osoaa.dal.preferences;

import osoaa.bll.exception.InitException;

public class DALAdminConfiguration extends AbstractPreferences {

	public DALAdminConfiguration()
			throws InitException {
		super("adminConfiguration.properties");
		setPropertiesFileComment("You can use {0} which is replaced by the environment variable OSOAA_ROOT when OSOAA is running");
	}
}
