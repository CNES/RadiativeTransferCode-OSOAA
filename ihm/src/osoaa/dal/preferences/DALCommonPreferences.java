package osoaa.dal.preferences;

import osoaa.bll.exception.InitException;

public class DALCommonPreferences extends AbstractPreferences {

	public DALCommonPreferences()
			throws InitException {
		super("common.properties");
	}
}
