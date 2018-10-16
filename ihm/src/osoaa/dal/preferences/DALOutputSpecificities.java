package osoaa.dal.preferences;

import osoaa.bll.exception.InitException;

public class DALOutputSpecificities extends AbstractPreferences {

	public DALOutputSpecificities()
			throws InitException {
		super("outputSpecificities.properties");
	}
}
