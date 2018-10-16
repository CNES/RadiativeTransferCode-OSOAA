package osoaa.dal.preferences;

import osoaa.bll.exception.InitException;

public class DALAerosolsModel extends AbstractPreferences {

	public DALAerosolsModel()
			throws InitException {
		super("aerosolsModel.properties");
	}
}
