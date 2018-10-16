package osoaa.dal.preferences;

import osoaa.bll.exception.InitException;

public class DALHydrogroundModel extends AbstractPreferences {

	public DALHydrogroundModel()
			throws InitException {
		super("hydrogroundModel.properties");
	}
}
