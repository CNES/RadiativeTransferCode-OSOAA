package osoaa.dal.preferences;

import osoaa.bll.exception.InitException;

public class DALAtmosphericAndSeaProfiles extends AbstractPreferences {

	public DALAtmosphericAndSeaProfiles(String fileName)
			throws InitException {
		super(fileName);
	}

    @Override
    public void putProperty(String key_, Object value_) {
        super.putProperty(key_, value_);    //To change body of overridden methods use File | Settings | File Templates.
    }
}
