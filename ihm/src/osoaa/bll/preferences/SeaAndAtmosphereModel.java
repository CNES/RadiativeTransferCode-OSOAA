package osoaa.bll.preferences;

import java.math.BigDecimal;
import java.util.Properties;

import osoaa.bll.exception.InitException;
import osoaa.common.string.StringUtils;
import osoaa.dal.preferences.DALAtmosphericAndSeaProfiles;

public class SeaAndAtmosphereModel implements ISeaAndAtmosphereModel {

	private DALAtmosphericAndSeaProfiles dalPreferences;

	public SeaAndAtmosphereModel() throws InitException {
		dalPreferences = new DALAtmosphericAndSeaProfiles("SEA.properties");
	}

	@Override
	public void init() {
		// We check each properties, to check if a value already exists
		// if no value is defined, we set the value from the properties
		setSEADir(getSEADir());

		setSEALog(getSEALog());

		setSEAInd(getSEAInd());

		setSEAWind(getSEAWind());
	}
	
	@Override
	public void setSEADir(String value_) {
	    dalPreferences.putProperty("SEA.Dir", value_);
	}

	@Override
	public String getSEADir() {
		return StringUtils.asStringValue(dalPreferences.getProperty("SEA.Dir", String.class, ""));
	}


	@Override
	public void setSEALog(String value_) {
	    dalPreferences.putProperty("SEA.Log", value_);
	}

	@Override
	public String getSEALog() {
		return StringUtils.asStringValue(dalPreferences.getProperty("SEA.Log", String.class, ""));
	}


	@Override
	public void setSEAInd(BigDecimal value_) {
	    dalPreferences.putProperty("SEA.Ind", value_);
	}

	@Override
	public BigDecimal getSEAInd() {
		return (BigDecimal)dalPreferences.getProperty("SEA.Ind", BigDecimal.class, "1.34");
	}


	@Override
	public void setSEAWind(BigDecimal value_) {
	    dalPreferences.putProperty("SEA.Wind", value_);
	}

	@Override
	public BigDecimal getSEAWind() {
		return (BigDecimal)dalPreferences.getProperty("SEA.Wind", BigDecimal.class, "5");
	}

	@Override
	public void reset() throws InitException {
		dalPreferences.reset();
		init();
	}
	
	@Override
	public Properties getAllCommandProperties() {
		return dalPreferences.getAllCommandProperties();
	}

}