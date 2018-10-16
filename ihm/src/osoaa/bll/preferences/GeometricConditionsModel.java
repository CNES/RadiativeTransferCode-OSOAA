package osoaa.bll.preferences;

import java.util.Properties;

import osoaa.bll.exception.InitException;
import osoaa.bll.properties.PropertiesManager;
import osoaa.common.string.StringUtils;
import osoaa.dal.preferences.DALGeometricConditionsModel;

public class GeometricConditionsModel implements IGeometricConditionsModel {

	private DALGeometricConditionsModel dalPreferences;

	public GeometricConditionsModel() throws InitException {
		dalPreferences = new DALGeometricConditionsModel();
	}


	@Override
	public void init() {
		// We check each properties, to check if a value already exists
		// if no value is defined, we set the value from the properties
		setANGRadNbGauss( getANGRadNbGauss() );

		setANGRadUserAngFile(getANGRadUserAngFile());

		setANGRadResFile(getANGRadResFile() );

		setANGMieNbGauss( getANGMieNbGauss() );

		setANGMieUserAngFile(getANGMieUserAngFile());

		setANGMieResFile(getANGMieResFile());

		setANGLog(getANGLog());

	}
	
	@Override
	public void setANGRadNbGauss(Integer value_) {
	    dalPreferences.putProperty("ANG.Rad.NbGauss", value_);
	}

	@Override
	public Integer getANGRadNbGauss() {
		return (Integer)dalPreferences.getProperty("ANG.Rad.NbGauss", Integer.class, PropertiesManager.getInstance().getCteDefaultNbmuLum().toString());
	}


	@Override
	public void setANGRadUserAngFile(String value_) {
	    dalPreferences.putProperty("ANG.Rad.UserAngFile", value_);
	}

	@Override
	public String getANGRadUserAngFile() {
		return StringUtils.asStringValue(dalPreferences.getProperty("ANG.Rad.UserAngFile", String.class, ""));
	}


	@Override
	public void setANGRadResFile(String value_) {
	    dalPreferences.putProperty("ANG.Rad.ResFile", value_);
	}

	@Override
	public String getANGRadResFile() {
		return (String)dalPreferences.getProperty("ANG.Rad.ResFile", String.class, PropertiesManager.getInstance().getCteDefaultFicanglesResLum());
	}


	@Override
	public void setANGMieNbGauss(Integer value_) {
	    dalPreferences.putProperty("ANG.Mie.NbGauss", value_);
	}

	@Override
	public Integer getANGMieNbGauss() {
		return (Integer)dalPreferences.getProperty("ANG.Mie.NbGauss", Integer.class, PropertiesManager.getInstance().getCteDefaultNbmuMie().toString());
	}


	@Override
	public void setANGMieUserAngFile(String value_) {
	    dalPreferences.putProperty("ANG.Mie.UserAngFile", value_);
	}

	@Override
	public String getANGMieUserAngFile() {
		return StringUtils.asStringValue(dalPreferences.getProperty("ANG.Mie.UserAngFile", String.class, ""));
	}


	@Override
	public void setANGMieResFile(String value_) {
	    dalPreferences.putProperty("ANG.Mie.ResFile", value_);
	}

	@Override
	public String getANGMieResFile() {
		return StringUtils.asStringValue(dalPreferences.getProperty("ANG.Mie.ResFile", String.class, PropertiesManager.getInstance().getCteDefaultFicanglesResMie()));
	}


	@Override
	public void setANGLog(String value_) {
	    dalPreferences.putProperty("ANG.Log", value_);
	}

	@Override
	public String getANGLog() {
		return StringUtils.asStringValue(dalPreferences.getProperty("ANG.Log", String.class, ""));
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