package osoaa.bll.preferences;

import java.math.BigDecimal;
import java.util.Properties;

import osoaa.bll.exception.InitException;
import osoaa.dal.preferences.DALCommonPreferences;

public class CommonPreferences implements ICommonPreferences {

	private DALCommonPreferences dalPreferences;

	public CommonPreferences() throws InitException {
		dalPreferences = new DALCommonPreferences();
	}
	
	@Override
	public void init() {
		setRadianceWaveLength(getRadianceWaveLength());

		
		setSolarZenithAngle(getSolarZenithAngle());

		
		setWorkingAbsolutePathDir(getWorkingAbsolutePathDir());


        setExpert(isExpert());
	}

	@Override
	public BigDecimal getRadianceWaveLength() {
		return (BigDecimal) dalPreferences.getProperty("OSOAA.Wa", BigDecimal.class, "0.0");
	}

	@Override
	public BigDecimal getSolarZenithAngle() {
		return (BigDecimal) dalPreferences.getProperty("ANG.Thetas", BigDecimal.class, "0.0");
	}

    @Override
    public Boolean isExpert()
    {
        return (Boolean) dalPreferences.getProperty("@OSOAA.Expert", Boolean.class, "true");
    }

    @Override
    public void setExpert(Boolean value_) {
        dalPreferences.putProperty("@OSOAA.Expert", value_);
    }

    @Override
	public String getWorkingAbsolutePathDir() {
		return (String) dalPreferences.getProperty("OSOAA.ResRoot", String.class, "");
	}

	@Override
	public void setRadianceWaveLength(BigDecimal value_) {
		dalPreferences.putProperty("OSOAA.Wa", value_);
	}

	@Override
	public void setSolarZenithAngle(BigDecimal value_) {
		dalPreferences.putProperty("ANG.Thetas", value_);
	}

	@Override
	public void setWorkingAbsolutePathDir(String value_) {
		dalPreferences.putProperty("OSOAA.ResRoot", value_);
	}

	@Override
	public Properties getAllCommandProperties() {
		return dalPreferences.getAllCommandProperties();
	}

}