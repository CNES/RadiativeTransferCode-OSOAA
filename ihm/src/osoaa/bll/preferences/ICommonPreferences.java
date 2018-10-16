package osoaa.bll.preferences;

import java.math.BigDecimal;
import java.util.Properties;

public interface ICommonPreferences {

	BigDecimal getRadianceWaveLength();
	
	BigDecimal getSolarZenithAngle();
	
	String getWorkingAbsolutePathDir();

    Boolean isExpert();

    void setExpert(Boolean value_);
	
	void setRadianceWaveLength(BigDecimal value_);
	
	void setSolarZenithAngle(BigDecimal value_);
	
	void setWorkingAbsolutePathDir(String value_);

	Properties getAllCommandProperties();

	void init();

}
