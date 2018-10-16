package osoaa.bll.preferences;

import java.math.BigDecimal;
import java.util.Properties;

import osoaa.bll.exception.InitException;

public interface ISeaAndAtmosphereModel {
	
	Properties getAllCommandProperties();
	
	void reset() throws InitException;
	
	void setSEADir(String value_);

	String getSEADir();


	void setSEALog(String value_);

	String getSEALog();


	void setSEAInd(BigDecimal value_);

    BigDecimal getSEAInd();


	void setSEAWind(BigDecimal value_);

    BigDecimal getSEAWind();

	void init();
}
