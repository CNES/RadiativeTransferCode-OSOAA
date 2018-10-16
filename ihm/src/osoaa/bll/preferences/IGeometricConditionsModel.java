package osoaa.bll.preferences;

import java.util.Properties;

import osoaa.bll.exception.InitException;

public interface IGeometricConditionsModel {
	
	Properties getAllCommandProperties();
	
	void reset() throws InitException;
	
	void setANGRadNbGauss(Integer value_);

	Integer getANGRadNbGauss();


	void setANGRadUserAngFile(String value_);

	String getANGRadUserAngFile();


	void setANGRadResFile(String value_);

	String getANGRadResFile();


	void setANGMieNbGauss(Integer value_);

	Integer getANGMieNbGauss();


	void setANGMieUserAngFile(String value_);

	String getANGMieUserAngFile();


	void setANGMieResFile(String value_);

	String getANGMieResFile();


	void setANGLog(String value_);

	String getANGLog();

	void init();
}
