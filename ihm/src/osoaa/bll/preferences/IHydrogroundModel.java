package osoaa.bll.preferences;

import java.math.BigDecimal;
import java.util.Properties;

import osoaa.bll.exception.InitException;

public interface IHydrogroundModel {
	
	Properties getAllCommandProperties();
	
	void reset() throws InitException;
	
	void setHYDDirMie(String value_);

	String getHYDDirMie();


	void setHYDMieLog(String value_);

	String getHYDMieLog();


	void setHYDLog(String value_);

	String getHYDLog();


	void setPHYTOResFile(String value_);

	String getPHYTOResFile();


	void setMLPResFile(String value_);

	String getMLPResFile();


	void setHYDModel(Integer value_);

	Integer getHYDModel();


	void setPHYTOJDMRwa(BigDecimal value_);

	BigDecimal getPHYTOJDMRwa();


	void setPHYTOJDMIwa(BigDecimal value_);

	BigDecimal getPHYTOJDMIwa();


	void setPHYTOJDslope(BigDecimal value_);

	BigDecimal getPHYTOJDslope();


	void setPHYTOJDrmin(BigDecimal value_);

	BigDecimal getPHYTOJDrmin();


	void setPHYTOJDrmax(BigDecimal value_);

	BigDecimal getPHYTOJDrmax();


	void setPHYTOJDrate(BigDecimal value_);

	BigDecimal getPHYTOJDrate();


	void setPHYTOLNDSMMRwa(BigDecimal value_);

	BigDecimal getPHYTOLNDSMMRwa();


	void setPHYTOLNDSMMIwa(BigDecimal value_);

	BigDecimal getPHYTOLNDSMMIwa();


	void setPHYTOLNDSMSDradius(BigDecimal value_);

	BigDecimal getPHYTOLNDSMSDradius();


	void setPHYTOLNDSMSDvar(BigDecimal value_);

	BigDecimal getPHYTOLNDSMSDvar();


	void setPHYTOLNDSMrate(BigDecimal value_);

	BigDecimal getPHYTOLNDSMrate();


	void setPHYTOLNDTMMRwa(BigDecimal value_);

	BigDecimal getPHYTOLNDTMMRwa();


	void setPHYTOLNDTMMIwa(BigDecimal value_);

	BigDecimal getPHYTOLNDTMMIwa();


	void setPHYTOLNDTMSDradius(BigDecimal value_);

	BigDecimal getPHYTOLNDTMSDradius();


	void setPHYTOLNDTMSDvar(BigDecimal value_);

	BigDecimal getPHYTOLNDTMSDvar();


	void setPHYTOLNDTMrate(BigDecimal value_);

	BigDecimal getPHYTOLNDTMrate();


	void setSEDJDMRwa(BigDecimal value_);

	BigDecimal getSEDJDMRwa();


	void setSEDJDMIwa(BigDecimal value_);

	BigDecimal getSEDJDMIwa();


	void setSEDJDslope(BigDecimal value_);

	BigDecimal getSEDJDslope();


	void setSEDJDrmin(BigDecimal value_);

	BigDecimal getSEDJDrmin();


	void setSEDJDrmax(BigDecimal value_);

	BigDecimal getSEDJDrmax();


	void setSEDJDrate(BigDecimal value_);

	BigDecimal getSEDJDrate();


	void setSEDLNDSMMRwa(BigDecimal value_);

	BigDecimal getSEDLNDSMMRwa();


	void setSEDLNDSMMIwa(BigDecimal value_);

	BigDecimal getSEDLNDSMMIwa();


	void setSEDLNDSMSDradius(BigDecimal value_);

	BigDecimal getSEDLNDSMSDradius();


	void setSEDLNDSMSDvar(BigDecimal value_);

	BigDecimal getSEDLNDSMSDvar();


	void setSEDLNDSMrate(BigDecimal value_);

	BigDecimal getSEDLNDSMrate();


	void setSEDLNDTMMRwa(BigDecimal value_);

	BigDecimal getSEDLNDTMMRwa();


	void setSEDLNDTMMIwa(BigDecimal value_);

	BigDecimal getSEDLNDTMMIwa();


	void setSEDLNDTMSDradius(BigDecimal value_);

	BigDecimal getSEDLNDTMSDradius();


	void setSEDLNDTMSDvar(BigDecimal value_);

	BigDecimal getSEDLNDTMSDvar();


	void setSEDLNDTMrate(BigDecimal value_);

	BigDecimal getSEDLNDTMrate();


	void setHYDExtData(String value_);

	String getHYDExtData();

	void unsetPhyto();

	void unsetSED();

	void init();
}
