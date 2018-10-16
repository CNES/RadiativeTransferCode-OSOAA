package osoaa.bll.preferences;

import java.math.BigDecimal;
import java.util.Properties;

import osoaa.bll.exception.InitException;

public interface IAerosolsModel {
	
	Properties getAllCommandProperties();
	
	void setAERResFile(String value_);

	String getAERResFile();


	void setAERDirMie(String value_);

	String getAERDirMie();


	void setAERMieLog(String value_);

	String getAERMieLog();


	void setAERLog(String value_);

	String getAERLog();


	void setAERTronca(Boolean value_);

	Boolean getAERTronca();


	void setAERModel(Integer value_);

	Integer getAERModel();


	void setAERMMDMRwa(BigDecimal value_);

    BigDecimal getAERMMDMRwa();


	void setAERMMDMIwa(BigDecimal value_);

	BigDecimal getAERMMDMIwa();


	void setAERMMDSDtype(Integer value_);

	Integer getAERMMDSDtype();


	void setAERMMDLNDradius(BigDecimal value_);

	BigDecimal getAERMMDLNDradius();


	void setAERMMDLNDvar(BigDecimal value_);

	BigDecimal getAERMMDLNDvar();


	void setAERMMDJDslope(BigDecimal value_);

	BigDecimal getAERMMDJDslope();


	void setAERMMDJDrmin(BigDecimal value_);

	BigDecimal getAERMMDJDrmin();


	void setAERMMDJDrmax(BigDecimal value_);

	BigDecimal getAERMMDJDrmax();


	void setAERMMDMRwaref(BigDecimal value_);

	BigDecimal getAERMMDMRwaref();


	void setAERMMDMIwaref(BigDecimal value_);

	BigDecimal getAERMMDMIwaref();


	void setAERWMOModel(Integer value_);

	Integer getAERWMOModel();


	void setAERWMODL(BigDecimal value_);

	BigDecimal getAERWMODL();


	void setAERWMOWS(BigDecimal value_);

	BigDecimal getAERWMOWS();


	void setAERWMOOC(BigDecimal value_);

	BigDecimal getAERWMOOC();


	void setAERWMOSO(BigDecimal value_);

	BigDecimal getAERWMOSO();


	void setAERSFModel(Integer value_);

	Integer getAERSFModel();


	void setAERSFRH(BigDecimal value_);

	BigDecimal getAERSFRH();


	void setAERBMDVCdef(Integer value_);

	Integer getAERBMDVCdef();


	void setAERBMDCoarseVC(BigDecimal value_);

	BigDecimal getAERBMDCoarseVC();


	void setAERBMDFineVC(BigDecimal value_);

	BigDecimal getAERBMDFineVC();


	void setAERBMDRAOT(BigDecimal value_);

	BigDecimal getAERBMDRAOT();


	void setAERBMDCMMRwa(BigDecimal value_);

	BigDecimal getAERBMDCMMRwa();


	void setAERBMDCMMIwa(BigDecimal value_);

	BigDecimal getAERBMDCMMIwa();


	void setAERBMDCMSDradius(BigDecimal value_);

	BigDecimal getAERBMDCMSDradius();


	void setAERBMDCMSDvar(BigDecimal value_);

	BigDecimal getAERBMDCMSDvar();


	void setAERBMDCMMRwaref(BigDecimal value_);

	BigDecimal getAERBMDCMMRwaref();


	void setAERBMDCMMIwaref(BigDecimal value_);

	BigDecimal getAERBMDCMMIwaref();


	void setAERBMDFMMRwa(BigDecimal value_);

	BigDecimal getAERBMDFMMRwa();


	void setAERBMDFMMIwa(BigDecimal value_);

	BigDecimal getAERBMDFMMIwa();


	void setAERBMDFMSDradius(BigDecimal value_);

	BigDecimal getAERBMDFMSDradius();


	void setAERBMDFMSDvar(BigDecimal value_);

	BigDecimal getAERBMDFMSDvar();


	void setAERBMDFMMRwaref(BigDecimal value_);

	BigDecimal getAERBMDFMMRwaref();


	void setAERBMDFMMIwaref(BigDecimal value_);

	BigDecimal getAERBMDFMMIwaref();


	void setAERExtData(String value_);

	String getAERExtData();

	void reset() throws InitException;

	void init();
}
