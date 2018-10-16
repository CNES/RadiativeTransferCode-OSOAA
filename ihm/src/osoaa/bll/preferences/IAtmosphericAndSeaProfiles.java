package osoaa.bll.preferences;

import java.math.BigDecimal;
import java.util.Properties;

import osoaa.bll.exception.InitException;

public interface IAtmosphericAndSeaProfiles {
	
	Properties getAllCommandProperties();
	
	String getCteDefaultFicprofilAtmRes();

	void setCteDefaultFicprofilAtmRes(String value_);

	String getCteDefaultFicprofilSeaRes();

	void setCteDefaultFicprofilSeaRes(String value_);

	Integer getCteStdPressure();

	void setCteStdPressure(Integer value_);

	BigDecimal getCteAltToa();

	void setCteAltToa(BigDecimal value_);

	Integer getCteNtAtm();

	void setCteNtAtm(Integer value_);

	Integer getCteNtSea();

	void setCteNtSea(Integer value_);

	BigDecimal getCteTransOptThickness();

	void setCteTransOptThickness(BigDecimal value_);

	Integer getCteNzMax();

	void setCteNzMax(Integer value_);

	BigDecimal getCteSeaDepthStep();

	void setCteSeaDepthStep(BigDecimal value_);

	String getCteFicEuphDepth();

	void setCteFicEuphDepth(String value_);

	String getCteFicMolSpectralData();

	void setCteFicMolSpectralData(String value_);

	String getCteFicPhytoSpectralData();

	void setCteFicPhytoSpectralData(String value_);

	Integer getCteNbwaMax();

	void setCteNbwaMax(Integer value_);

	BigDecimal getCteDefaultSpectralYs();

	void setCteDefaultSpectralYs(BigDecimal value_);

	BigDecimal getCteDefaultSpectralDet();

	void setCteDefaultSpectralDet(BigDecimal value_);

	BigDecimal getCteSeaTLimit();

	void setCteSeaTLimit(BigDecimal value_);

	void setAERAOTRef(BigDecimal aeraotRef);

	void setProfileSeaResFile(String profileSeaResFile);

	void setSeaDepth(BigDecimal seaDepth);

	void setPhytoChl(BigDecimal phytoChl);

	void setPhytoprofileType(Integer phytoprofileType);

	void setPhytoGPChlbg(BigDecimal phytoGPChlbg);

	void setPhytoGPWidth(BigDecimal phytoGPWidth);

	void setPhytoGPDeep(BigDecimal phytoGPDeep);

	void setPhytoUserFile(String phytoUserFile);

	void setAPHA(BigDecimal apha);

	void setAERWaref(BigDecimal aerWaref);

	void setAPPressure(BigDecimal apPressure);

	void setAPHR(BigDecimal aphr);

	void setAPMot(BigDecimal apMot);

	void setAtmosphericProfile(String atmosphericProfile);

	void setProfileLog(String profileLog);

	void setSeaBottomAlbedo(BigDecimal seaBottomAlbedo);

	void setSeaBottomType(Integer seaBottomType);

	void setSeaSurfAlb(BigDecimal seaSurfAlb);

	void setDETSwa(BigDecimal detSwa);

	void setYSAbs440(BigDecimal ysAbs440);

	void setYSSwa(BigDecimal ysSwa);

	void setSEDCsed(BigDecimal sedCsed);

	void setDETAbs400(BigDecimal detAbs400);
	
	
	////
	
	BigDecimal getAERAOTRef();
	
	String getProfileSeaResFile();
	BigDecimal getSeaDepth();
	BigDecimal getPhytoChl();
	Integer getPhytoprofileType();
	BigDecimal getPhytoGPChlbg();
	
	BigDecimal getPhytoGPWidth();
	
	BigDecimal getPhytoGPDeep();
	BigDecimal getAPHA();
	
	BigDecimal getAERWaref();
	BigDecimal getAPPressure();
	
	BigDecimal getAPHR();
	
	BigDecimal getAPMot();
	String getAtmosphericProfile();
	
	String getProfileLog();
	
	String getPhytoUserFile();
	
	Integer getSeaBottomType();
	
	BigDecimal getSeaSurfAlb();
	
	BigDecimal getDETSwa();
	
	BigDecimal getYSAbs440();
	
	BigDecimal getYSSwa();
	
	BigDecimal getSEDCsed();
	
	BigDecimal getDETAbs400();

	void setAPMotSelected(boolean isSelected);

    void setAPPressureSelected(boolean isSelected);

	Boolean getAPMotSelected();

    Boolean getAPPressureSelected();

	BigDecimal getSeaBottomAlbedo();

	void reset() throws InitException;

	void init();
}
