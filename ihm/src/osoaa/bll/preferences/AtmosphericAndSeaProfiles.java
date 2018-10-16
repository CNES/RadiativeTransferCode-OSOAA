package osoaa.bll.preferences;

import java.math.BigDecimal;
import java.util.Properties;

import org.apache.log4j.Logger;

import osoaa.bll.exception.InitException;
import osoaa.bll.properties.PropertiesManager;
import osoaa.common.string.StringUtils;
import osoaa.dal.preferences.DALAtmosphericAndSeaProfiles;

public class AtmosphericAndSeaProfiles implements IAtmosphericAndSeaProfiles {

	private DALAtmosphericAndSeaProfiles dalPreferences;
	private Logger log = Logger.getLogger(this.getClass().getCanonicalName());

	public AtmosphericAndSeaProfiles() throws InitException {
		dalPreferences = new DALAtmosphericAndSeaProfiles("otherSeaProfiles.properties");
	}

	@Override
	public String getCteDefaultFicprofilAtmRes() {
		return dalPreferences
				.getProperty("CTE_DEFAULT_FICPROFIL_ATM_RES", String.class, PropertiesManager.getInstance().getCteDefaultFicprofilAtmRes()).toString();
	}

	@Override
	public void setCteDefaultFicprofilAtmRes(String value_) {
		dalPreferences.putProperty("CTE_DEFAULT_FICPROFIL_ATM_RES", value_);
	}

	@Override
	public String getCteDefaultFicprofilSeaRes() {
		return dalPreferences
				.getProperty("CTE_DEFAULT_FICPROFIL_SEA_RES", String.class, PropertiesManager.getInstance().getCteDefaultFicprofilSeaRes()).toString();
	}

	@Override
	public void setCteDefaultFicprofilSeaRes(String value_) {
		dalPreferences.putProperty("CTE_DEFAULT_FICPROFIL_SEA_RES", value_);
	}

	@Override
	public Integer getCteStdPressure() {
		return (Integer) dalPreferences.getProperty("CTE_STD_PRESSURE", Integer.class, PropertiesManager.getInstance().getCteStdPressure().toString());
	}

	@Override
	public void setCteStdPressure(Integer value_) {
		dalPreferences.putProperty("CTE_STD_PRESSURE", value_);
	}

	@Override
	public BigDecimal getCteAltToa() {
		return (BigDecimal) dalPreferences.getProperty("CTE_ALT_TOA", BigDecimal.class, PropertiesManager.getInstance().getCteAltToa().toString());
	}

	@Override
	public void setCteAltToa(BigDecimal value_) {
		dalPreferences.putProperty("CTE_ALT_TOA", value_);
	}

	@Override
	public Integer getCteNtAtm() {
		return (Integer) dalPreferences.getProperty("CTE_NT_ATM", Integer.class, PropertiesManager.getInstance().getCteNtAtm().toString());
	}

	@Override
	public void setCteNtAtm(Integer value_) {
		dalPreferences.putProperty("CTE_NT_ATM", value_);
	}

	@Override
	public Integer getCteNtSea() {
		return (Integer) dalPreferences.getProperty("CTE_NT_SEA", Integer.class, PropertiesManager.getInstance().getCteNtSea().toString());
	}

	@Override
	public void setCteNtSea(Integer value_) {
		dalPreferences.putProperty("CTE_NT_SEA", value_);
	}

	@Override
	public BigDecimal getCteTransOptThickness() {
		return (BigDecimal) dalPreferences.getProperty("CTE_TRANS_OPT_THICKNESS", BigDecimal.class, PropertiesManager.getInstance().getCteTransOptThickness().toString());
	}

	@Override
	public void setCteTransOptThickness(BigDecimal value_) {
		dalPreferences.putProperty("CTE_TRANS_OPT_THICKNESS", value_);
	}

	@Override
	public Integer getCteNzMax() {
		return (Integer) dalPreferences.getProperty("CTE_NZ_MAX", Integer.class, PropertiesManager.getInstance().getCteNzMax().toString());
	}

	@Override
	public void setCteNzMax(Integer value_) {
		dalPreferences.putProperty("CTE_NZ_MAX", value_);
	}

	@Override
	public BigDecimal getCteSeaDepthStep() {
		return (BigDecimal) dalPreferences.getProperty("CTE_SEA_DEPTH_STEP", BigDecimal.class, PropertiesManager.getInstance().getCteSeaDepthStep().toString());
	}

	@Override
	public void setCteSeaDepthStep(BigDecimal value_) {
		dalPreferences.putProperty("CTE_SEA_DEPTH_STEP", value_);
	}

	@Override
	public String getCteFicEuphDepth() {
		return dalPreferences.getProperty("CTE_FIC_EUPH_DEPTH", String.class, PropertiesManager.getInstance().getCteFicEuphDepth()).toString();
	}

	@Override
	public void setCteFicEuphDepth(String value_) {
		dalPreferences.putProperty("CTE_FIC_EUPH_DEPTH", value_);
	}

	@Override
	public String getCteFicMolSpectralData() {
		return dalPreferences.getProperty("CTE_FIC_MOL_SPECTRAL_DATA", String.class, PropertiesManager.getInstance().getCteFicMolSpectralData()).toString();
	}

	@Override
	public void setCteFicMolSpectralData(String value_) {
		dalPreferences.putProperty("CTE_FIC_MOL_SPECTRAL_DATA", value_);
	}

	@Override
	public String getCteFicPhytoSpectralData() {
		return dalPreferences
				.getProperty("CTE_FIC_PHYTO_SPECTRAL_DATA", String.class, PropertiesManager.getInstance().getCteFicPhytoSpectralData()).toString();
	}

	@Override
	public void setCteFicPhytoSpectralData(String value_) {
		dalPreferences.putProperty("CTE_FIC_PHYTO_SPECTRAL_DATA", value_);
	}

	@Override
	public Integer getCteNbwaMax() {
		return (Integer) dalPreferences.getProperty("CTE_NBWA_MAX", Integer.class, PropertiesManager.getInstance().getCteNbwaMax().toString());
	}

	@Override
	public void setCteNbwaMax(Integer value_) {
		dalPreferences.putProperty("CTE_NBWA_MAX", value_);
	}

	@Override
	public BigDecimal getCteDefaultSpectralYs() {
		return (BigDecimal) dalPreferences.getProperty("CTE_DEFAULT_SPECTRAL_YS", BigDecimal.class, PropertiesManager.getInstance().getCteDefaultSpectralYs().toString());
	}

	@Override
	public void setCteDefaultSpectralYs(BigDecimal value_) {
		dalPreferences.putProperty("CTE_DEFAULT_SPECTRAL_YS", value_);
	}

	@Override
	public BigDecimal getCteDefaultSpectralDet() {
		return (BigDecimal) dalPreferences.getProperty("CTE_DEFAULT_SPECTRAL_DET", BigDecimal.class, PropertiesManager.getInstance().getCteDefaultSpectralDet().toString());
	}

	@Override
	public void setCteDefaultSpectralDet(BigDecimal value_) {
		dalPreferences.putProperty("CTE_DEFAULT_SPECTRAL_DET", value_);
	}

	@Override
	public BigDecimal getCteSeaTLimit() {
		return (BigDecimal) dalPreferences.getProperty("CTE_SEA_T_LIMIT", BigDecimal.class, PropertiesManager.getInstance().getCteSeaTLimit().toString());
	}

	@Override
	public void setCteSeaTLimit(BigDecimal value_) {
		dalPreferences.putProperty("CTE_SEA_T_LIMIT", value_);
	}

	@Override
	public void init() {
		// We check each properties, to check if a value already exists
		// if no value is defined, we set the value from the properties
		setCteDefaultFicprofilAtmRes(getCteDefaultFicprofilAtmRes());

		setCteDefaultFicprofilSeaRes(getCteDefaultFicprofilSeaRes());

		setCteStdPressure(getCteStdPressure());

		setCteAltToa(getCteAltToa());

		setCteNtAtm(getCteNtAtm());


		setCteNtSea(getCteNtSea());


		setCteTransOptThickness(getCteTransOptThickness());

		setCteNzMax(getCteNzMax());

		setCteSeaDepthStep(getCteSeaDepthStep());

		setCteFicEuphDepth(getCteFicEuphDepth());

		setCteFicMolSpectralData(getCteFicMolSpectralData());

		setCteFicPhytoSpectralData(getCteFicPhytoSpectralData());

		setCteNbwaMax(getCteNbwaMax());

		setCteDefaultSpectralYs(getCteDefaultSpectralYs());

		setCteDefaultSpectralDet(getCteDefaultSpectralDet());

		setCteSeaTLimit(getCteSeaTLimit());

		//XXXXXXXXXXXXX
		
		setAERAOTRef(getAERAOTRef());

//		setAPMot(getAPMot());
		
//		Optional fields
		setAPHR(getAPHR());
		
//		setAPPressure(getAPPressure());
		
		setAERWaref(getAERWaref());

//		Optional fields
		setAPHA(getAPHA());

		setPhytoGPDeep(getPhytoGPDeep());
		
		setPhytoGPWidth(getPhytoGPWidth());

		setPhytoGPChlbg(getPhytoGPChlbg());

		
		setProfileSeaResFile(getProfileSeaResFile() );
		
		setPhytoprofileType(getPhytoprofileType());

		
		setPhytoChl(getPhytoChl());

		
		//Optional field
		setSeaDepth(getSeaDepth());

		setAtmosphericProfile(getAtmosphericProfile()  );

		setProfileLog(getProfileLog());

		setPhytoUserFile(getPhytoUserFile());
		
		setSeaBottomAlbedo(getSeaBottomAlbedo());

		// setAPMotSelected(getAPMotSelected());
		
//      setAPPressureSelected(getAPPressureSelected());
		
		setDETAbs400(getDETAbs400());

		setSEDCsed(getSEDCsed());

		setYSSwa( getYSSwa()  );

		setYSAbs440(getYSAbs440());

		setDETSwa( getDETSwa() );
		
		setSeaSurfAlb(getSeaSurfAlb());

		setSeaBottomType(getSeaBottomType());
	}

	@Override
	public void setAERAOTRef(BigDecimal aeraotRef) {
		dalPreferences.putProperty("AER.AOTref", aeraotRef);
	}

	@Override
	public void setProfileSeaResFile(String profileSeaResFile) {
		dalPreferences.putProperty("PROFILE_SEA.ResFile", profileSeaResFile);
		
	}

	@Override
	public void setSeaDepth(BigDecimal seaDepth) {
		dalPreferences.putProperty("SEA.Depth", seaDepth);
		
	}

	@Override
	public void setPhytoChl(BigDecimal phytoChl) {
		dalPreferences.putProperty("PHYTO.Chl", phytoChl);
		try{
            /*
			if(phytoChl.doubleValue() <= 0.0 && PreferencesFactory.getInstance().getHydrogroundModel().getHYDModel()==1){
				PreferencesFactory.getInstance().getHydrogroundModel().unsetPhyto();
			}
			*/
		} catch(java.lang.NullPointerException e_){
			//HydrogroundModel means not initialized
            log.warn(e_);
		}
	}

	@Override
	public void setPhytoprofileType(Integer phytoprofileType) {
		dalPreferences.putProperty("PHYTO.ProfilType", phytoprofileType);
		switch(phytoprofileType)
		{
		case 1:
			dalPreferences.disablePreferences("PHYTO.GP");
			dalPreferences.disablePreferences("PHYTO.Userfile");
			break;
		case 2:
			dalPreferences.enablePreferences("PHYTO.GP");
			dalPreferences.disablePreferences("PHYTO.Userfile");
			break;
		case 3:
			dalPreferences.disablePreferences("PHYTO.GP");
			dalPreferences.enablePreferences("PHYTO.Userfile");
			break;
		}
	}

	@Override
	public void setPhytoGPChlbg(BigDecimal phytoGPChlbg) {
		dalPreferences.putProperty("PHYTO.GP.Chlbg", phytoGPChlbg);
		
	}

	@Override
	public void setPhytoGPWidth(BigDecimal phytoGPWidth) {
		dalPreferences.putProperty("PHYTO.GP.Width", phytoGPWidth);
		
	}

	@Override
	public void setPhytoGPDeep(BigDecimal phytoGPDeep) {
		dalPreferences.putProperty("PHYTO.GP.Deep", phytoGPDeep);
		
	}

	@Override
	public void setPhytoUserFile(String phytoUserFile) {
		dalPreferences.putProperty("PHYTO.Userfile", phytoUserFile);
		
	}

	@Override
	public void setAPHA(BigDecimal apha) {
		dalPreferences.putProperty("AP.HA", apha);
		
	}

	@Override
	public void setAERWaref(BigDecimal aerWaref) {
		dalPreferences.putProperty("AER.Waref", aerWaref);
		
	}

	@Override
	public void setAPPressure(BigDecimal apPressure) {
		// dalPreferences.enablePreferences("AP.Pressure");
		// dalPreferences.disablePreferences("AP.MOT");
        dalPreferences.putProperty("@AP.MOT", true);
        dalPreferences.removeProperty("@AP.Pressure");
		dalPreferences.putProperty("AP.Pressure", apPressure);
        // this.setAPMotSelected(false);
		// dalPreferences.putProperty("AP.MOT", null);
		// dalPreferences.putProperty("AP.HR", null);
	}

	@Override
	public void setAPHR(BigDecimal aphr) {
		dalPreferences.putProperty("AP.HR", aphr);
	}

	@Override
	public void setAPMot(BigDecimal apMot) {
		// dalPreferences.enablePreferences("AP.MOT");
		// dalPreferences.disablePreferences("AP.Pressure");
        dalPreferences.putProperty("@AP.Pressure", true);
        dalPreferences.removeProperty("@AP.MOT");
		dalPreferences.putProperty("AP.MOT", apMot);
        // this.setAPMotSelected(true);
		// dalPreferences.putProperty("AP.Pressure", null);
		// dalPreferences.putProperty("AP.HR", null);
	}

	@Override
	public void setAtmosphericProfile(String atmosphericProfile) {
		dalPreferences.putProperty("PROFILE_ATM.ResFile", atmosphericProfile);
		
	}

	@Override
	public void setProfileLog(String profileLog) {
		dalPreferences.putProperty("PROFILE.Log", profileLog);
		
	}

	@Override
	public void setSeaBottomType(Integer seaBottomType) {
		dalPreferences.putProperty("SEA.BotType", seaBottomType);
		switch(seaBottomType)
		{
		case 1:
            dalPreferences.enablePreferences("SEA.BotAlb");
			break;
		default:
            dalPreferences.disablePreferences("SEA.BotAlb");
			break;
		}
	}

	@Override
	public void setSeaSurfAlb(BigDecimal seaSurfAlb) {
		dalPreferences.putProperty("SEA.SurfAlb", seaSurfAlb);
		
	}

	@Override
	public void setDETSwa(BigDecimal detSwa) {
		dalPreferences.putProperty("DET.Swa", detSwa);
		
	}

	@Override
	public void setYSAbs440(BigDecimal ysAbs440) {
		dalPreferences.putProperty("YS.Abs440", ysAbs440);
		
	}

	@Override
	public void setYSSwa(BigDecimal ysSwa) {
		dalPreferences.putProperty("YS.Swa", ysSwa);
		
	}

	@Override
	public void setSEDCsed(BigDecimal sedCsed) {
		dalPreferences.putProperty("SED.Csed", sedCsed);
		try{
            /*
			if(sedCsed.doubleValue() <= 0.0 && PreferencesFactory.getInstance().getHydrogroundModel().getHYDModel()==1){
			PreferencesFactory.getInstance().getHydrogroundModel().unsetSED();
			}
			*/
		} catch(java.lang.NullPointerException e_){
			//HydrogroundModel means not initialized
            log.warn(e_);
		}
	}

	@Override
	public void setDETAbs400(BigDecimal detAbs400) {
		dalPreferences.putProperty("DET.Abs440", detAbs400);
		
	}

	@Override
	public void setAPMotSelected(boolean apMotIsSelected) {
        dalPreferences.putProperty("@AP.Pressure", apMotIsSelected);
        dalPreferences.removeProperty("@AP.MOT");
	}

    public void setAPPressureSelected(boolean apMotIsSelected) {
        dalPreferences.putProperty("@AP.MOT", apMotIsSelected);
        dalPreferences.removeProperty("@AP.Pressure");
    }
	
	//XXXXXXXXXXXXXXXXXXXXXXXXXXX
	
	
	
	@Override
	public BigDecimal getAERAOTRef() {
		return (BigDecimal) dalPreferences.getProperty("AER.AOTref", BigDecimal.class, "0.0");
	}

	@Override
	public String getProfileSeaResFile() {
		return StringUtils.asStringValue(dalPreferences.getProperty("PROFILE_SEA.ResFile", String.class, PropertiesManager.getInstance().getCteDefaultFicprofilSeaRes()));
		
	}

	@Override
	public BigDecimal getSeaDepth() {
		return (BigDecimal)dalPreferences.getProperty("SEA.Depth", BigDecimal.class, "0.0");
	}

	@Override
	public BigDecimal getPhytoChl() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.Chl", BigDecimal.class, "0.0");
	}

	@Override
	public Integer getPhytoprofileType() {
		return (Integer)dalPreferences.getProperty("PHYTO.ProfilType", Integer.class, "1");
	}

	@Override
	public BigDecimal getPhytoGPChlbg() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.GP.Chlbg", BigDecimal.class, "0.0");
	}

	@Override
	public BigDecimal getPhytoGPWidth() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.GP.Width", BigDecimal.class, null);
	}

	@Override
	public BigDecimal getPhytoGPDeep() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.GP.Deep", BigDecimal.class, "0.0");
	}

	@Override
	public BigDecimal getAPHA() {
		return (BigDecimal)dalPreferences.getProperty("AP.HA", BigDecimal.class, "0.0");
		
	}

	@Override
	public BigDecimal getAERWaref() {
		return (BigDecimal)dalPreferences.getProperty("AER.Waref", BigDecimal.class, "0.0");
	}

	@Override
	public BigDecimal getAPPressure() {
		return (BigDecimal)dalPreferences.getProperty("AP.Pressure", BigDecimal.class, "0.0");
	}

	@Override
	public BigDecimal getAPHR() {
		return (BigDecimal)dalPreferences.getProperty("AP.HR", BigDecimal.class, "0.0");
	}

	@Override
	public BigDecimal getAPMot() {
		return (BigDecimal)dalPreferences.getProperty("AP.MOT", BigDecimal.class, "0.0");
	}

	@Override
	public String getAtmosphericProfile() {
		return StringUtils.asStringValue(dalPreferences.getProperty("PROFILE_ATM.ResFile", String.class, PropertiesManager.getInstance().getCteDefaultFicprofilAtmRes()));
	}

	@Override
	public String getProfileLog() {
		return StringUtils.asStringValue(dalPreferences.getProperty("PROFILE.Log", String.class, ""));
	}

	@Override
	public String getPhytoUserFile() {
		return StringUtils.asStringValue(dalPreferences.getProperty("PHYTO.Userfile", String.class, ""));
	}

	@Override
	public Integer getSeaBottomType() {
		return (Integer)dalPreferences.getProperty("SEA.BotType", Integer.class, "1");
	}

	@Override
	public BigDecimal getSeaSurfAlb() {
		return (BigDecimal)dalPreferences.getProperty("SEA.SurfAlb", BigDecimal.class, "0.0");
	}

	@Override
	public BigDecimal getDETSwa() {
		return (BigDecimal)dalPreferences.getProperty("DET.Swa", BigDecimal.class, PropertiesManager.getInstance().getCteDefaultSpectralDet().toString());
	}

	@Override
	public BigDecimal getYSAbs440() {
		return (BigDecimal)dalPreferences.getProperty("YS.Abs440", BigDecimal.class, "0.0");
	}

	@Override
	public BigDecimal getYSSwa() {
		return (BigDecimal)dalPreferences.getProperty("YS.Swa", BigDecimal.class, PropertiesManager.getInstance().getCteDefaultSpectralYs().toString());
	}

	@Override
	public BigDecimal getSEDCsed() {
		return (BigDecimal)dalPreferences.getProperty("SED.Csed", BigDecimal.class, "0.0");
	}

	@Override
	public BigDecimal getDETAbs400() {
		return (BigDecimal)dalPreferences.getProperty("DET.Abs440", BigDecimal.class, "0.0");
	}
	
	@Override
	public Boolean getAPMotSelected() {
		return (Boolean)dalPreferences.getProperty("@AP.MOT", Boolean.class, "true");
	}

    @Override
    public Boolean getAPPressureSelected() {
        return (Boolean)dalPreferences.getProperty("@AP.Pressure", Boolean.class, "false");
	}

	@Override
	public void setSeaBottomAlbedo(BigDecimal seaBottomAlbedo) {
		dalPreferences.putProperty("SEA.BotAlb", seaBottomAlbedo);
	}

	@Override
	public BigDecimal getSeaBottomAlbedo() {
		return (BigDecimal)dalPreferences.getProperty("SEA.BotAlb", BigDecimal.class, "0.0");
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