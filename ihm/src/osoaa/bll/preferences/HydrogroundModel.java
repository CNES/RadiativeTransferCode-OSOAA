package osoaa.bll.preferences;

import java.math.BigDecimal;
import java.util.Properties;

import osoaa.bll.exception.InitException;
import osoaa.bll.properties.PropertiesManager;
import osoaa.common.string.StringUtils;
import osoaa.dal.preferences.DALHydrogroundModel;

public class HydrogroundModel implements IHydrogroundModel {

	private DALHydrogroundModel dalPreferences;

	public HydrogroundModel() throws InitException {
		dalPreferences = new DALHydrogroundModel();
	}

	@Override
	public void init() {
		// We check each properties, to check if a value already exists
		// if no value is defined, we set the value from the properties
		setHYDDirMie(getHYDDirMie());

		setHYDMieLog(getHYDMieLog());

		setHYDLog(getHYDLog());

		setPHYTOResFile(getPHYTOResFile());

		setMLPResFile(getMLPResFile());

		setHYDModel(getHYDModel());

		setPHYTOJDMRwa(getPHYTOJDMRwa());

		setPHYTOJDMIwa(getPHYTOJDMIwa());

		setPHYTOJDslope(getPHYTOJDslope());

		setPHYTOJDrmin(getPHYTOJDrmin());

		setPHYTOJDrmax(getPHYTOJDrmax());

		setPHYTOJDrate(getPHYTOJDrate());

		setSEDJDMRwa(getSEDJDMRwa());

		setSEDJDMIwa(getSEDJDMIwa());

		setSEDJDslope(getSEDJDslope());

		setSEDJDrmin(getSEDJDrmin());

		setSEDJDrmax(getSEDJDrmax());

		setSEDJDrate(getSEDJDrate());

		setHYDExtData(getHYDExtData());
	}

	@Override
	public void setHYDDirMie(String value_) {
	    dalPreferences.putProperty("HYD.DirMie", value_);
	}

	@Override
	public String getHYDDirMie() {
		return StringUtils.asStringValue(dalPreferences.getProperty("HYD.DirMie", String.class, ""));
	}

	@Override
	public void setHYDMieLog(String value_) {
	    dalPreferences.putProperty("HYD.MieLog", value_);
	}

	@Override
	public String getHYDMieLog() {
		return StringUtils.asStringValue(dalPreferences.getProperty("HYD.MieLog", String.class, ""));
	}


	@Override
	public void setHYDLog(String value_) {
	    dalPreferences.putProperty("HYD.Log", value_);
	}

	@Override
	public String getHYDLog() {
		return StringUtils.asStringValue(dalPreferences.getProperty("HYD.Log", String.class, ""));
	}


	@Override
	public void setPHYTOResFile(String value_) {
	    dalPreferences.putProperty("PHYTO.ResFile", value_);
	}

	@Override
	public String getPHYTOResFile() {
		return StringUtils.asStringValue(dalPreferences.getProperty("PHYTO.ResFile", String.class, PropertiesManager.getInstance().getCteDefaultFicgranuPhyto()));
	}


	@Override
	public void setMLPResFile(String value_) {
	    dalPreferences.putProperty("MLP.ResFile", value_);
	}

	@Override
	public String getMLPResFile() {
		return StringUtils.asStringValue(dalPreferences.getProperty("MLP.ResFile", String.class, PropertiesManager.getInstance().getCteDefaultFicgranuMlp()));
	}


	@Override
	public void setHYDModel(Integer value_) {
	    dalPreferences.putProperty("HYD.Model", value_);
	    if( value_ != null ){
	    	switch(value_){
	    	case 1:
                /*
	    		if(PreferencesFactory.getInstance().getAtmosphericAndSeaProfiles().getPhytoChl().doubleValue() <= 0.0 ){
	    			unsetPhyto();
	    		} else {
	    			setAllPhyto();
	    		} */
                setAllPhyto();

                /*
	    		if(PreferencesFactory.getInstance().getAtmosphericAndSeaProfiles().getSEDCsed().doubleValue() <= 0.0 ){
	    			unsetSED();
	    		} else {
	    			setAllSED();
	    		}
	    		*/
                setAllSED();
	    		
	    		setHYDExtData(getHYDExtData());
	    		dalPreferences.disablePreferences("HYD.ExtData");
	    		
	    		dalPreferences.enablePreferences("PHYTO.JD");
	    		dalPreferences.enablePreferences("PHYTO.LND");
	    		dalPreferences.enablePreferences("SED.JD");
	    		dalPreferences.enablePreferences("SED.LND");
	    		break;
	    		
	    	case 2:
	    		dalPreferences.enablePreferences("HYD.ExtData");
	    		
	    		dalPreferences.disablePreferences("PHYTO.JD");
	    		dalPreferences.disablePreferences("PHYTO.LND");
	    		dalPreferences.disablePreferences("SED.JD");
	    		dalPreferences.disablePreferences("SED.LND");
	    		
	    		unsetSED();
	    		unsetPhyto();
	    		
	    		break;
	    	}
	    }
	}
	
	private void setAllSED() {
		setSEDJDMRwa( getSEDJDMRwa() );
		setSEDJDMIwa( getSEDJDMIwa() );
		setSEDJDslope( getSEDJDslope() );
		setSEDJDrmin( getSEDJDrmin() );
		setSEDJDrmax( getSEDJDrmax() );
		setSEDJDrate( getSEDJDrate() );
		setSEDLNDSMMRwa ( getSEDLNDSMMRwa() );
		setSEDLNDSMMIwa ( getSEDLNDSMMIwa() );
		setSEDLNDSMSDradius( getSEDLNDSMSDradius() );
		setSEDLNDSMSDvar( getSEDLNDSMSDvar() );
		setSEDLNDSMrate( getSEDLNDSMrate() );
		setSEDLNDTMMRwa ( getSEDLNDTMMRwa() );
		setSEDLNDTMMIwa ( getSEDLNDTMMIwa() );
		setSEDLNDTMSDradius( getSEDLNDTMSDradius() );
		setSEDLNDTMSDvar( getSEDLNDTMSDvar() );
		setSEDLNDTMrate( getSEDLNDTMrate() );
	}

	public void setAllPhyto(){
		setPHYTOJDMRwa( getPHYTOJDMRwa() );
		setPHYTOJDMIwa( getPHYTOJDMIwa() );
		setPHYTOJDslope( getPHYTOJDslope() );
		setPHYTOJDrmin( getPHYTOJDrmin() );
		setPHYTOJDrmax( getPHYTOJDrmax() );
		setPHYTOJDrate( getPHYTOJDrate() );
		setPHYTOLNDSMMRwa ( getPHYTOLNDSMMRwa() );
		setPHYTOLNDSMMIwa ( getPHYTOLNDSMMIwa() );
		setPHYTOLNDSMSDradius( getPHYTOLNDSMSDradius() );
		setPHYTOLNDSMSDvar( getPHYTOLNDSMSDvar() );
		setPHYTOLNDSMrate( getPHYTOLNDSMrate() );
		setPHYTOLNDTMMRwa ( getPHYTOLNDTMMRwa() );
		setPHYTOLNDTMMIwa ( getPHYTOLNDTMMIwa() );
		setPHYTOLNDTMSDradius( getPHYTOLNDTMSDradius() );
		setPHYTOLNDTMSDvar( getPHYTOLNDTMSDvar() );
		setPHYTOLNDTMrate( getPHYTOLNDTMrate() );
	}
	
	@Override
	public void unsetPhyto(){
		setPHYTOJDMRwa(new BigDecimal("0.0"));
		setPHYTOJDMIwa(new BigDecimal("0.0"));
		setPHYTOJDslope(new BigDecimal("3.0"));
		setPHYTOJDrmin( PropertiesManager.getInstance().getCteDefaultHydJungeRmin() );
		setPHYTOJDrmax( PropertiesManager.getInstance().getCteDefaultHydJungeRmax() );
		setPHYTOJDrate(new BigDecimal("0.0"));
		
		setPHYTOLNDSMMRwa (null);
		setPHYTOLNDSMMIwa (null);
		setPHYTOLNDSMSDradius(null);
		setPHYTOLNDSMSDvar(null);
		setPHYTOLNDSMrate(null);
		setPHYTOLNDTMMRwa (null);
		setPHYTOLNDTMMIwa (null);
		setPHYTOLNDTMSDradius(null);
		setPHYTOLNDTMSDvar(null);
		setPHYTOLNDTMrate(null);
	}
	
	@Override
	public void unsetSED(){
		setSEDJDMRwa(new BigDecimal("0.0"));
		setSEDJDMIwa(new BigDecimal("0.0"));
		setSEDJDslope(new BigDecimal("3.0"));
		setSEDJDrmin( PropertiesManager.getInstance().getCteDefaultHydJungeRmin() );
		setSEDJDrmax( PropertiesManager.getInstance().getCteDefaultHydJungeRmax() );
		setSEDJDrate(new BigDecimal("0.0"));
		
		setSEDLNDSMMRwa (null);
		setSEDLNDSMMIwa (null);
		setSEDLNDSMSDradius(null);
		setSEDLNDSMSDvar(null);
		setSEDLNDSMrate(null);
		setSEDLNDTMMRwa (null);
		setSEDLNDTMMIwa (null);
		setSEDLNDTMSDradius(null);
		setSEDLNDTMSDvar(null);
		setSEDLNDTMrate(null);
	}

	@Override
	public Integer getHYDModel() {
		return (Integer)dalPreferences.getProperty("HYD.Model", Integer.class, "1");
	}


	@Override
	public void setPHYTOJDMRwa(BigDecimal value_) {
	    dalPreferences.putProperty("PHYTO.JD.MRwa", value_);
	}

	@Override
	public BigDecimal getPHYTOJDMRwa() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.JD.MRwa", BigDecimal.class, "0.0");
	}


	@Override
	public void setPHYTOJDMIwa(BigDecimal value_) {
	    dalPreferences.putProperty("PHYTO.JD.MIwa", value_);
	}

	@Override
	public BigDecimal getPHYTOJDMIwa() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.JD.MIwa", BigDecimal.class, "0.0");
	}


	@Override
	public void setPHYTOJDslope(BigDecimal value_) {
	    dalPreferences.putProperty("PHYTO.JD.slope", value_);
	}

	@Override
	public BigDecimal getPHYTOJDslope() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.JD.slope", BigDecimal.class, "3.0");
	}


	@Override
	public void setPHYTOJDrmin(BigDecimal value_) {
	    dalPreferences.putProperty("PHYTO.JD.rmin", value_);
	}

	@Override
	public BigDecimal getPHYTOJDrmin() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.JD.rmin", BigDecimal.class, PropertiesManager.getInstance().getCteDefaultHydJungeRmin().toString());
	}


	@Override
	public void setPHYTOJDrmax(BigDecimal value_) {
	    dalPreferences.putProperty("PHYTO.JD.rmax", value_);
	}

	@Override
	public BigDecimal getPHYTOJDrmax() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.JD.rmax", BigDecimal.class, PropertiesManager.getInstance().getCteDefaultHydJungeRmax().toString());
	}


	@Override
	public void setPHYTOJDrate(BigDecimal value_) {
	    dalPreferences.putProperty("PHYTO.JD.rate", value_);
	}

	@Override
	public BigDecimal getPHYTOJDrate() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.JD.rate", BigDecimal.class, "0.0");
	}


	@Override
	public void setPHYTOLNDSMMRwa(BigDecimal value_) {
	    dalPreferences.putProperty("PHYTO.LND.SM.MRwa", value_);
	}

	@Override
	public BigDecimal getPHYTOLNDSMMRwa() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.LND.SM.MRwa", BigDecimal.class, null);
	}


	@Override
	public void setPHYTOLNDSMMIwa(BigDecimal value_) {
	    dalPreferences.putProperty("PHYTO.LND.SM.MIwa", value_);
	}

	@Override
	public BigDecimal getPHYTOLNDSMMIwa() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.LND.SM.MIwa", BigDecimal.class, null);
	}


	@Override
	public void setPHYTOLNDSMSDradius(BigDecimal value_) {
	    dalPreferences.putProperty("PHYTO.LND.SM.SDradius", value_);
	}

	@Override
	public BigDecimal getPHYTOLNDSMSDradius() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.LND.SM.SDradius", BigDecimal.class, null);
	}


	@Override
	public void setPHYTOLNDSMSDvar(BigDecimal value_) {
	    dalPreferences.putProperty("PHYTO.LND.SM.SDvar", value_);
	}

	@Override
	public BigDecimal getPHYTOLNDSMSDvar() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.LND.SM.SDvar", BigDecimal.class, null);
	}


	@Override
	public void setPHYTOLNDSMrate(BigDecimal value_) {
	    dalPreferences.putProperty("PHYTO.LND.SM.rate", value_);
	}

	@Override
	public BigDecimal getPHYTOLNDSMrate() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.LND.SM.rate", BigDecimal.class, null);
	}


	@Override
	public void setPHYTOLNDTMMRwa(BigDecimal value_) {
	    dalPreferences.putProperty("PHYTO.LND.TM.MRwa", value_);
	}

	@Override
	public BigDecimal getPHYTOLNDTMMRwa() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.LND.TM.MRwa", BigDecimal.class, null);
	}


	@Override
	public void setPHYTOLNDTMMIwa(BigDecimal value_) {
	    dalPreferences.putProperty("PHYTO.LND.TM.MIwa", value_);
	}

	@Override
	public BigDecimal getPHYTOLNDTMMIwa() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.LND.TM.MIwa", BigDecimal.class, null);
	}


	@Override
	public void setPHYTOLNDTMSDradius(BigDecimal value_) {
	    dalPreferences.putProperty("PHYTO.LND.TM.SDradius", value_);
	}

	@Override
	public BigDecimal getPHYTOLNDTMSDradius() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.LND.TM.SDradius", BigDecimal.class, null);
	}


	@Override
	public void setPHYTOLNDTMSDvar(BigDecimal value_) {
	    dalPreferences.putProperty("PHYTO.LND.TM.SDvar", value_);
	}

	@Override
	public BigDecimal getPHYTOLNDTMSDvar() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.LND.TM.SDvar", BigDecimal.class, null);
	}


	@Override
	public void setPHYTOLNDTMrate(BigDecimal value_) {
	    dalPreferences.putProperty("PHYTO.LND.TM.rate", value_);
	}

	@Override
	public BigDecimal getPHYTOLNDTMrate() {
		return (BigDecimal)dalPreferences.getProperty("PHYTO.LND.TM.rate", BigDecimal.class, null);
	}


	@Override
	public void setSEDJDMRwa(BigDecimal value_) {
	    dalPreferences.putProperty("SED.JD.MRwa", value_);
	}

	@Override
	public BigDecimal getSEDJDMRwa() {
		return (BigDecimal)dalPreferences.getProperty("SED.JD.MRwa", BigDecimal.class, "0.0");
	}


	@Override
	public void setSEDJDMIwa(BigDecimal value_) {
	    dalPreferences.putProperty("SED.JD.MIwa", value_);
	}

	@Override
	public BigDecimal getSEDJDMIwa() {
		return (BigDecimal)dalPreferences.getProperty("SED.JD.MIwa", BigDecimal.class, "0.0");
	}


	@Override
	public void setSEDJDslope(BigDecimal value_) {
	    dalPreferences.putProperty("SED.JD.slope", value_);
	}

	@Override
	public BigDecimal getSEDJDslope() {
		return (BigDecimal)dalPreferences.getProperty("SED.JD.slope", BigDecimal.class, "3.0");
	}


	@Override
	public void setSEDJDrmin(BigDecimal value_) {
	    dalPreferences.putProperty("SED.JD.rmin", value_);
	}

	@Override
	public BigDecimal getSEDJDrmin() {
		return (BigDecimal)dalPreferences.getProperty("SED.JD.rmin", BigDecimal.class, PropertiesManager.getInstance().getCteDefaultHydJungeRmin().toString());
	}


	@Override
	public void setSEDJDrmax(BigDecimal value_) {
	    dalPreferences.putProperty("SED.JD.rmax", value_);
	}

	@Override
	public BigDecimal getSEDJDrmax() {
		return (BigDecimal)dalPreferences.getProperty("SED.JD.rmax",BigDecimal.class,  PropertiesManager.getInstance().getCteDefaultHydJungeRmax().toString());
	}


	@Override
	public void setSEDJDrate(BigDecimal value_) {
	    dalPreferences.putProperty("SED.JD.rate", value_);
	}

	@Override
	public BigDecimal getSEDJDrate() {
		return (BigDecimal)dalPreferences.getProperty("SED.JD.rate", BigDecimal.class, "0.0");
	}


	@Override
	public void setSEDLNDSMMRwa(BigDecimal value_) {
	    dalPreferences.putProperty("SED.LND.SM.MRwa", value_);
	}

	@Override
	public BigDecimal getSEDLNDSMMRwa() {
		return (BigDecimal)dalPreferences.getProperty("SED.LND.SM.MRwa",BigDecimal.class,  null);
	}


	@Override
	public void setSEDLNDSMMIwa(BigDecimal value_) {
	    dalPreferences.putProperty("SED.LND.SM.MIwa", value_);
	}

	@Override
	public BigDecimal getSEDLNDSMMIwa() {
		return (BigDecimal)dalPreferences.getProperty("SED.LND.SM.MIwa", BigDecimal.class, null);
	}


	@Override
	public void setSEDLNDSMSDradius(BigDecimal value_) {
	    dalPreferences.putProperty("SED.LND.SM.SDradius", value_);
	}

	@Override
	public BigDecimal getSEDLNDSMSDradius() {
		return (BigDecimal)dalPreferences.getProperty("SED.LND.SM.SDradius", BigDecimal.class, null);
	}


	@Override
	public void setSEDLNDSMSDvar(BigDecimal value_) {
	    dalPreferences.putProperty("SED.LND.SM.SDvar", value_);
	}

	@Override
	public BigDecimal getSEDLNDSMSDvar() {
		return (BigDecimal)dalPreferences.getProperty("SED.LND.SM.SDvar", BigDecimal.class, null);
	}


	@Override
	public void setSEDLNDSMrate(BigDecimal value_) {
	    dalPreferences.putProperty("SED.LND.SM.rate", value_);
	}

	@Override
	public BigDecimal getSEDLNDSMrate() {
		return (BigDecimal)dalPreferences.getProperty("SED.LND.SM.rate", BigDecimal.class, null);
	}


	@Override
	public void setSEDLNDTMMRwa(BigDecimal value_) {
	    dalPreferences.putProperty("SED.LND.TM.MRwa", value_);
	}

	@Override
	public BigDecimal getSEDLNDTMMRwa() {
		return (BigDecimal)dalPreferences.getProperty("SED.LND.TM.MRwa", BigDecimal.class, null);
	}


	@Override
	public void setSEDLNDTMMIwa(BigDecimal value_) {
	    dalPreferences.putProperty("SED.LND.TM.MIwa", value_);
	}

	@Override
	public BigDecimal getSEDLNDTMMIwa() {
		return (BigDecimal)dalPreferences.getProperty("SED.LND.TM.MIwa", BigDecimal.class, null);
	}


	@Override
	public void setSEDLNDTMSDradius(BigDecimal value_) {
	    dalPreferences.putProperty("SED.LND.TM.SDradius", value_);
	}

	@Override
	public BigDecimal getSEDLNDTMSDradius() {
		return (BigDecimal)dalPreferences.getProperty("SED.LND.TM.SDradius", BigDecimal.class, null);
	}


	@Override
	public void setSEDLNDTMSDvar(BigDecimal value_) {
	    dalPreferences.putProperty("SED.LND.TM.SDvar", value_);
	}

	@Override
	public BigDecimal getSEDLNDTMSDvar() {
		return (BigDecimal)dalPreferences.getProperty("SED.LND.TM.SDvar", BigDecimal.class, null);
	}


	@Override
	public void setSEDLNDTMrate(BigDecimal value_) {
	    dalPreferences.putProperty("SED.LND.TM.rate", value_);
	}

	@Override
	public BigDecimal getSEDLNDTMrate() {
		return (BigDecimal)dalPreferences.getProperty("SED.LND.TM.rate", BigDecimal.class, null);
	}


	@Override
	public void setHYDExtData(String value_) {
	    dalPreferences.putProperty("HYD.ExtData", value_);
	}

	@Override
	public String getHYDExtData() {
		return StringUtils.asStringValue(dalPreferences.getProperty("HYD.ExtData", String.class, ""));
	}

	@Override
	public Properties getAllCommandProperties() {
		return dalPreferences.getAllCommandProperties();
	}

	@Override
	public void reset() throws InitException {
		dalPreferences.reset();
		init();
	}

}