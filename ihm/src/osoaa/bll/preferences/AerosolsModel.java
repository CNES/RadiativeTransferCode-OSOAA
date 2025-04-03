package osoaa.bll.preferences;

import java.math.BigDecimal;
import java.util.Properties;

import osoaa.bll.domain.AERMMDSDTypeEnum;
import osoaa.bll.domain.AERSFModelEnum;
import osoaa.bll.domain.AERWMOModelEnum;
import osoaa.bll.domain.AerosolModelTypeEnum;
import osoaa.bll.exception.InitException;
import osoaa.bll.properties.PropertiesManager;
import osoaa.common.string.StringUtils;
import osoaa.dal.preferences.DALAerosolsModel;

public class AerosolsModel implements IAerosolsModel {

	private DALAerosolsModel dalPreferences;
	
	// valeur par defaut pour AER.WMO.XX, XX = DL, WS, OC ou SO
	private static final BigDecimal AER_WMO_DEFAULT =  new BigDecimal(0.25);

	public AerosolsModel() throws InitException {
		dalPreferences = new DALAerosolsModel();
	}

	@Override
	public void init() {
		// We check each properties, to check if a value already exists
		// if no value is defined, we set the value from the properties
		setAERResFile(getAERResFile());
		
//		Done with a constant value
//		if( getAERResFile() ==null){
//			setAERResFile("");
//		}
		setAERDirMie(getAERDirMie());

		setAERMieLog(getAERMieLog());

		setAERLog(getAERLog());

		setAERResFileIOP(getAERResFileIOP());

		setAERTronca(getAERTronca());

		setAERModel( getAERModel() );

		setAERMMDMRwa(getAERMMDMRwa());

        setAERMMDMIwa(getAERMMDMIwa());
		setAERMMDSDtype(getAERMMDSDtype() );
		setAERMMDLNDradius(getAERMMDLNDradius());
		setAERMMDLNDvar(getAERMMDLNDvar());
		setAERMMDJDslope(getAERMMDJDslope());
		setAERMMDJDrmin(getAERMMDJDrmin());

//		Done with a constant value
//		if( getAERMMDJDrmax() ==null){
//			setAERMMDJDrmax(0.0);
//		}
		setAERMMDMRwaref(getAERMMDMRwaref());

		setAERMMDMIwaref(getAERMMDMIwaref());

		setAERWMOModel( getAERWMOModel() );

		setAERWMODL(getAERWMODL());

		setAERWMOWS(getAERWMOWS());

		setAERWMOOC(getAERWMOOC());

		setAERWMOSO(getAERWMOSO());

		setAERSFModel( getAERSFModel() );

		setAERSFRH(getAERSFRH());

		setAERBMDVCdef(getAERBMDVCdef());

		setAERBMDCoarseVC(getAERBMDCoarseVC());

		setAERBMDFineVC(getAERBMDFineVC());

		setAERBMDRAOT(getAERBMDRAOT());

		setAERBMDCMMRwa(getAERBMDCMMRwa());

		setAERBMDCMMIwa(getAERBMDCMMIwa());

		setAERBMDCMSDradius(getAERBMDCMSDradius());

		setAERBMDCMSDvar(getAERBMDCMSDvar());

		setAERBMDCMMRwaref(getAERBMDCMMRwaref());

		setAERBMDCMMIwaref(getAERBMDCMMIwaref());

		setAERBMDFMMRwa(getAERBMDFMMRwa());

		setAERBMDFMMIwa(getAERBMDFMMIwa());

		setAERBMDFMSDradius(getAERBMDFMSDradius());

		setAERBMDFMSDvar(getAERBMDFMSDvar());

		setAERBMDFMMRwaref(getAERBMDFMMRwaref());

		setAERBMDFMMIwaref(getAERBMDFMMIwaref());

		setAERExtData(getAERExtData());


		//Must be at the end due to unset
		setAERMMDJDrmax(getAERMMDJDrmax());
	}

	@Override
	public void setAERResFile(String value_) {
	    dalPreferences.putProperty("AER.ResFile", value_);
	}

	@Override
	public String getAERResFile() {
		return StringUtils.asStringValue(dalPreferences.getProperty("AER.ResFile", String.class, PropertiesManager.getInstance().getCteDefaultFicgranuAer()));
	}


	@Override
	public void setAERDirMie(String value_) {
	    dalPreferences.putProperty("AER.DirMie", value_);
	}

	@Override
	public String getAERDirMie() {
		return StringUtils.asStringValue(dalPreferences.getProperty("AER.DirMie", String.class, ""));
	}


	@Override
	public void setAERMieLog(String value_) {
	    dalPreferences.putProperty("AER.MieLog", value_);
	}

	@Override
	public String getAERMieLog() {
		return StringUtils.asStringValue(dalPreferences.getProperty("AER.MieLog", String.class, ""));
	}


	@Override
	public void setAERLog(String value_) {
	    dalPreferences.putProperty("AER.Log", value_);
	}

	@Override
	public String getAERLog() {
		return StringUtils.asStringValue(dalPreferences.getProperty("AER.Log", String.class, ""));
	}
	
	@Override
	public void setAERResFileIOP(String value_) {
	    dalPreferences.putProperty("AER.ResFile.IOP", value_);
	}

	@Override
	public String getAERResFileIOP() {
		return StringUtils.asStringValue(dalPreferences.getProperty("AER.ResFile.IOP", String.class, ""));
	}

	@Override
	public void setAERTronca(Boolean value_) {
		int val = value_ ? 1 : 0;
	    dalPreferences.putProperty("AER.Tronca", val);
	}

	@Override
	public Boolean getAERTronca() {
		int val = (Integer)dalPreferences.getProperty("AER.Tronca", Integer.class, "1");
		return val == 1;
	}

	private void unsetFieldsAERModel(int AERModelValue_){
		switch(AERModelValue_){
		case 0:
	    	setAERMMDMRwa(null);
	    	setAERMMDMIwa(null);
	    	setAERMMDSDtype(null);
	    	setAERMMDLNDradius(null);
	    	setAERMMDLNDvar(null);
	    	
	    	setAERMMDJDslope(new BigDecimal("3.0"));
	    	setAERMMDJDrmin(null);
	    	setAERMMDJDrmax(null);
	    	setAERMMDMRwaref(null);
	    	setAERMMDMIwaref(null);
	    	
	    	break;
	    case 1:
	    	setAERWMOModel(null);
	    	setAERWMODL(null);
	    	setAERWMOWS(null);
	    	setAERWMOOC(null);
	    	setAERWMOSO(null);
	    	break;
	    case 2:	
	    	setAERSFModel(null);
	    	setAERSFRH(null);
	    	break;
	    case 3:	
	    	setAERBMDVCdef(null);
	    	setAERBMDCoarseVC(null);
	    	setAERBMDFineVC(null);
	    	setAERBMDRAOT(null);
	    	setAERBMDCMMRwa(null);
	    	setAERBMDCMMIwa(null);
	    	setAERBMDCMSDradius(null);
	    	setAERBMDCMSDvar(null);
	    	setAERBMDCMMRwaref(null);
	    	setAERBMDCMMIwaref(null);
	    	setAERBMDFMMRwa(null);
	    	setAERBMDFMMIwa(null);
	    	setAERBMDFMSDradius(null);
	    	setAERBMDFMSDvar(null);
	    	setAERBMDVCdef(null);
	    	setAERBMDFMMRwaref(null);
	    	setAERBMDFMMIwaref(null);
	    	break;
	    case 4:	
	    	setAERExtData(null);
	    	break;
	    }
	}

	@Override
	public void setAERModel(Integer value_) {
	    dalPreferences.putProperty("AER.Model", value_);
	    switch(value_){
	    case 0:
	    	// unsetFieldsAERModel(1);
	    	// unsetFieldsAERModel(2);
	    	// unsetFieldsAERModel(3);
	    	// unsetFieldsAERModel(4);
	    	
	    	dalPreferences.enablePreferences("AER.MMD");
	    	dalPreferences.disablePreferences("AER.WMO");
	    	dalPreferences.disablePreferences("AER.SF");
	    	dalPreferences.disablePreferences("AER.BMD");
	    	dalPreferences.disablePreferences("AER.ExtData");
	    	break;
	    	
	    case 1:
            // unsetFieldsAERModel(0);
            // unsetFieldsAERModel(2);
            // unsetFieldsAERModel(3);
            // unsetFieldsAERModel(4);
	    	
	    	dalPreferences.disablePreferences("AER.MMD");
	    	dalPreferences.enablePreferences("AER.WMO");
	    	dalPreferences.disablePreferences("AER.SF");
	    	dalPreferences.disablePreferences("AER.BMD");
	    	dalPreferences.disablePreferences("AER.ExtData");
	    	break;
	    	
	    case 2:
            // unsetFieldsAERModel(0);
            // unsetFieldsAERModel(1);
            // unsetFieldsAERModel(3);
            // unsetFieldsAERModel(4);
	    	
	    	dalPreferences.disablePreferences("AER.MMD");
	    	dalPreferences.disablePreferences("AER.WMO");
	    	dalPreferences.enablePreferences("AER.SF");
	    	dalPreferences.disablePreferences("AER.BMD");
	    	dalPreferences.disablePreferences("AER.ExtData");
	    	
	    	
	    	break;
	    case 3:
            // unsetFieldsAERModel(0);
            // unsetFieldsAERModel(2);
            // unsetFieldsAERModel(3);
            // unsetFieldsAERModel(4);
	    	
	    	dalPreferences.disablePreferences("AER.MMD");
	    	dalPreferences.disablePreferences("AER.WMO");
	    	dalPreferences.disablePreferences("AER.SF");
	    	dalPreferences.enablePreferences("AER.BMD");
	    	dalPreferences.disablePreferences("AER.ExtData");
	    	break;
	    case 4:
            // unsetFieldsAERModel(0);
            // unsetFieldsAERModel(1);
            // unsetFieldsAERModel(2);
            // unsetFieldsAERModel(3);
	    	
	    	dalPreferences.disablePreferences("AER.MMD");
	    	dalPreferences.disablePreferences("AER.WMO");
	    	dalPreferences.disablePreferences("AER.SF");
	    	dalPreferences.disablePreferences("AER.BMD");
	    	dalPreferences.enablePreferences("AER.ExtData");
	    	break;
	    }
	}

	@Override
	public Integer getAERModel() {
        Integer num = AerosolModelTypeEnum.SHETTLE_AND_FENN_BI_MODAL.ordinal();
		return (Integer)dalPreferences.getProperty("AER.Model", Integer.class, num.toString());
	}


	@Override
	public void setAERMMDMRwa(BigDecimal value_) {
	    dalPreferences.putProperty("AER.MMD.MRwa", value_);
	}

	@Override
	public BigDecimal getAERMMDMRwa() {
		return (BigDecimal)dalPreferences.getProperty("AER.MMD.MRwa", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERMMDMIwa(BigDecimal value_) {
	    dalPreferences.putProperty("AER.MMD.MIwa", value_);
	}

	@Override
	public BigDecimal getAERMMDMIwa() {
		return (BigDecimal)dalPreferences.getProperty("AER.MMD.MIwa", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERMMDSDtype(Integer value_) {
	    dalPreferences.putProperty("AER.MMD.SDtype", value_);
	    
	    if( value_ != null ){
		    switch(value_){
		    case 1:
		    	setAERMMDJDslope(getAERMMDJDslope());
		    	setAERMMDJDrmin(getAERMMDJDrmin());
		    	setAERMMDJDrmax(getAERMMDJDrmax());
		    	break;
		    case 2: 
		    	setAERMMDLNDradius(getAERMMDLNDradius());
		    	setAERMMDLNDvar(getAERMMDLNDvar());
		    	break;
		    }
		    
		    if( PreferencesFactory.getInstance().getCommonPreferences().getRadianceWaveLength().doubleValue()
		    	== PreferencesFactory.getInstance().getAtmosphericAndSeaProfiles().getAERWaref().doubleValue() ){
		    	setAERMMDMRwaref(getAERMMDMRwaref());
		    	setAERMMDMIwaref(getAERMMDMIwaref());
		    }
	    }
	}

	@Override
	public Integer getAERMMDSDtype() {
        Integer num = AERMMDSDTypeEnum.LOG_NORMAL_SIZE_DISTRIBUTION.ordinal()+1;
		return dalPreferences.<Integer>getProperty("AER.MMD.SDtype", Integer.class, num.toString());
	}


	@Override
	public void setAERMMDLNDradius(BigDecimal value_) {
	    dalPreferences.putProperty("AER.MMD.LNDradius", value_);
	}

	@Override
	public BigDecimal getAERMMDLNDradius() {
		return (BigDecimal)dalPreferences.getProperty("AER.MMD.LNDradius", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERMMDLNDvar(BigDecimal value_) {
	    dalPreferences.putProperty("AER.MMD.LNDvar", value_);
	}

	@Override
	public BigDecimal getAERMMDLNDvar() {
		return (BigDecimal)dalPreferences.getProperty("AER.MMD.LNDvar", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERMMDJDslope(BigDecimal value_) {
	    dalPreferences.putProperty("AER.MMD.JD.slope", value_);
	}

	@Override
	public BigDecimal getAERMMDJDslope() {
		return (BigDecimal)dalPreferences.getProperty("AER.MMD.JD.slope", BigDecimal.class, "3.0");
	}


	@Override
	public void setAERMMDJDrmin(BigDecimal value_) {
	    dalPreferences.putProperty("AER.MMD.JD.rmin", value_);
	}

	@Override
	public BigDecimal getAERMMDJDrmin() {
		return (BigDecimal)dalPreferences.getProperty("AER.MMD.JD.rmin", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERMMDJDrmax(BigDecimal value_) {
	    dalPreferences.putProperty("AER.MMD.JD.rmax", value_);
	}

	@Override
	public BigDecimal getAERMMDJDrmax() {
		return (BigDecimal)dalPreferences.getProperty("AER.MMD.JD.rmax", BigDecimal.class, PropertiesManager.getInstance().getCteDefaultAerJungeRmax().toString());
	}


	@Override
	public void setAERMMDMRwaref(BigDecimal value_) {
	    dalPreferences.putProperty("AER.MMD.MRwaref", value_);
	}

	@Override
	public BigDecimal getAERMMDMRwaref() {
		return (BigDecimal)dalPreferences.getProperty("AER.MMD.MRwaref", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERMMDMIwaref(BigDecimal value_) {
	    dalPreferences.putProperty("AER.MMD.MIwaref", value_);
	}

	@Override
	public BigDecimal getAERMMDMIwaref() {
		return (BigDecimal)dalPreferences.getProperty("AER.MMD.MIwaref", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERWMOModel(Integer value_) {
	    dalPreferences.putProperty("AER.WMO.Model", value_);
	    if(value_ != null){
		    switch( value_ ){
		    case 0:
		    case 1:
		    case 2:
		    case 3:
		    	dalPreferences.disablePreferences("AER.WMO.DL");
		    	dalPreferences.disablePreferences("AER.WMO.WS");
		    	dalPreferences.disablePreferences("AER.WMO.OC");
		    	dalPreferences.disablePreferences("AER.WMO.SO");
		    	break;
		    case 4:
		    	dalPreferences.enablePreferences("AER.WMO.DL");
		    	dalPreferences.enablePreferences("AER.WMO.WS");
		    	dalPreferences.enablePreferences("AER.WMO.OC");
		    	dalPreferences.enablePreferences("AER.WMO.SO");
		    	setAERWMODL(AER_WMO_DEFAULT);
		    	setAERWMOWS(AER_WMO_DEFAULT);
		    	setAERWMOOC(AER_WMO_DEFAULT);
		    	setAERWMOSO(AER_WMO_DEFAULT);
		    	break;
		    }
	    }
	}

	@Override
	public Integer getAERWMOModel() {
        Integer num = AERWMOModelEnum.CONTINENTAL.ordinal()+1;
		return (Integer)dalPreferences.getProperty("AER.WMO.Model", Integer.class, num.toString());
	}


	@Override
	public void setAERWMODL(BigDecimal value_) {
	    dalPreferences.putProperty("AER.WMO.DL", value_);
	}

	@Override
	public BigDecimal getAERWMODL() {
		return (BigDecimal)dalPreferences.getProperty("AER.WMO.DL", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERWMOWS(BigDecimal value_) {
	    dalPreferences.putProperty("AER.WMO.WS", value_);
	}

	@Override
	public BigDecimal getAERWMOWS() {
		return (BigDecimal)dalPreferences.getProperty("AER.WMO.WS", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERWMOOC(BigDecimal value_) {
	    dalPreferences.putProperty("AER.WMO.OC", value_);
	}

	@Override
	public BigDecimal getAERWMOOC() {
		return (BigDecimal)dalPreferences.getProperty("AER.WMO.OC", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERWMOSO(BigDecimal value_) {
	    dalPreferences.putProperty("AER.WMO.SO", value_);
	}

	@Override
	public BigDecimal getAERWMOSO() {
		return (BigDecimal)dalPreferences.getProperty("AER.WMO.SO", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERSFModel(Integer value_) {
	    dalPreferences.putProperty("AER.SF.Model", value_);
	}

	@Override
	public Integer getAERSFModel() {
        Integer num = AERSFModelEnum.TROPOSPHERIC.ordinal()+1;
		return (Integer)dalPreferences.getProperty("AER.SF.Model", Integer.class, num.toString());
	}


	@Override
	public void setAERSFRH(BigDecimal value_) {
	    dalPreferences.putProperty("AER.SF.RH", value_);
	}

	@Override
	public BigDecimal getAERSFRH() {
		return (BigDecimal)dalPreferences.getProperty("AER.SF.RH", BigDecimal.class, "70.0");
	}


	@Override
	public void setAERBMDVCdef(Integer value_) {
		if( value_ == null  ){
			dalPreferences.putProperty("AER.BMD.VCdef", value_);
		} else {
			int val = value_;
		    dalPreferences.putProperty("AER.BMD.VCdef", val);
		    switch( val ){
		    case 1:
		    	setAERBMDRAOT (getAERBMDRAOT());
		    	setAERBMDCMMRwa (getAERBMDCMMRwa());
		    	setAERBMDCMMIwa(getAERBMDCMMIwa());
		    	setAERBMDCMSDradius(getAERBMDCMSDradius());
		    	setAERBMDCMSDvar(getAERBMDCMSDvar());
		    	
		    	setAERBMDFMMRwa(getAERBMDFMMRwa());
		    	setAERBMDFMMIwa(getAERBMDFMMIwa());
		    	setAERBMDFMSDradius(getAERBMDFMSDradius());
		    	setAERBMDFMSDvar(getAERBMDFMSDvar());
		    	
		    	if( PreferencesFactory.getInstance().getCommonPreferences().getRadianceWaveLength().doubleValue()
		    	    	== PreferencesFactory.getInstance().getAtmosphericAndSeaProfiles().getAERWaref().doubleValue() ){
		    		setAERBMDFMMRwaref(getAERBMDFMMRwaref());
			    	setAERBMDFMMIwaref(getAERBMDFMMIwaref());
			    	setAERBMDCMMRwaref(getAERBMDCMMRwaref());
			    	setAERBMDCMMIwaref(getAERBMDCMMIwaref());
	    	    }
		    	
		    	
		    	break;
		    case 2:
		    	setAERBMDCoarseVC(getAERBMDCoarseVC());
		    	setAERBMDFineVC(getAERBMDFineVC());
		    	break;
		    }
		}
	}

	@Override
	public Integer getAERBMDVCdef() {
		int val = (Integer)dalPreferences.getProperty("AER.BMD.VCdef", Integer.class, "1");
        return val;
	}


	@Override
	public void setAERBMDCoarseVC(BigDecimal value_) {
	    dalPreferences.putProperty("AER.BMD.CoarseVC", value_);
	}

	@Override
	public BigDecimal getAERBMDCoarseVC() {
		return (BigDecimal)dalPreferences.getProperty("AER.BMD.CoarseVC", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERBMDFineVC(BigDecimal value_) {
	    dalPreferences.putProperty("AER.BMD.FineVC", value_);
	}

	@Override
	public BigDecimal getAERBMDFineVC() {
		return (BigDecimal)dalPreferences.getProperty("AER.BMD.FineVC", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERBMDRAOT(BigDecimal value_) {
	    dalPreferences.putProperty("AER.BMD.RAOT", value_);
	}

	@Override
	public BigDecimal getAERBMDRAOT()
	{
		return (BigDecimal)dalPreferences.getProperty("AER.BMD.RAOT", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERBMDCMMRwa(BigDecimal value_) {
	    dalPreferences.putProperty("AER.BMD.CM.MRwa", value_);
	}

	@Override
	public BigDecimal getAERBMDCMMRwa() {
		return (BigDecimal)dalPreferences.getProperty("AER.BMD.CM.MRwa", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERBMDCMMIwa(BigDecimal value_) {
	    dalPreferences.putProperty("AER.BMD.CM.MIwa", value_);
	}

	@Override
	public BigDecimal getAERBMDCMMIwa() {
		return (BigDecimal)dalPreferences.getProperty("AER.BMD.CM.MIwa", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERBMDCMSDradius(BigDecimal value_) {
	    dalPreferences.putProperty("AER.BMD.CM.SDradius", value_);
	}

	@Override
	public BigDecimal getAERBMDCMSDradius() {
		return (BigDecimal)dalPreferences.getProperty("AER.BMD.CM.SDradius", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERBMDCMSDvar(BigDecimal value_) {
	    dalPreferences.putProperty("AER.BMD.CM.SDvar", value_);
	}

	@Override
	public BigDecimal getAERBMDCMSDvar() {
		return (BigDecimal)dalPreferences.getProperty("AER.BMD.CM.SDvar", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERBMDCMMRwaref(BigDecimal value_) {
	    dalPreferences.putProperty("AER.BMD.CM.MRwaref", value_);
	}

	@Override
	public BigDecimal getAERBMDCMMRwaref() {
		return (BigDecimal)dalPreferences.getProperty("AER.BMD.CM.MRwaref", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERBMDCMMIwaref(BigDecimal value_) {
	    dalPreferences.putProperty("AER.BMD.CM.MIwaref", value_);
	}

	@Override
	public BigDecimal getAERBMDCMMIwaref() {
		return (BigDecimal)dalPreferences.getProperty("AER.BMD.CM.MIwaref", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERBMDFMMRwa(BigDecimal value_) {
	    dalPreferences.putProperty("AER.BMD.FM.MRwa", value_);
	}

	@Override
	public BigDecimal getAERBMDFMMRwa() {
		return (BigDecimal)dalPreferences.getProperty("AER.BMD.FM.MRwa",BigDecimal.class,  "0.0");
	}


	@Override
	public void setAERBMDFMMIwa(BigDecimal value_) {
	    dalPreferences.putProperty("AER.BMD.FM.MIwa", value_);
	}

	@Override
	public BigDecimal getAERBMDFMMIwa() {
		return (BigDecimal)dalPreferences.getProperty("AER.BMD.FM.MIwa", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERBMDFMSDradius(BigDecimal value_) {
	    dalPreferences.putProperty("AER.BMD.FM.SDradius", value_);
	}

	@Override
	public BigDecimal getAERBMDFMSDradius() {
		return (BigDecimal)dalPreferences.getProperty("AER.BMD.FM.SDradius", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERBMDFMSDvar(BigDecimal value_) {
	    dalPreferences.putProperty("AER.BMD.FM.SDvar", value_);
	}

	@Override
	public BigDecimal getAERBMDFMSDvar() {
		return (BigDecimal)dalPreferences.getProperty("AER.BMD.FM.SDvar", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERBMDFMMRwaref(BigDecimal value_) {
	    dalPreferences.putProperty("AER.BMD.FM.MRwaref", value_);
	}

	@Override
	public BigDecimal getAERBMDFMMRwaref() {
		return (BigDecimal)dalPreferences.getProperty("AER.BMD.FM.MRwaref", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERBMDFMMIwaref(BigDecimal value_) {
	    dalPreferences.putProperty("AER.BMD.FM.MIwaref", value_);
	}

	@Override
	public BigDecimal getAERBMDFMMIwaref() {
		return (BigDecimal)dalPreferences.getProperty("AER.BMD.FM.MIwaref", BigDecimal.class, "0.0");
	}


	@Override
	public void setAERExtData(String value_) {
	    dalPreferences.putProperty("AER.ExtData", value_);
	}

	@Override
	public String getAERExtData() {
		return dalPreferences.getProperty("AER.ExtData", String.class, "").toString();
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
