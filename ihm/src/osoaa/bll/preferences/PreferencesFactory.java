package osoaa.bll.preferences;

import org.apache.log4j.Logger;

import osoaa.bll.exception.InitException;

public class PreferencesFactory {
	public static PreferencesFactory s_instance;
	
	public static PreferencesFactory getInstance(){
		if(s_instance==null){
			s_instance = new PreferencesFactory();
		}
		return s_instance; 
	}
	
	private AtmosphericAndSeaProfiles atmosphericAndSeaProfiles;
	private CommonPreferences commonPreferences;
	private IAdminConfiguration adminConfiguration;
	private IAerosolsModel aerosolsModel;
	private IHydrogroundModel hydrogroundModel;
	private ISeaAndAtmosphereModel seaAndAtmosphereModel;
	private IGeometricConditionsModel geometricConditionsModel;
	private IOutputSpecificities outputSpecificities;
	
	private Logger log = Logger.getLogger(this.getClass().getCanonicalName());
	
	private PreferencesFactory() {}
	
	public void init(){
		try {
			atmosphericAndSeaProfiles = new AtmosphericAndSeaProfiles();
			aerosolsModel = new AerosolsModel();
			hydrogroundModel = new HydrogroundModel();

			seaAndAtmosphereModel = new SeaAndAtmosphereModel();
			geometricConditionsModel = new GeometricConditionsModel();
			outputSpecificities = new OutputSpecificities();
			
			commonPreferences = new CommonPreferences();
			adminConfiguration = new AdminConfiguration();
			
			commonPreferences.init();
			atmosphericAndSeaProfiles.init();
			aerosolsModel.init();
			hydrogroundModel.init();

			seaAndAtmosphereModel.init();
			geometricConditionsModel.init();
			outputSpecificities.init();
			
			
		} catch (InitException e) {
			log.error(e.getMessage(), e);
		}
	}

	public IAtmosphericAndSeaProfiles getAtmosphericAndSeaProfiles() {
		return atmosphericAndSeaProfiles; // "atmosphericAndSeaProfiles.properties"
	}

	public ICommonPreferences getCommonPreferences() {
		return commonPreferences;
	}
	
	public IAdminConfiguration getAdminConfiguration(){
		return adminConfiguration;
	}

	public IAerosolsModel getAerosolsModel() {
		return aerosolsModel;
	}

	public IHydrogroundModel getHydrogroundModel() {
		return hydrogroundModel;
	}

	public ISeaAndAtmosphereModel getSeaAndAtmosphereModel() {
		return seaAndAtmosphereModel; // "otherSeaProfiles.properties"
	}

	public IGeometricConditionsModel getGeometricConditionsModel() {
		return geometricConditionsModel;
	}

	public IOutputSpecificities getOutputSpecificities() {
		return outputSpecificities;
	}
}
