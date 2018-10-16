package osoaa.bll.command;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.Properties;

import org.apache.log4j.Logger;

import osoaa.bll.preferences.PreferencesFactory;
import osoaa.common.process.ProcessBuilderUtils;
import osoaa.common.properties.PropertiesUtils;
import osoaa.common.properties.SortedProperties;
import osoaa.dal.command.DALCommandManager;
import osoaa.dal.command.IDALCommandManager;

public class CommandManager implements ICommandManager  {

	private IDALCommandManager dalCommandManager;
	private Process runningProcess = null;
	
	private Logger log = Logger.getLogger(this.getClass().getCanonicalName());
	
	public CommandManager(){
		dalCommandManager = new DALCommandManager();
		init();
	}
	
	private void init(){
		//nop
	}
	
	public Properties getAllPropertiesUsed(){
		Properties allProperties = new SortedProperties();
		
		Properties atmosphericAndSeaProfilesProp = PreferencesFactory.getInstance().getAtmosphericAndSeaProfiles().getAllCommandProperties();
		PropertiesUtils.copyProperties(atmosphericAndSeaProfilesProp, allProperties, true);
		
		Properties aerosolModelProp = PreferencesFactory.getInstance().getAerosolsModel().getAllCommandProperties();
		PropertiesUtils.copyProperties(aerosolModelProp, allProperties, true);
		
		Properties hydrogroundModelProp = PreferencesFactory.getInstance().getHydrogroundModel().getAllCommandProperties();
		PropertiesUtils.copyProperties(hydrogroundModelProp, allProperties, true);

		Properties seaAndAtmosphereProp = PreferencesFactory.getInstance().getSeaAndAtmosphereModel().getAllCommandProperties();
		PropertiesUtils.copyProperties(seaAndAtmosphereProp, allProperties, true);
		
		Properties geometricProp = PreferencesFactory.getInstance().getGeometricConditionsModel().getAllCommandProperties();
		PropertiesUtils.copyProperties(geometricProp, allProperties, true);
		
		Properties outputSpecProp = PreferencesFactory.getInstance().getOutputSpecificities().getAllCommandProperties();
		PropertiesUtils.copyProperties(outputSpecProp, allProperties, true);
		
		Properties commonProp = PreferencesFactory.getInstance().getCommonPreferences().getAllCommandProperties();
		PropertiesUtils.copyProperties(commonProp, allProperties, true);
		
		return allProperties;
	}
	
	@Override
	public String getCommand(){
		return ProcessBuilderUtils.toString( getProcessBuilder() );
	}
	
	private ProcessBuilder getProcessBuilder(){
		ProcessBuilder pb = dalCommandManager.getProcessBuilder(
				PreferencesFactory.getInstance().getCommonPreferences().getWorkingAbsolutePathDir(), 
				PreferencesFactory.getInstance().getAdminConfiguration().getOsoaaExe(), 
				getAllPropertiesUsed() );

		return pb;
	}
	
	@Override
	public Process startProcess() throws IOException{
		ProcessBuilder pb = getProcessBuilder();

		StringBuilder sbu = new StringBuilder();
		for (String item : pb.command()) {
			sbu.append(item);
			sbu.append(" ");
		}

		log.debug("Running [" + sbu.toString() +"]");

		String base = PreferencesFactory.getInstance().getAdminConfiguration().getPreferences().CONF_DIR_PATH.getCanonicalPath();
		
		File f = new File(base, "lastRun.sh");
		String last_command_line = f.getCanonicalPath();
		
		runningProcess = Runtime.getRuntime().exec(last_command_line);

        // ProcessBuilder pbu = new ProcessBuilder(pb.command().toArray(new String[pb.command().size()]));
        // runningProcess = pbu.start();

		return runningProcess;
	}

	@Override
	public boolean isProcessRunning() {
		boolean isProcessRunning = false;
		if(runningProcess != null){
			try{
				runningProcess.exitValue();
			} catch(IllegalThreadStateException e_){
				return true;
			}
		}
		return isProcessRunning;
	}
}
