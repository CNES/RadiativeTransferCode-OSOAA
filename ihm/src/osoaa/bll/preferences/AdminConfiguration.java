package osoaa.bll.preferences;

import java.io.File;
import java.text.MessageFormat;

import org.apache.log4j.Logger;
import osoaa.bll.exception.InitException;
import osoaa.dal.preferences.AbstractPreferences;
import osoaa.dal.preferences.DALAdminConfiguration;

public class AdminConfiguration implements IAdminConfiguration {

    private Logger log = Logger.getLogger(this.getClass().getCanonicalName());

	private DALAdminConfiguration dalPreferences;

	public AdminConfiguration() throws InitException {
		dalPreferences = new DALAdminConfiguration();
		
		if( getOsoaaFic() == null){
			saveOsoaaFic("{0}/fic");
		}
		
		if( getOsoaaExe() == null){
			saveOsoaaExe("{0}/exe/OSOAA_MAIN.exe");
		}
		
		if( getOsoaaLog() == null){
			saveOsoaaLog("MainLog.log");
		}
	}
	
	public AbstractPreferences getPreferences()
	{
		return dalPreferences;
	}
		
	private void saveOsoaaExe(String value_) {
		dalPreferences.putProperty("OSOAA_EXE", value_);
	}

	private void saveOsoaaLog(String value_) {
		dalPreferences.putProperty("OSOAA.Log", value_);
	}

	private void saveOsoaaFic(String value_) {
		dalPreferences.putProperty("OSOAA_FIC", value_);
	}

	private String getProperty(String key_){
		String prop = dalPreferences.<String>getProperty(key_, String.class, null);
		
		String osoaaRootEnvVar = OSOAAEnvironment.getRoot();
		return prop.equals("") ? null :
			new File( MessageFormat.format(prop, 
				osoaaRootEnvVar ) ).getAbsolutePath();
	}
	
	private String getPropertyAsFilePath(String key_){
		String prop = getProperty(key_);
		String result = prop == null ? null : new File( prop ).getAbsolutePath();

        if(result != null)
        {
            boolean fileExists = new File(result).exists();

            if(!fileExists)
            {
                if(!key_.contains("Log"))
                {
                    log.warn("File not found !! : Property [" + key_ + "] with value [" + result + "] should point to a EXISTING file");
                }
            }
        }

        return result;
	}
			
	@Override
	public String getOsoaaFic() {
		return getPropertyAsFilePath("OSOAA_FIC");
	}

	@Override
	public String getOsoaaExe() {
		return getPropertyAsFilePath("OSOAA_EXE");
	}

	@Override
	public String getOsoaaLog() {
		return getPropertyAsFilePath("OSOAA.Log");
	}
	
}