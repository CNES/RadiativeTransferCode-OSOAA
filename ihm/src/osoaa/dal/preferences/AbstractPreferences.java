package osoaa.dal.preferences;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.*;

import org.apache.log4j.Logger;

import osoaa.Launcher;
import osoaa.bll.exception.InitException;
import osoaa.bll.preferences.OSOAAEnvironment;
import osoaa.common.properties.PropertiesUtils;
import osoaa.common.properties.SortedProperties;
import osoaa.common.types.WithTypeExtractor;

public abstract class AbstractPreferences {

	private String propertiesFileName;
	public static final File CONF_DIR_PATH;
	private Properties properties;
	private String propertiesFileComment = "";
	
	private Set<String> disabledPrefrences;
	
	private Logger log = Logger.getLogger(this.getClass().getCanonicalName());
	
	static {
		Logger stlog = Logger.getLogger("osoaa.dal.preferences");
		String osoaaUiHomeEnvVar = OSOAAEnvironment.getUIHome();
		
		if (null == OSOAAEnvironment.getRoot())
		{
			throw new ExceptionInInitializerError(new Exception("OSOAA_ROOT not found !"));
		}
		try
		{
		File confDir = null;
		if(osoaaUiHomeEnvVar==null || osoaaUiHomeEnvVar.trim().length()<=0)
		{
			String path = Launcher.class.getProtectionDomain().getCodeSource().getLocation().getPath();
			if(path.toLowerCase().endsWith(".jar"))
			{
				path = new File(path + "/../../conf").getCanonicalPath();
			}
			else
			{
				path = new File(path + "/../conf").getCanonicalPath();
			}
			
			confDir = new File(path);
			if( ! confDir.exists() )
			{
				confDir.mkdirs();
			}
		} else {
			confDir = new File( osoaaUiHomeEnvVar, "conf" );
			if( ! confDir.exists() ) 
			{
				confDir.mkdirs();
			}
		}
		CONF_DIR_PATH = confDir;
		stlog.debug("RUNNING DIR = " + confDir.getCanonicalPath());
		} catch (IOException e) {
			throw new ExceptionInInitializerError(e);
		}
	}
	
	public AbstractPreferences(String propertiesFileName_) throws InitException {
		propertiesFileName = propertiesFileName_;
		disabledPrefrences = new HashSet<String>();
		init();
	}
	
	public void enablePreferences(String prefix)
	{
        if(disabledPrefrences.contains(prefix))
        {
            disabledPrefrences.remove(prefix);
        }

        if(properties.containsKey("@" + prefix))
        {
            properties.remove("@" + prefix);
        }
	}
	
	public void disablePreferences(String prefix)
	{
        disabledPrefrences.add(prefix);
        this.putProperty("@" + prefix, prefix);
	}
	
	public void reset() throws InitException {
        disabledPrefrences.clear();
		properties.clear();
		File f = getPropertiesFile();
		if(f.exists()){
			f.delete();
		}
	}

	private void init() throws InitException {
		properties = new SortedProperties();
		File f = getPropertiesFile();
		try {
			if(f.exists()){
				FileInputStream fis = new FileInputStream(f);
				properties.load(fis);
				fis.close();
			} else {
				f.createNewFile();
			}
		} catch (FileNotFoundException e_) {
			throw new InitException(e_);
		} catch (IOException e_) {
			throw new InitException(e_);
		}
	}

	public <T> T getProperty(String key_, Class<T> klass, String defaultValue_){

        String returnValue = properties.getProperty(key_, defaultValue_);
		if((returnValue == null) && (defaultValue_ == null))
		{
            if(klass.isAssignableFrom(BigDecimal.class))
            {
                BigDecimal d = new BigDecimal("0.0");
                return klass.cast(d);
            }

			if(klass.isAssignableFrom(Double.class))
			{
				Double d = Double.valueOf("0.0");
				return klass.cast(d);
			}
			
			if(klass.isAssignableFrom(Integer.class))
			{
				Integer d = Integer.valueOf("0");
				return klass.cast(d);
			}
			
			if(klass.isAssignableFrom(Boolean.class))
			{
				Boolean d = Boolean.FALSE;
				return klass.cast(d);
			}
			
			return klass.cast("");
		}
        else
        {
            if(defaultValue_ != null)
            {
                if(!klass.isAssignableFrom(String.class))
                {
                    if(defaultValue_.equals(""))
                    {
                        if(returnValue == null)
                        {
                            return (T) defaultValue_;
                        }
                        else
                        {
                            if (returnValue.equals(""))
                            {
                                return (T) defaultValue_;
                            }
                        }
                    }
                }
            }
        }
		
		return WithTypeExtractor.<T>extractObjectType( klass, returnValue );
	}
	
	public Properties getAllCommandProperties() {
		Properties commandPr = new SortedProperties();
		PropertiesUtils.copyProperties(properties, commandPr, true);
		for(Object k : properties.keySet())
        {
            boolean removed = false;

            if( k.toString().startsWith("@") )
            {
                disabledPrefrences.add(k.toString().substring(1));
            }

			if( k.toString().startsWith("@") || k.toString().startsWith("CTE_") )
            {
                removed = true;
				commandPr.remove(k);
                log.warn("Removing by prefix: " + k.toString());
			}

            if(!removed)
            {
                if( commandPr.get(k) == null || commandPr.get(k).toString().trim().length()<=0)
                {
                    removed = true;
                    commandPr.remove(k);
                    log.warn("Removing by void content: " + k.toString());
                }
            }
		}

        Set<String> tbr = new HashSet<String>();

        for(Object k : commandPr.keySet())
        {
            String key = (String) k;
            if(isDisabledByPrefixes(key))
            {
                tbr.add(key);
            }
        }

        Iterator<String> it = tbr.iterator();
        while(it.hasNext())
        {
            String nextKey = it.next();
            log.warn("Removing by prefixes " + nextKey);
            commandPr.remove(nextKey);
        }

		return commandPr;
	}

    private boolean isDisabledByPrefixes(String value)
    {
        boolean disabled = false;
        Iterator<String> it = disabledPrefrences.iterator();
        while(it.hasNext())
        {
            String nextPrefix = it.next();
            if(value.startsWith(nextPrefix))
            {
                log.warn(value + " starts with " + nextPrefix);
                disabled = true;
                break;
            }
        }
        return disabled;
    }
	
	private File getPropertiesFile(){
		return new File(CONF_DIR_PATH, propertiesFileName);
	}
	
	public void removePropertiesStartingWithPrefix(String prefix)
	{
		Set<String> tbd = new HashSet<String>();
		for(Object k : properties.keySet())
		{
			String key = (String) k;
			if(key.startsWith(prefix))
			{
				tbd.add(key);
			}
		}
		
		Iterator<String> it = tbd.iterator();
		while(it.hasNext())
		{
			String nextKey = it.next();
			properties.remove(nextKey);
		}
		
		storeProperties();
	}
	
	private void storeProperties()
	{
		try {
			properties.store(
					new FileOutputStream(getPropertiesFile()), 
					"OSOAA_UI : Last save date :" + SimpleDateFormat.getDateTimeInstance().format(new Date())
					+ "\n" + getPropertiesFileComment()
					
					);
		} catch (FileNotFoundException e) {
			log.error(e.getMessage(), e);
		} catch (IOException e) {
			log.error(e.getMessage(), e);
		}
	}

    public void removeProperty(String key_)
    {
        if ( key_.startsWith("@"))
        {
            String subkey = key_.substring(1);
            if(disabledPrefrences.contains(subkey))
            {
                disabledPrefrences.remove(subkey);
            }
        }

        if( properties.containsKey(key_)){
            log.debug("Removing key: " + key_);
            properties.remove(key_);
        }
    }
	
	public void putProperty(String key_, Object value_)
    {
		if( properties.containsKey(key_)){
			properties.remove(key_);
		}

		if( value_!=null )
        {
            if(value_ instanceof BigDecimal)
            {
                DecimalFormat nf = (DecimalFormat) DecimalFormat.getInstance();
                nf.setGroupingUsed(false);

                // TODO Take into account Locale
                // String candidate =((BigDecimal) value_).toPlainString();

                String candidate = nf.format(value_);

                if(!candidate.contains("."))
                {
                    // 2 decimals by default in integers...
                    properties.put(key_, candidate + ".00");
                }
                else
                {
                    properties.put(key_, candidate);
                }
            }
            else
            {
			    properties.put(key_, value_.toString());
            }
		}
        else
        {
            log.debug("Removing key: " + key_);
            properties.remove(key_);
        }
		
		storeProperties();
	}

	public String getPropertiesFileComment() {
		return propertiesFileComment;
	}

	public void setPropertiesFileComment(String propertiesFileComment) {
		this.propertiesFileComment = propertiesFileComment;
	}
	
	
}
