package osoaa.dal.properties;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.Properties;
import java.util.Scanner;

import osoaa.bll.exception.InitException;
import osoaa.bll.preferences.OSOAAEnvironment;
import osoaa.common.properties.SortedProperties;
import osoaa.common.types.WithTypeExtractor;

public class DALPropertiesManager implements IDALPropertiesManager {

	private static final String OSOAA_H_FILE_NAME = "OSOAA.h";
	private Properties properties;

    public boolean containsKey(String key)
    {
        return properties.containsKey(key);
    }
	
	@Override
	public Object getProperty(String key_, Class c){
		
		String returnValue = properties.getProperty(key_, null);
		if(returnValue == null)
		{
			if(c.isAssignableFrom(Double.class))
			{
				Double d = new Double("0.0");
				return c.cast(d);
			}
			
			if(c.isAssignableFrom(Integer.class))
			{
				Integer d = new Integer("0");
				return c.cast(d);
			}
			
			if(c.isAssignableFrom(Boolean.class))
			{
				Boolean d = Boolean.FALSE;
				return c.cast(d);
			}
			
			return c.cast("");
		}
		
		return WithTypeExtractor.extractObjectType( c, returnValue );
	}
	
	private String checkEnvVarOSOAARoot() throws InitException{
		String OSOAA_ROOTEnvVar = OSOAAEnvironment.getRoot();
		if( OSOAA_ROOTEnvVar == null || OSOAA_ROOTEnvVar.length()<=0 ){
			throw new InitException("Env var not set : OSOAA_ROOT");
		}
		return OSOAA_ROOTEnvVar;
	}
	
	private File checkFileOSOAAheaderFile(String OSOAA_ROOTEnvVar_) throws InitException{
		File osoaaHFile = new File(
				OSOAA_ROOTEnvVar_ + File.separator 
				+ "inc" + File.separator + OSOAA_H_FILE_NAME );
		if( ! osoaaHFile.exists() ){
			throw new InitException("File not found : " + osoaaHFile.getAbsolutePath() );
		}
		return osoaaHFile;
	}
	
	@Override
	public void init() throws InitException {
		properties = new SortedProperties();
		
		String OSOAA_ROOTEnvVar = checkEnvVarOSOAARoot();
		File osoaaHFile = checkFileOSOAAheaderFile( OSOAA_ROOTEnvVar );
		
		Scanner scanner = null;
		try {
			scanner = new Scanner( new FileInputStream(osoaaHFile) );
		      while (scanner.hasNextLine()){
		    	String line = scanner.nextLine();
		        String [] keyValAr = appendPropertyFromLine( line );
		        onScannedLine(line, keyValAr!=null?keyValAr[0]:null, keyValAr!=null?keyValAr[1]:null);
		      }
		} catch (FileNotFoundException e_) {
			throw new InitException(e_);
		} finally{
			if(scanner!=null){
				scanner.close();
			}
	    }
	}
	
	/**
	 * @param line_ current read line
	 * @param key_ null if not found
	 * @param value_ null if not found
	 */
	protected void onScannedLine(String line_, String key_,String value_) {
        // nothing to do...
	}

	private String [] appendPropertyFromLine(String str_){
		String [] propertyAr = null;
		if( str_ != null 
			&& str_.length()>0 
			&& str_.startsWith("#define ")){
			String [] strAr = str_.split(" ");
			if ( strAr.length == 3 ){
				String propName = strAr[1];
				String propValue = strAr[2];
				if( propValue.startsWith("\"") ){
					propValue=propValue.substring(1);
				}
				if( propValue.endsWith("\"") ){
					propValue=propValue.substring(0, propValue.length()-1);
				}
				propertyAr = new String[2];
				propertyAr[0] = propName;
				propertyAr[1] = propValue;
				
				properties.put(propName, WithTypeExtractor.extractObjectType(String.class, propValue) );
			}
		}
		return propertyAr;
	}

	@Override
	public Properties getAllProperties() {
		return properties;
	}
	
	

}
