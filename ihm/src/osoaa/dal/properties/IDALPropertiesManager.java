package osoaa.dal.properties;

import java.util.Properties;

import osoaa.bll.exception.InitException;

public interface IDALPropertiesManager {

	Object getProperty(String key_, Class c);
	
	Properties getAllProperties();
	
	void init() throws InitException;

    boolean containsKey(String working_copy);
}
