package osoaa.dal.command;

import java.io.File;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Properties;

import org.apache.log4j.Logger;

public class DALCommandManager implements IDALCommandManager {
	
	private Logger log = Logger.getLogger(this.getClass().getCanonicalName());
	
	public DALCommandManager(){
	}
	
	@Override
	public ProcessBuilder getProcessBuilder(String cmdDir_, String cmd_, Properties cmdArgs_){
        log.info(cmdArgs_);

        List<String> theArgs = new ArrayList<String>();

        theArgs.add(cmd_);

        Enumeration<Object> enu = cmdArgs_.keys();
        while(enu.hasMoreElements())
        {
            String key = (String) enu.nextElement();
            theArgs.add("-" + key);
            theArgs.add(cmdArgs_.getProperty(key));
        }

        String [] processCmdAndArgsAr = theArgs.toArray(new String[]{});
		
		ProcessBuilder p = new ProcessBuilder(processCmdAndArgsAr);
		if(cmdDir_!=null){
			File fDir = new File(cmdDir_);
			if(fDir.exists()){
				p.directory(fDir);
			}
		}
		return p;
	}

}
