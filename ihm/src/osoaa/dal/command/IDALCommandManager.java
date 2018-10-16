package osoaa.dal.command;

import java.util.Properties;

public interface IDALCommandManager {

	ProcessBuilder getProcessBuilder(String cmdDir_, String cmd_,
			Properties cmdArgs_);

}
