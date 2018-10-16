package osoaa.bll.command;

import java.io.IOException;

public interface ICommandManager {

	String getCommand();

	Process startProcess() throws IOException;

	boolean isProcessRunning();

}
