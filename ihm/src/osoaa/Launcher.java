package osoaa;

import java.awt.EventQueue;

import org.apache.log4j.Logger;

import osoaa.bll.preferences.PreferencesFactory;
import osoaa.bll.properties.PropertiesManager;
import osoaa.usl.MainJFrame;
import osoaa.usl.exception.JFrameException;
import java.util.Locale;

public class Launcher {

	/**
	 * @param args
	 */
	
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				Logger log = Logger.getLogger("osoaa.Launcher");
				try {
                    Locale.setDefault(new Locale("en", "UK"));
					PropertiesManager.getInstance().init();
					PreferencesFactory.getInstance().init();
					MainJFrame frame = new MainJFrame();
					frame.setVisible(true);
				} catch (Exception e_) {
					log.error(e_.getMessage(), e_);
					JFrameException jfe = new JFrameException();
					jfe.setException(e_);
					jfe.setVisible(true);
				}
			}
		});
	}

}
