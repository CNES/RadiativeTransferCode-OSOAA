package osoaa.usl.common.ui.forms;

import org.apache.log4j.Logger;

import java.awt.Color;

import javax.swing.JComponent;
import javax.swing.SwingUtilities;

public class FormUtils {

    private static Logger log = Logger.getLogger("osoaa.usl.common.ui.forms.FormUtils");

	public static boolean setFieldState(final boolean isAnErr_, final JComponent jc_) {
		return setFieldState(isAnErr_, jc_, Color.black);
	}
	
	/**
	 * @param isAnErr_ condition to check if the form is on err
	 * @param jc_
	 * @param defaultColor_
	 * @return Not isAnErr_
	 */
	public static boolean setFieldState(final boolean isAnErr_, final JComponent jc_, final Color defaultColor_) {
		if (jc_ != null) {
			SwingUtilities.invokeLater(new Runnable() {

				@Override
				public void run() {
                    try
                    {
					jc_.setForeground(isAnErr_ ? Color.red : defaultColor_);
                    }
                    catch (Exception ex)
                    {
                        log.error(ex);
                    }
				}
			});
		}
		return ! isAnErr_;
	}


}
