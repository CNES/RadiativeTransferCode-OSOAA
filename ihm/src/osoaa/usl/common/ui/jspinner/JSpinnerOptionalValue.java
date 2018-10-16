package osoaa.usl.common.ui.jspinner;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.ParseException;

import javax.swing.AbstractSpinnerModel;
import javax.swing.BoxLayout;
import javax.swing.JFormattedTextField;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JSpinner.DefaultEditor;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.DefaultFormatterFactory;
import javax.swing.text.NumberFormatter;

import org.apache.log4j.Logger;

/**
 * Create a JSpinner which allow to have no value. Null is return in case of no value.
 * 
 * @author smarty
 *
 */
public class JSpinnerOptionalValue extends JSpinner {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public JSpinnerOptionalValue() {
		this(null);
	}
	
	public void setModel(SpinnerNumberModel snm_) {
		setModel(new OptionalSpinnerNumberModel(snm_));
		setEditor(new NumberEditorOptionalValue(this));
	}

	public JSpinnerOptionalValue(SpinnerNumberModel snm_) {
		super();
		SpinnerNumberModel sm = snm_;
		if( sm == null ){
			sm = new SpinnerNumberModel();
		}
		setModel(sm);
	}
}



