package osoaa.usl.forms;

import com.jgoodies.forms.factories.DefaultComponentFactory;
import com.jgoodies.forms.factories.FormFactory;
import com.jgoodies.forms.layout.ColumnSpec;
import com.jgoodies.forms.layout.FormLayout;
import com.jgoodies.forms.layout.RowSpec;
import org.apache.log4j.Logger;
import osoaa.bll.exception.InitException;
import osoaa.bll.preferences.ISeaAndAtmosphereModel;
import osoaa.bll.preferences.PreferencesFactory;
import osoaa.bll.properties.PropertiesManager;
import osoaa.usl.common.ui.forms.FormUtils;
import osoaa.usl.common.ui.jspinner.JSpinnerRangedValue;
import osoaa.usl.common.ui.jspinner.SpinnerBigDecimalModel;

import javax.swing.*;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.MaskFormatter;
import java.math.BigDecimal;

public class SeaAndAtmosphereModelJPanel extends AbstractForm {
	/**
	 * 
	 */
	private Logger log = Logger.getLogger(this.getClass().getCanonicalName());
	
	private static final long serialVersionUID = 1L;
	
	private ISeaAndAtmosphereModel seaAndAtmosphereModel;

	
	private JTextField SEADir_textField;
	private JLabel SEAInd_desc;
	private JLabel SEADir_title;
	private JLabel SEAWind_title;
	private JTextField SEALog_textField;
	private JLabel SEAWind_desc;
	private JLabel SEALog_desc;
	private JLabel SEADir_desc;
	private JSpinner SEAWind_spinner;
	private JSpinner SEAInd_spinner;

	private JLabel SEALog_title;

	private JLabel SEAInd_title;

	public void init() {
		seaAndAtmosphereModel = PreferencesFactory.getInstance().getSeaAndAtmosphereModel();
		
		SEADir_textField.setText( seaAndAtmosphereModel.getSEADir());
		SEALog_textField.setText( seaAndAtmosphereModel.getSEALog());
		SEAInd_spinner.setValue( seaAndAtmosphereModel.getSEAInd());
		SEAWind_spinner.setValue( seaAndAtmosphereModel.getSEAWind());
		
		validateForm();
	}
	


	private void validateFormInitToFalse() {
		FormUtils.setFieldState(false, SEADir_title);
		FormUtils.setFieldState(false, SEALog_title);
		FormUtils.setFieldState(false, SEAInd_title);
		FormUtils.setFieldState(false, SEAWind_title);
	}
	private boolean validateHydrogroundModelForm() {
		validateFormInitToFalse();
		boolean isValid = true;

		isValid &= FormUtils.setFieldState(getSEADir() == null || getSEADir().trim().length()<=0, SEADir_title);
		isValid &= FormUtils.setFieldState(getSEAInd() == null, SEAInd_title);
		isValid &= FormUtils.setFieldState(getSEAWind() == null, SEAWind_title);

		return isValid;
	}

	private void validateForm() {
		validateHydrogroundModelForm();

		notifyFormValidated();
	}

	protected MaskFormatter createFormatter(String s) {
		MaskFormatter formatter = null;
		try {
			formatter = new MaskFormatter(s);
		} catch (java.text.ParseException exc) {
			log.error("formatter is bad: " + exc.getMessage());
			System.exit(-1);
		}
		return formatter;
	}

	/**
	 * Create the panel.
	 */
	public SeaAndAtmosphereModelJPanel() {
		super("Sea surface interface");
		
		FormLayout formLayout = new FormLayout(new ColumnSpec[] {
				FormFactory.RELATED_GAP_COLSPEC,
				ColumnSpec.decode("max(67dlu;default)"),
				FormFactory.RELATED_GAP_COLSPEC,
				ColumnSpec.decode("max(145dlu;default):grow"),
				FormFactory.RELATED_GAP_COLSPEC,
				FormFactory.DEFAULT_COLSPEC,
				FormFactory.RELATED_GAP_COLSPEC,
				ColumnSpec.decode("max(4dlu;default)"),
				ColumnSpec.decode("default:grow"),},
			new RowSpec[] {
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,});
		getFormFieldsPanel().setLayout(formLayout);

		SEADir_title = DefaultComponentFactory.getInstance().createLabel(
				"SEA.Dir *:");
		getFormFieldsPanel().add(SEADir_title, "2, 1, right, default");

		SEADir_textField = new JTextField();
		SEADir_textField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				saveSEADir();
				validateForm();
			}
		});
		getFormFieldsPanel().add(SEADir_textField, "4, 1, fill, default");
		SEADir_textField.setColumns(10);

		SEADir_desc = DefaultComponentFactory.getInstance()
				.createLabel("Surface files repository directory (full path)");
		SEADir_desc
				.setToolTipText("Surface files repository directory (full path)");
		getFormFieldsPanel().add(SEADir_desc, "9, 1");

		SEALog_title = new JLabel("SEA.Log :");
		SEALog_title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(SEALog_title, "2, 3, right, default");

		SEALog_textField = new JTextField();
		SEALog_textField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent arg0) {
				saveSEALog();
				validateForm();
			}
		});
		SEALog_textField.setColumns(10);
		getFormFieldsPanel().add(SEALog_textField, "4, 3, fill, default");

		SEALog_desc = DefaultComponentFactory.getInstance()
				.createLabel("Log filename of the OSOAA_SURFACE routine ");
		SEALog_desc
				.setToolTipText("Log filename of the OSOAA_SURFACE routine ");
		getFormFieldsPanel().add(SEALog_desc, "9, 3");

		SEAInd_title = new JLabel("SEA.Ind *:");
		SEAInd_title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(SEAInd_title, "2, 5, right, default");
		
		SEAInd_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"),new BigDecimal("0.00"), null, new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(SEAInd_title, SEAInd_spinner);
		SEAInd_spinner.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                saveSEAInd();
                validateForm();
            }
        });
		getFormFieldsPanel().add(SEAInd_spinner, "4, 5");

		SEAInd_desc = new JLabel("Sea water refractive index for the  simulation wavelength  ");
		SEAInd_desc
				.setToolTipText("Sea water refractive index for the  simulation wavelength ");
		getFormFieldsPanel().add(SEAInd_desc, "9, 5");

		SEAWind_title = DefaultComponentFactory.getInstance().createLabel(
				"SEA.Wind *:");
		getFormFieldsPanel().add(SEAWind_title, "2, 7, right, default");
		
		SEAWind_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.0"), new BigDecimal("0.0"), null, new BigDecimal("0.5")));
        PropertiesManager.getInstance().register(SEAWind_title, SEAWind_spinner);
		SEAWind_spinner.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                saveSEAWind();
                validateForm();
            }
        });
		getFormFieldsPanel().add(SEAWind_spinner, "4, 7");

		SEAWind_desc = new JLabel(
				"Wind velocity at sea surface (m/s)");
		SEAWind_desc
				.setToolTipText("Wind velocity at sea surface (m/s)");
		getFormFieldsPanel().add(SEAWind_desc, "9, 7");

	}
	
	@Override
	protected void onResetBtnAction() {
		int n = JOptionPane
				.showConfirmDialog(
						this,
						"This action erases all user modifications, and reload form fields\n as if the application just come to be installed.\n\nAre you sure you want to reset all fields ?",
						"Confirm reset action ?", JOptionPane.YES_NO_OPTION);
		if (n == JOptionPane.YES_OPTION) {
			try {
				PreferencesFactory.getInstance().getSeaAndAtmosphereModel().reset();
			} catch (InitException e_) {
				log.error(e_.getMessage(), e_);
			}
			init();
		}
	}
	
	protected void setSEADirVisible(boolean isVisible_) {
		SEADir_title.setVisible(isVisible_);
		SEADir_textField.setVisible(isVisible_);
		SEADir_desc.setVisible(isVisible_);
	}

	protected void setSEALogVisible(boolean isVisible_) {
		SEALog_title.setVisible(isVisible_);
		SEALog_textField.setVisible(isVisible_);
		SEALog_desc.setVisible(isVisible_);
	}

	protected void setSEAIndVisible(boolean isVisible_) {
		SEAInd_title.setVisible(isVisible_);
		SEAInd_spinner.setVisible(isVisible_);
		SEAInd_desc.setVisible(isVisible_);
	}

	protected void setSEAWindVisible(boolean isVisible_) {
		SEAWind_title.setVisible(isVisible_);
		SEAWind_spinner.setVisible(isVisible_);
		SEAWind_desc.setVisible(isVisible_);
	}
	
	public String getSEADir() {
		SEADir_textField.validate();
		return (String) SEADir_textField.getText();
	}

	private void saveSEADir(){
		if(getSEADir()!=null){
			seaAndAtmosphereModel.setSEADir( getSEADir() );
		}
	}

	public String getSEALog() {
		SEALog_textField.validate();
		return (String) SEALog_textField.getText();
	}

	private void saveSEALog(){
		if(getSEALog()!=null){
			seaAndAtmosphereModel.setSEALog( getSEALog() );
		}
	}

	public BigDecimal getSEAInd() {
		SEAInd_spinner.validate();
		return (BigDecimal) SEAInd_spinner.getValue();
	}

	private void saveSEAInd(){
		if(getSEAInd()!=null){
			seaAndAtmosphereModel.setSEAInd( getSEAInd() );
		}
	}

	public BigDecimal getSEAWind() {
		SEAWind_spinner.validate();
		return (BigDecimal) SEAWind_spinner.getValue();
	}

	private void saveSEAWind(){
		if(getSEAWind()!=null){
			seaAndAtmosphereModel.setSEAWind( getSEAWind() );
		}
	}

	@Override
	public boolean isFormValid() {
		return validateHydrogroundModelForm();
	}

	@Override
	public void checkChanges() {
		
	}

}
