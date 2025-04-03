package osoaa.usl.forms;

import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.MaskFormatter;

import org.apache.log4j.Logger;

import osoaa.bll.exception.InitException;
import osoaa.bll.preferences.IGeometricConditionsModel;
import osoaa.bll.preferences.PreferencesFactory;
import osoaa.bll.properties.PropertiesManager;
import osoaa.usl.common.ui.forms.FormUtils;

import com.jgoodies.forms.factories.DefaultComponentFactory;
import com.jgoodies.forms.factories.FormFactory;
import com.jgoodies.forms.layout.ColumnSpec;
import com.jgoodies.forms.layout.FormLayout;
import com.jgoodies.forms.layout.RowSpec;
import osoaa.usl.common.ui.jspinner.JSpinnerOptionalValue;
import osoaa.usl.common.ui.jspinner.JSpinnerRangedValue;
import osoaa.usl.common.ui.jspinner.SpinnerBigDecimalModel;

import java.math.BigDecimal;

public class GeometricConditionsModelJPanel extends AbstractForm {
	/**
	 * 
	 */
	
	private Logger log = Logger.getLogger(this.getClass().getCanonicalName());
	
	private static final long serialVersionUID = 1L;
	
	private IGeometricConditionsModel geometricConditionsModel;

	
	private JTextField ANGRadResFile_textField;
	private JLabel ANGRadResFile_desc;
	private JLabel ANGRadNbGauss_title;
	private JLabel ANGMieNbGauss_title;
	private JTextField ANGRadUserAngFile_textField;
	private JLabel ANGMieNbGauss_desc;
	private JLabel ANGRadUserAngFile_desc;
	private JLabel ANGRadNbGauss_desc;
	private JSpinner ANGRadNbGauss_spinner;

	private JLabel ANGRadUserAngFile_title;

	private JLabel ANGRadResFile_title;
	private JSpinner ANGMieNbGauss_textField;
	private JTextField ANGMieUserAngFile_textField;
	private JTextField ANGMieResFile_textField;
	private JTextField ANGLog_textField;
	private JLabel ANGMieUserAngFile_desc;
	private JLabel ANGMieResFile_desc;
	private JLabel ANGLog_desc;
	private JLabel ANGMieUserAngFile_title;
	private JLabel ANGMieResFile_title;
	private JLabel ANGLog_title;

	public void init() {
		geometricConditionsModel = PreferencesFactory.getInstance().getGeometricConditionsModel();
		
		ANGRadNbGauss_spinner.setValue( geometricConditionsModel.getANGRadNbGauss());
		ANGRadUserAngFile_textField.setText( geometricConditionsModel.getANGRadUserAngFile());
		ANGRadResFile_textField.setText( geometricConditionsModel.getANGRadResFile());
		ANGMieNbGauss_textField.setValue( geometricConditionsModel.getANGMieNbGauss());
		ANGMieUserAngFile_textField.setText( geometricConditionsModel.getANGMieUserAngFile());
		ANGMieResFile_textField.setText( geometricConditionsModel.getANGMieResFile());
		ANGLog_textField.setText( geometricConditionsModel.getANGLog());
		
		validateForm();
	}
	


	private void validateFormInitToFalse() {
		FormUtils.setFieldState(false, ANGRadNbGauss_title);
		FormUtils.setFieldState(false, ANGRadUserAngFile_title);
		FormUtils.setFieldState(false, ANGRadResFile_title);
		FormUtils.setFieldState(false, ANGMieNbGauss_title);
		FormUtils.setFieldState(false, ANGMieUserAngFile_title);
		FormUtils.setFieldState(false, ANGMieResFile_title);
		FormUtils.setFieldState(false, ANGLog_title);
	}
	private boolean validateHydrogroundModelForm() {
		validateFormInitToFalse();
		boolean isValid = true;

		isValid &= FormUtils.setFieldState(getANGRadNbGauss() == null, ANGRadNbGauss_title);
		isValid &= FormUtils.setFieldState(getANGRadResFile() == null || getANGRadResFile().trim().length()<=0, ANGRadResFile_title);
		isValid &= FormUtils.setFieldState(getANGMieNbGauss() == null, ANGMieNbGauss_title);
		isValid &= FormUtils.setFieldState(getANGMieResFile() == null || getANGMieResFile().trim().length()<=0, ANGMieResFile_title);

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
	public GeometricConditionsModelJPanel() {
		super("Geometric conditions");
		
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
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,});
		getFormFieldsPanel().setLayout(formLayout);

		ANGRadNbGauss_title = DefaultComponentFactory.getInstance().createLabel(
				"ANG.Rad.NbGauss *:");
		getFormFieldsPanel().add(ANGRadNbGauss_title, "2, 1, right, default");
		
		ANGRadNbGauss_spinner = new JSpinnerRangedValue(new SpinnerNumberModel(Integer.valueOf(0), Integer.valueOf(0), null, Integer.valueOf(1)));
        PropertiesManager.getInstance().register(ANGRadNbGauss_title, ANGRadNbGauss_spinner);
		ANGRadNbGauss_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveANGRadNbGauss();
				validateForm();
			}
		});
		getFormFieldsPanel().add(ANGRadNbGauss_spinner, "4, 1");

		ANGRadNbGauss_desc = DefaultComponentFactory.getInstance()
				.createLabel("Number of Gauss angles for radiance simulation and BRDF/BPDF calculations at sea interface");
		ANGRadNbGauss_desc
				.setToolTipText("Number of Gauss angles for radiance simulation and BRDF/BPDF calculations at sea interface");
		getFormFieldsPanel().add(ANGRadNbGauss_desc, "9, 1");

		ANGRadUserAngFile_title = new JLabel("ANG.Rad.UserAngFile :");
		ANGRadUserAngFile_title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(ANGRadUserAngFile_title, "2, 3, right, default");

		ANGRadUserAngFile_textField = new JTextField();
		ANGRadUserAngFile_textField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent arg0) {
				saveANGRadUserAngFile();
				validateForm();
			}
		});
		ANGRadUserAngFile_textField.setColumns(10);
		getFormFieldsPanel().add(ANGRadUserAngFile_textField, "4, 3, fill, default");

		ANGRadUserAngFile_desc = DefaultComponentFactory.getInstance()
				.createLabel("File name of user-defined supplementary angles for radiance computations");
		ANGRadUserAngFile_desc
				.setToolTipText("File name of user-defined supplementary angles for radiance computations");
		getFormFieldsPanel().add(ANGRadUserAngFile_desc, "9, 3");

		ANGRadResFile_title = new JLabel("ANG.Rad.ResFile :");
		ANGRadResFile_title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(ANGRadResFile_title, "2, 5, right, default");
		
				ANGRadResFile_textField = new JTextField();
				ANGRadResFile_textField.addCaretListener(new CaretListener() {
					public void caretUpdate(CaretEvent e) {
						saveANGRadResFile();
						validateForm();
					}
				});
				getFormFieldsPanel().add(ANGRadResFile_textField, "4, 5, fill, default");
				ANGRadResFile_textField.setColumns(10);

		ANGRadResFile_desc = new JLabel("Output file name of angles and maximum orders of series expansion to be used for radiance calculations");
		ANGRadResFile_desc
				.setToolTipText("Output file name of angles and maximum orders of series expansion to be used for radiance calculations");
		getFormFieldsPanel().add(ANGRadResFile_desc, "9, 5");

		ANGMieNbGauss_title = DefaultComponentFactory.getInstance().createLabel(
				"ANG.Mie.NbGauss *:");
		getFormFieldsPanel().add(ANGMieNbGauss_title, "2, 7, right, default");
		
		ANGMieNbGauss_textField = new JSpinnerRangedValue(new SpinnerNumberModel(Integer.valueOf(0),Integer.valueOf(0), null, Integer.valueOf(1)));
        PropertiesManager.getInstance().register(ANGMieNbGauss_title, ANGMieNbGauss_textField);
		ANGMieNbGauss_textField.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveANGMieNbGauss();
				validateForm();
			}
		});
		getFormFieldsPanel().add(ANGMieNbGauss_textField, "4, 7, fill, default");

		ANGMieNbGauss_desc = new JLabel(
				"Number of Gauss angles for phase functions ");
		ANGMieNbGauss_desc
				.setToolTipText("Number of Gauss angles for phase functions");
		getFormFieldsPanel().add(ANGMieNbGauss_desc, "9, 7");
		
		ANGMieUserAngFile_title = new JLabel("ANG.Mie.UserAngFile :");
		ANGMieUserAngFile_title.setToolTipText("");
		getFormFieldsPanel().add(ANGMieUserAngFile_title, "2, 9, right, default");
		
		ANGMieUserAngFile_textField = new JTextField();
		ANGMieUserAngFile_textField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				saveANGMieUserAngFile();
				validateForm();
			}
		});
		ANGMieUserAngFile_textField.setColumns(10);
		getFormFieldsPanel().add(ANGMieUserAngFile_textField, "4, 9, fill, default");
		
		ANGMieUserAngFile_desc = new JLabel("File name of user-defined supplementary angles for phase functions");
		ANGMieUserAngFile_desc.setToolTipText("File name of user-defined supplementary angles for phase functions");
		getFormFieldsPanel().add(ANGMieUserAngFile_desc, "9, 9");
		
		ANGMieResFile_title = new JLabel("ANG.Mie.ResFile :");
		ANGMieResFile_title.setToolTipText("");
		getFormFieldsPanel().add(ANGMieResFile_title, "2, 11, right, default");
		
		ANGMieResFile_textField = new JTextField();
		ANGMieResFile_textField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				saveANGMieResFile();
				validateForm();
			}
		});
		ANGMieResFile_textField.setColumns(10);
		getFormFieldsPanel().add(ANGMieResFile_textField, "4, 11, fill, default");
		
		ANGMieResFile_desc = new JLabel("Output file name of angles and maximum orders of series expansion to be used for phase function calculations");
		ANGMieResFile_desc.setToolTipText("Output file name of angles and maximum orders of series expansion to be used for phase function calculations");
		getFormFieldsPanel().add(ANGMieResFile_desc, "9, 11");
		
		ANGLog_title = new JLabel("ANG.Log :");
		ANGLog_title.setToolTipText("");
		getFormFieldsPanel().add(ANGLog_title, "2, 13, right, default");
		
		ANGLog_textField = new JTextField();
		ANGLog_textField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				saveANGLog();
				validateForm();
			}
		});
		ANGLog_textField.setColumns(10);
		getFormFieldsPanel().add(ANGLog_textField, "4, 13, fill, default");
		
		ANGLog_desc = new JLabel("Name of log file for angle calculation");
		ANGLog_desc.setToolTipText("Name of log file for angle calculation");
		getFormFieldsPanel().add(ANGLog_desc, "9, 13");

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
				PreferencesFactory.getInstance().getGeometricConditionsModel().reset();
			} catch (InitException e_) {
				log.error(e_.getMessage(), e_);
			}
			init();
		}
	}

	@Override
	public boolean isFormValid() {
		return validateHydrogroundModelForm();
	}

	@Override
	public void checkChanges() {
		
	}
	
	
	protected void setANGRadNbGaussVisible(boolean isVisible_) {
		ANGRadNbGauss_title.setVisible(isVisible_);
		ANGRadNbGauss_spinner.setVisible(isVisible_);
		ANGRadNbGauss_desc.setVisible(isVisible_);
	}

	protected void setANGRadUserAngFileVisible(boolean isVisible_) {
		ANGRadUserAngFile_title.setVisible(isVisible_);
		ANGRadUserAngFile_textField.setVisible(isVisible_);
		ANGRadUserAngFile_desc.setVisible(isVisible_);
	}

	protected void setANGRadResFileVisible(boolean isVisible_) {
		ANGRadResFile_title.setVisible(isVisible_);
		ANGRadResFile_textField.setVisible(isVisible_);
		ANGRadResFile_desc.setVisible(isVisible_);
	}

	protected void setANGMieNbGaussVisible(boolean isVisible_) {
		ANGMieNbGauss_title.setVisible(isVisible_);
		ANGMieNbGauss_textField.setVisible(isVisible_);
		ANGMieNbGauss_desc.setVisible(isVisible_);
	}

	protected void setANGMieUserAngFileVisible(boolean isVisible_) {
		ANGMieUserAngFile_title.setVisible(isVisible_);
		ANGMieUserAngFile_textField.setVisible(isVisible_);
		ANGMieUserAngFile_desc.setVisible(isVisible_);
	}

	protected void setANGMieResFileVisible(boolean isVisible_) {
		ANGMieResFile_title.setVisible(isVisible_);
		ANGMieResFile_textField.setVisible(isVisible_);
		ANGMieResFile_desc.setVisible(isVisible_);
	}

	protected void setANGLogVisible(boolean isVisible_) {
		ANGLog_title.setVisible(isVisible_);
		ANGLog_textField.setVisible(isVisible_);
		ANGLog_desc.setVisible(isVisible_);
	}
	
	public Integer getANGRadNbGauss() {
		ANGRadNbGauss_spinner.validate();
		return (Integer) ANGRadNbGauss_spinner.getValue();
	}

	private void saveANGRadNbGauss(){
		if(getANGRadNbGauss()!=null){
			geometricConditionsModel.setANGRadNbGauss( getANGRadNbGauss() );
		}
	}

	public String getANGRadUserAngFile() {
		ANGRadUserAngFile_textField.validate();
		return (String) ANGRadUserAngFile_textField.getText();
	}

	private void saveANGRadUserAngFile(){
		if(getANGRadUserAngFile()!=null){
			geometricConditionsModel.setANGRadUserAngFile( getANGRadUserAngFile() );
		}
	}

	public String getANGRadResFile() {
		ANGRadResFile_textField.validate();
		return (String) ANGRadResFile_textField.getText();
	}

	private void saveANGRadResFile(){
		if(getANGRadResFile()!=null){
			geometricConditionsModel.setANGRadResFile( getANGRadResFile() );
		}
	}

	public Integer getANGMieNbGauss() {
		ANGMieNbGauss_textField.validate();
		return (Integer) ANGMieNbGauss_textField.getValue();
	}

	private void saveANGMieNbGauss(){
		if(getANGMieNbGauss()!=null){
			geometricConditionsModel.setANGMieNbGauss( getANGMieNbGauss() );
		}
	}

	public String getANGMieUserAngFile() {
		ANGMieUserAngFile_textField.validate();
		return (String) ANGMieUserAngFile_textField.getText();
	}

	private void saveANGMieUserAngFile(){
		if(getANGMieUserAngFile()!=null){
			geometricConditionsModel.setANGMieUserAngFile( getANGMieUserAngFile() );
		}
	}

	public String getANGMieResFile() {
		ANGMieResFile_textField.validate();
		return (String) ANGMieResFile_textField.getText();
	}

	private void saveANGMieResFile(){
		if(getANGMieResFile()!=null){
			geometricConditionsModel.setANGMieResFile( getANGMieResFile() );
		}
	}

	public String getANGLog() {
		ANGLog_textField.validate();
		return (String) ANGLog_textField.getText();
	}

	private void saveANGLog(){
		if(getANGLog()!=null){
			geometricConditionsModel.setANGLog( getANGLog() );
		}
	}

}
