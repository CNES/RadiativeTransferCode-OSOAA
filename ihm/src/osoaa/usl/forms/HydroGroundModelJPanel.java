package osoaa.usl.forms;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Font;
import java.math.BigDecimal;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JSeparator;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.MaskFormatter;

import org.apache.log4j.Logger;

import osoaa.bll.exception.InitException;
import osoaa.bll.preferences.IAtmosphericAndSeaProfiles;
import osoaa.bll.preferences.IHydrogroundModel;
import osoaa.bll.preferences.PreferencesFactory;
import osoaa.bll.properties.PropertiesManager;
import osoaa.usl.common.ui.forms.FormUtils;
import osoaa.usl.common.ui.jspinner.JSpinnerOptionalValue;

import com.jgoodies.forms.factories.DefaultComponentFactory;
import com.jgoodies.forms.factories.FormFactory;
import com.jgoodies.forms.layout.ColumnSpec;
import com.jgoodies.forms.layout.FormLayout;
import com.jgoodies.forms.layout.RowSpec;
import osoaa.usl.common.ui.jspinner.JSpinnerRangedValue;
import osoaa.usl.common.ui.jspinner.SpinnerBigDecimalModel;

public class HydroGroundModelJPanel extends AbstractForm {
	/**
	 * 
	 */
	private Logger log = Logger.getLogger(this.getClass().getCanonicalName());
	
	private static final long serialVersionUID = 1L;
	private JTextField HYDDirMie_textField;

	private IAtmosphericAndSeaProfiles atmosphericAndSeaProfiles;
	private IHydrogroundModel hydrogroundModel;

	private final ButtonGroup buttonGroup = new ButtonGroup();
	
	private JLabel HYDLog_desc;
	private JSpinner PHYTOLNDSMMRwa_spinner;
	private JSpinner PHYTOLNDSMMIwa_spinner;
	private JSpinner PHYTOJDMIwa_spinner;
	private JLabel PHYTOLNDSMMIwa_desc;
	private JLabel PHYTOLNDSMMIwa_title;
	private JLabel HYDMieLog_title;
	private JSpinner PHYTOLNDSMSDvar_spinner;
	private JLabel PHYTOLNDTMMRwa_desc;
	private JSpinner PHYTOLNDTMMRwa_spinner;
	private JLabel PHYTOLNDTMMRwa_title;
	private JLabel SEDJDslope_desc;
	private JLabel SEDLNDSMMRwa_title;
	private JLabel SEDLNDSMMRwa_desc;
	private JLabel SEDJDrmin_title;
	private JLabel SEDJDrmax_title;
	private JSpinner SEDJDrmax_spinner;
	private JSpinner SEDJDrmin_spinner;
	private JLabel SEDJDrmin_desc;
	private JLabel SEDJDrmax_desc;
	private JLabel SEDLNDSMMIwa_title;
	private JLabel SEDLNDSMMIwa_desc;
	private JSpinner SEDLNDSMSDradius_spinner;
	private JSpinner SEDJDMRwa_spinner;
	private JLabel SEDJDMRwa_title;
	private JLabel SEDJDMRwa_desc;
	private JLabel HYDDirMie_title;
	private JLabel PHYTOResFile_title;
	private JLabel PHYTOJDMIwa_title;
	private JLabel PHYTOJDMRwa_title;
	private JLabel PHYTOLNDSMSDvar_title;
	private JLabel PHYTOLNDSMrate_title;
	private JLabel PHYTOLNDTMrate_title;
	private JLabel SEDJDslope_title;
	private JLabel SEDLNDSMSDradius_title;
	private JPanel panel_2;
	private JTextField HYDMieLog_textField;
	private JTextField HYDLog_textField;
	private JTextField PHYTOResFile_textField;
	private JLabel PHYTOResFile_desc;
	private JLabel HYDModel_desc;
	private JLabel HYDModel_title;
	private JLabel HYDModel_choice1_title;
	private JLabel PHYTOJDslope_title;
	private JLabel PHYTOJDrmin_title;
	private JSpinner PHYTOJDslope_spinner;
	private JLabel PHYTOJDslope_desc;
	private JLabel PHYTOJDrmin_desc;
	private JLabel PHYTOJDMIwa_desc;
	private JLabel PHYTOJDrmax_title;
	private JSpinner PHYTOJDrmax_spinner;
	private JLabel PHYTOJDrmax_desc;
	private JLabel PHYTOJDrate_desc;
	private JSpinner PHYTOJDrate_spinner;
	private JLabel PHYTOJDrate_title;
	private JLabel PHYTOLNDSMMRwa_title;
	private JLabel PHYTOLNDSMSDradius_title;
	private JLabel PHYTOLNDSMSDradius_desc;
	private JSpinner PHYTOLNDSMSDradius_spinner;
	private JLabel PHYTOLNDSMrate_desc;
	private JLabel PHYTOLNDTMMIwa_title;
	private JLabel PHYTOLNDTMSDradius_title;
	private JLabel PHYTOLNDTMMIwa_desc;
	private JLabel PHYTOLNDTMSDradius_desc;
	private JSpinner PHYTOLNDTMMIwa_spinner;
	private JSpinner PHYTOLNDTMSDradius_spinner;
	private JSpinner PHYTOLNDTMSDvar_spinner;
	private JLabel PHYTOLNDTMSDvar_title;
	private JLabel PHYTOLNDTMSDvar_desc;
	private JSpinner SEDJDslope_spinner;
	private JSpinner SEDLNDSMMIwa_spinner;
	private JLabel SEDLNDSMSDvar_title;
	private JSpinner SEDLNDSMSDvar_spinner;
	private JLabel SEDLNDSMSDvar_desc;
	private JLabel SEDLNDSMrate_title;
	private JSpinner SEDLNDSMrate_spinner;
	private JLabel SEDLNDSMrate_desc;
	private JLabel SEDLNDTMMRwa_title;
	private JSpinner SEDLNDTMMRwa_spinner;
	private JComponent SEDLNDTMMRwa_desc;
	private JLabel SEDLNDTMMIwa_title;
	private JSpinner SEDLNDTMMIwa_spinner;
	private JLabel SEDLNDTMMIwa_desc;
	private JLabel SEDLNDTMSDradius_desc;
	private JSpinner SEDLNDTMSDradius_spinner;
	private JLabel SEDLNDTMSDradius_title;
	private JLabel SEDLNDTMSDvar_title;
	private JSpinner SEDLNDTMSDvar_spinner;
	private JLabel SEDLNDTMSDvar_desc;
	private JLabel SEDLNDTMrate_title;
	private JSpinner SEDLNDTMrate_spinner;
	private JLabel SEDLNDTMrate_desc;
	private JLabel HYDExtData_title;
	private JTextField HYDExtData_textField;
	private JLabel HYDExtData_desc;
	private JLabel HYDLog_title;
	private JLabel SEDLNDSMSDradius_desc;
	private JLabel PHYTOLNDTMrate_desc;
	private JLabel PHYTOLNDSMMRwa_desc;
	private JLabel PHYTOLNDSMSDvar_desc;
	private JLabel HYDModel_choice15_title;
	private JTextField MLPResFile_textField;
	private JLabel MLPResFile_desc;
	private JLabel MLPResFile_title;
	private JPanel HYDModel_checkboxJPanel;
	private JRadioButton HYDModel_choice1_ckeckbox;
	private JRadioButton HYDModel_choice2_ckeckbox;
	private JLabel HYDModel_choice11_title;
	private JSpinner PHYTOJDMRwa_spinner;
	private JSpinner PHYTOJDrmin_spinner;
	private JLabel HYDModel_choice12_title;
	private JSeparator HYDModel_choice111_separator;
	private JSeparator HYDModel_choice121_separator;
	private JSeparator HYDModel_choice112_separator;
	private JSeparator HYDModel_choice122_separator;
	private JSpinner PHYTOLNDSMrate_spinner;
	private JSeparator HYDModel_choice131_separator;
	private JSeparator HYDModel_choice132_separator;
	private JLabel HYDModel_choice13_title;
	private JSpinner PHYTOLNDTMrate_spinner;
	private JSeparator HYDModel_choice141_separator;
	private JLabel HYDModel_choice14_title;
	private JSeparator HYDModel_choice142_separator;
	private JSeparator HYDModel_choice151_separator;
	private JSeparator HYDModel_choice152_separator;
	private JSpinner SEDLNDSMMRwa_spinner;
	private JLabel SEDJDMIwa_title;
	private JSpinner SEDJDMIwa_spinner;
	private JLabel SEDJDMIwa_desc;
	private JLabel SEDJDrate_title;
	private JLabel SEDJDrate_desc;
	private JSpinner SEDJDrate_spinner;
	private JSeparator HYDModel_choice161_separator;
	private JLabel HYDModel_choice16_title;
	private JSeparator HYDModel_choice162_separator;
	private JLabel HYDModel_choice2_title;
	private JSeparator HYDModel_choice2_separator;
	private JLabel HYDMieLog_desc;
	private JLabel HYDDirMie_desc;
	private JLabel PHYTOJDMRwa_desc;
	private JSeparator HYDModel_choice1_mainSeparator;
	private JPanel hydroModelContentFormJp;

	public void init() {
		hydrogroundModel = PreferencesFactory.getInstance().getHydrogroundModel();
		atmosphericAndSeaProfiles = PreferencesFactory.getInstance()
				.getAtmosphericAndSeaProfiles();

		HYDDirMie_textField.setText( hydrogroundModel.getHYDDirMie());
		HYDMieLog_textField.setText( hydrogroundModel.getHYDMieLog());
		HYDLog_textField.setText( hydrogroundModel.getHYDLog());
		PHYTOResFile_textField.setText( hydrogroundModel.getPHYTOResFile());
		MLPResFile_textField.setText( hydrogroundModel.getMLPResFile());
		HYDModel_choice1_ckeckbox.setSelected( hydrogroundModel.getHYDModel()==1 );
		HYDModel_choice2_ckeckbox.setSelected( hydrogroundModel.getHYDModel()==2 );
		PHYTOJDMRwa_spinner.setValue( hydrogroundModel.getPHYTOJDMRwa());
		PHYTOJDMIwa_spinner.setValue( hydrogroundModel.getPHYTOJDMIwa());
		PHYTOJDslope_spinner.setValue( hydrogroundModel.getPHYTOJDslope());
		PHYTOJDrmin_spinner.setValue( hydrogroundModel.getPHYTOJDrmin());
		PHYTOJDrmax_spinner.setValue( hydrogroundModel.getPHYTOJDrmax());
		PHYTOJDrate_spinner.setValue( hydrogroundModel.getPHYTOJDrate());
		PHYTOLNDSMMRwa_spinner.setValue( hydrogroundModel.getPHYTOLNDSMMRwa());
		PHYTOLNDSMMIwa_spinner.setValue( hydrogroundModel.getPHYTOLNDSMMIwa());
		PHYTOLNDSMSDradius_spinner.setValue( hydrogroundModel.getPHYTOLNDSMSDradius());
		PHYTOLNDSMSDvar_spinner.setValue( hydrogroundModel.getPHYTOLNDSMSDvar());
		PHYTOLNDSMrate_spinner.setValue( hydrogroundModel.getPHYTOLNDSMrate());
		PHYTOLNDTMMRwa_spinner.setValue( hydrogroundModel.getPHYTOLNDTMMRwa());
		PHYTOLNDTMMIwa_spinner.setValue( hydrogroundModel.getPHYTOLNDTMMIwa());
		PHYTOLNDTMSDradius_spinner.setValue( hydrogroundModel.getPHYTOLNDTMSDradius());
		PHYTOLNDTMSDvar_spinner.setValue( hydrogroundModel.getPHYTOLNDTMSDvar());
		PHYTOLNDTMrate_spinner.setValue( hydrogroundModel.getPHYTOLNDTMrate());
		SEDJDMRwa_spinner.setValue( hydrogroundModel.getSEDJDMRwa());
		SEDJDMIwa_spinner.setValue( hydrogroundModel.getSEDJDMIwa());
		SEDJDslope_spinner.setValue( hydrogroundModel.getSEDJDslope());
		SEDJDrmin_spinner.setValue( hydrogroundModel.getSEDJDrmin());
		SEDJDrmax_spinner.setValue( hydrogroundModel.getSEDJDrmax());
		SEDJDrate_spinner.setValue( hydrogroundModel.getSEDJDrate());
		SEDLNDSMMRwa_spinner.setValue( hydrogroundModel.getSEDLNDSMMRwa());
		SEDLNDSMMIwa_spinner.setValue( hydrogroundModel.getSEDLNDSMMIwa());
		SEDLNDSMSDradius_spinner.setValue( hydrogroundModel.getSEDLNDSMSDradius());
		SEDLNDSMSDvar_spinner.setValue( hydrogroundModel.getSEDLNDSMSDvar());
		SEDLNDSMrate_spinner.setValue( hydrogroundModel.getSEDLNDSMrate());
		SEDLNDTMMRwa_spinner.setValue( hydrogroundModel.getSEDLNDTMMRwa());
		SEDLNDTMMIwa_spinner.setValue( hydrogroundModel.getSEDLNDTMMIwa());
		SEDLNDTMSDradius_spinner.setValue( hydrogroundModel.getSEDLNDTMSDradius());
		SEDLNDTMSDvar_spinner.setValue( hydrogroundModel.getSEDLNDTMSDvar());
		SEDLNDTMrate_spinner.setValue( hydrogroundModel.getSEDLNDTMrate());
		HYDExtData_textField.setText( hydrogroundModel.getHYDExtData());
		
		validateForm();
	}
	


	private void validateFormInitToFalse() {
		FormUtils.setFieldState(false, HYDDirMie_title);
		FormUtils.setFieldState(false, HYDMieLog_title);
		FormUtils.setFieldState(false, HYDLog_title);
		FormUtils.setFieldState(false, PHYTOResFile_title);
		FormUtils.setFieldState(false, MLPResFile_title);
		FormUtils.setFieldState(false, HYDModel_title);
		FormUtils.setFieldState(false, PHYTOJDMRwa_title);
		FormUtils.setFieldState(false, PHYTOJDMIwa_title);
		FormUtils.setFieldState(false, PHYTOJDslope_title);
		FormUtils.setFieldState(false, PHYTOJDrmin_title);
		FormUtils.setFieldState(false, PHYTOJDrmax_title);
		FormUtils.setFieldState(false, PHYTOJDrate_title);
		FormUtils.setFieldState(false, PHYTOLNDSMMRwa_title);
		FormUtils.setFieldState(false, PHYTOLNDSMMIwa_title);
		FormUtils.setFieldState(false, PHYTOLNDSMSDradius_title);
		FormUtils.setFieldState(false, PHYTOLNDSMSDvar_title);
		FormUtils.setFieldState(false, PHYTOLNDSMrate_title);
		FormUtils.setFieldState(false, PHYTOLNDTMMRwa_title);
		FormUtils.setFieldState(false, PHYTOLNDTMMIwa_title);
		FormUtils.setFieldState(false, PHYTOLNDTMSDradius_title);
		FormUtils.setFieldState(false, PHYTOLNDTMSDvar_title);
		FormUtils.setFieldState(false, PHYTOLNDTMrate_title);
		FormUtils.setFieldState(false, SEDJDMRwa_title);
		FormUtils.setFieldState(false, SEDJDMIwa_title);
		FormUtils.setFieldState(false, SEDJDslope_title);
		FormUtils.setFieldState(false, SEDJDrmin_title);
		FormUtils.setFieldState(false, SEDJDrmax_title);
		FormUtils.setFieldState(false, SEDJDrate_title);
		FormUtils.setFieldState(false, SEDLNDSMMRwa_title);
		FormUtils.setFieldState(false, SEDLNDSMMIwa_title);
		FormUtils.setFieldState(false, SEDLNDSMSDradius_title);
		FormUtils.setFieldState(false, SEDLNDSMSDvar_title);
		FormUtils.setFieldState(false, SEDLNDSMrate_title);
		FormUtils.setFieldState(false, SEDLNDTMMRwa_title);
		FormUtils.setFieldState(false, SEDLNDTMMIwa_title);
		FormUtils.setFieldState(false, SEDLNDTMSDradius_title);
		FormUtils.setFieldState(false, SEDLNDTMSDvar_title);
		FormUtils.setFieldState(false, SEDLNDTMrate_title);
		FormUtils.setFieldState(false, HYDExtData_title);
	}
	private boolean validateHydrogroundModelForm() {
		validateFormInitToFalse();
		boolean isValid = true;

		isValid &= FormUtils.setFieldState(getHYDDirMie() == null || getHYDDirMie().trim().length()<=0, HYDDirMie_title);
//		isValid &= FormUtils.setFieldState(getHYDMieLog() == null || getHYDMieLog().trim().length()<=0, HYDMieLog_title);
//		isValid &= FormUtils.setFieldState(getHYDLog() == null || getHYDLog().trim().length()<=0, HYDLog_title);
		isValid &= FormUtils.setFieldState(getMLPResFile() == null || getMLPResFile().trim().length()<=0, MLPResFile_title);

		isValid &= FormUtils.setFieldState(getHYDModel() != 1 &&  getHYDModel() != 2, HYDModel_title);

		if( getHYDModel() == 1){
			if( atmosphericAndSeaProfiles.getPhytoChl().doubleValue() > 0 ){
				isValid &= FormUtils.setFieldState(getPHYTOResFile() == null, PHYTOResFile_title);
				isValid &= FormUtils.setFieldState(getPHYTOJDMRwa() == null, PHYTOJDMRwa_title);
				isValid &= FormUtils.setFieldState(getPHYTOJDMIwa() == null, PHYTOJDMIwa_title);
				isValid &= FormUtils.setFieldState(getPHYTOJDslope() == null, PHYTOJDslope_title);
				isValid &= FormUtils.setFieldState(getPHYTOJDrmin() == null, PHYTOJDrmin_title);
				isValid &= FormUtils.setFieldState(getPHYTOJDrmax() == null, PHYTOJDrmax_title);
				isValid &= FormUtils.setFieldState(getPHYTOJDrate() == null, PHYTOJDrate_title);
				
				//Optional form fields
//				isValid &= FormUtils.setFieldState(getPHYTOLNDSMMRwa() == null, PHYTOLNDSMMRwa_title);
//				isValid &= FormUtils.setFieldState(getPHYTOLNDSMMIwa() == null, PHYTOLNDSMMIwa_title);
//				isValid &= FormUtils.setFieldState(getPHYTOLNDSMSDradius() == null, PHYTOLNDSMSDradius_title);
//				isValid &= FormUtils.setFieldState(getPHYTOLNDSMSDvar() == null, PHYTOLNDSMSDvar_title);
//				isValid &= FormUtils.setFieldState(getPHYTOLNDSMrate() == null, PHYTOLNDSMrate_title);
//				
//				isValid &= FormUtils.setFieldState(getPHYTOLNDTMMRwa() == null, PHYTOLNDTMMRwa_title);
//				isValid &= FormUtils.setFieldState(getPHYTOLNDTMMIwa() == null, PHYTOLNDTMMIwa_title);
//				isValid &= FormUtils.setFieldState(getPHYTOLNDTMSDradius() == null, PHYTOLNDTMSDradius_title);
//				isValid &= FormUtils.setFieldState(getPHYTOLNDTMSDvar() == null, PHYTOLNDTMSDvar_title);
//				isValid &= FormUtils.setFieldState(getPHYTOLNDTMrate() == null, PHYTOLNDTMrate_title);
			}
			
			if( atmosphericAndSeaProfiles.getSEDCsed().doubleValue() > 0 ){
				isValid &= FormUtils.setFieldState(getSEDJDMRwa() == null, SEDJDMRwa_title);
				isValid &= FormUtils.setFieldState(getSEDJDMIwa() == null, SEDJDMIwa_title);
				isValid &= FormUtils.setFieldState(getSEDJDslope() == null, SEDJDslope_title);
				isValid &= FormUtils.setFieldState(getSEDJDrmin() == null, SEDJDrmin_title);
				isValid &= FormUtils.setFieldState(getSEDJDrmax() == null, SEDJDrmax_title);
				isValid &= FormUtils.setFieldState(getSEDJDrate() == null, SEDJDrate_title);
				
				//Optional form fields
//				isValid &= FormUtils.setFieldState(getSEDLNDSMMRwa() == null, SEDLNDSMMRwa_title);
//				isValid &= FormUtils.setFieldState(getSEDLNDSMMIwa() == null, SEDLNDSMMIwa_title);
//				isValid &= FormUtils.setFieldState(getSEDLNDSMSDradius() == null, SEDLNDSMSDradius_title);
//				isValid &= FormUtils.setFieldState(getSEDLNDSMSDvar() == null, SEDLNDSMSDvar_title);
//				isValid &= FormUtils.setFieldState(getSEDLNDSMrate() == null, SEDLNDSMrate_title);
//				
//				isValid &= FormUtils.setFieldState(getSEDLNDTMMRwa() == null, SEDLNDTMMRwa_title);
//				isValid &= FormUtils.setFieldState(getSEDLNDTMMIwa() == null, SEDLNDTMMIwa_title);
//				isValid &= FormUtils.setFieldState(getSEDLNDTMSDradius() == null, SEDLNDTMSDradius_title);
//				isValid &= FormUtils.setFieldState(getSEDLNDTMSDvar() == null, SEDLNDTMSDvar_title);
//				isValid &= FormUtils.setFieldState(getSEDLNDTMrate() == null, SEDLNDTMrate_title);
			}
			
		} else if( getHYDModel() == 2){
			isValid &= FormUtils.setFieldState(getHYDExtData() == null || getHYDExtData().trim().length()<=0, HYDExtData_title);
		}
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
	public HydroGroundModelJPanel() {
		super("Hydrosol models : sediments and phytoplancton");

        try
        {
		
		getFormFieldsPanel().setLayout(new BorderLayout());
		
		FormLayout formLayout = new FormLayout(new ColumnSpec[] {
				FormFactory.RELATED_GAP_COLSPEC,
				ColumnSpec.decode("max(82dlu;default)"),
				FormFactory.RELATED_GAP_COLSPEC,
				FormFactory.DEFAULT_COLSPEC,
				FormFactory.RELATED_GAP_COLSPEC,
				FormFactory.DEFAULT_COLSPEC,
				FormFactory.RELATED_GAP_COLSPEC,
				ColumnSpec.decode("max(4dlu;default)"),
				FormFactory.DEFAULT_COLSPEC,},
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
				FormFactory.DEFAULT_ROWSPEC,});
		

		JPanel northFormPanel = new JPanel(formLayout);
		northFormPanel.setBackground(Color.WHITE);
		
		HYDDirMie_title = DefaultComponentFactory.getInstance().createLabel(
				"HYD.DirMie *:");
		northFormPanel.add(HYDDirMie_title, "2, 1, right, default");

		HYDDirMie_textField = new JTextField();
		HYDDirMie_textField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				saveHYDDirMie();
				validateForm();
			}
		});
		northFormPanel.add(HYDDirMie_textField, "4, 1, fill, default");
		HYDDirMie_textField.setColumns(10);

		HYDDirMie_desc = DefaultComponentFactory.getInstance()
				.createLabel("Mie files repository directory (full path)");
		HYDDirMie_desc
				.setToolTipText("Mie files repository directory (full path)");
		northFormPanel.add(HYDDirMie_desc, "9, 1");

		HYDMieLog_title = new JLabel("HYD.MieLog :");
		HYDMieLog_title.setHorizontalAlignment(SwingConstants.RIGHT);
		northFormPanel.add(HYDMieLog_title, "2, 3, right, default");

		HYDMieLog_textField = new JTextField();
		HYDMieLog_textField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent arg0) {
				saveHYDMieLog();
				validateForm();
			}
		});
		HYDMieLog_textField.setColumns(10);
		northFormPanel.add(HYDMieLog_textField, "4, 3, fill, default");

		HYDMieLog_desc = DefaultComponentFactory.getInstance()
				.createLabel("Log filename of Mie calculations for hydrosols");
		HYDMieLog_desc
				.setToolTipText("Log filename of Mie calculations for hydrosols");
		northFormPanel.add(HYDMieLog_desc, "9, 3");

		HYDLog_title = new JLabel("HYD.Log :");
		HYDLog_title.setHorizontalAlignment(SwingConstants.RIGHT);
		northFormPanel.add(HYDLog_title, "2, 5, right, default");

		HYDLog_textField = new JTextField();
		HYDLog_textField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				saveHYDLog();
				validateForm();
			}
		});
		HYDLog_textField.setColumns(10);
		northFormPanel.add(HYDLog_textField, "4, 5, fill, default");

		HYDLog_desc = new JLabel("Log filename of the Hydrosols computations ");
		HYDLog_desc
				.setToolTipText("Log filename of the Hydrosols computations ");
		northFormPanel.add(HYDLog_desc, "9, 5");

		PHYTOResFile_title = DefaultComponentFactory.getInstance().createLabel(
				"PHYTO.ResFile *:");
		northFormPanel.add(PHYTOResFile_title, "2, 7, right, default");

		PHYTOResFile_textField = new JTextField();
		PHYTOResFile_textField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				savePHYTOResFile();
				validateForm();
			}
		});
		PHYTOResFile_textField.setColumns(10);
		northFormPanel.add(PHYTOResFile_textField, "4, 7, fill, default");

		PHYTOResFile_desc = new JLabel(
				"Filename for the radiative properties output for phytoplankton calculation ");
		PHYTOResFile_desc
				.setToolTipText("Filename for the radiative properties output for phytoplankton calculation ");
		northFormPanel.add(PHYTOResFile_desc, "9, 7");

		MLPResFile_title = new JLabel("MLP.ResFile *:");
		MLPResFile_title.setToolTipText("");
		northFormPanel.add(MLPResFile_title, "2, 9, right, default");

		MLPResFile_textField = new JTextField();
		MLPResFile_textField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				saveMLPResFile();
				validateForm();
			}
		});
		MLPResFile_textField.setColumns(10);
		northFormPanel.add(MLPResFile_textField, "4, 9, fill, default");

		MLPResFile_desc = new JLabel(
				"Filename for the radiative properties output for mineral-like particles calculation ");
		MLPResFile_desc
				.setToolTipText("Filename for the radiative properties output for mineral-like particles calculation ");
		northFormPanel.add(MLPResFile_desc, "9, 9");

		HYDModel_title = new JLabel("HYD.Model *:");
		HYDModel_title.setHorizontalAlignment(SwingConstants.RIGHT);
		northFormPanel.add(HYDModel_title, "2, 11, right, default");

		HYDModel_checkboxJPanel = new JPanel();
		HYDModel_checkboxJPanel.setBackground(Color.WHITE);
		northFormPanel.add(HYDModel_checkboxJPanel, "4, 11, fill, fill");
		HYDModel_checkboxJPanel.setLayout(new BoxLayout(HYDModel_checkboxJPanel, BoxLayout.Y_AXIS));

		HYDModel_choice1_ckeckbox = new JRadioButton(
				"By using size distribution models");
		buttonGroup.add(HYDModel_choice1_ckeckbox);
		HYDModel_choice1_ckeckbox.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent arg0) {
				saveHYDModel();
				
				onHYDModelChanged();
				
				validateForm();
			}
		});
		HYDModel_choice1_ckeckbox.setBackground(Color.WHITE);
		HYDModel_checkboxJPanel.add(HYDModel_choice1_ckeckbox);

		HYDModel_choice2_ckeckbox = new JRadioButton(
				"By using an external phase function");
		buttonGroup.add(HYDModel_choice2_ckeckbox);
		HYDModel_choice2_ckeckbox.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveHYDModel();
				
				onHYDModelChanged();
				
				validateForm();
			}
		});
		HYDModel_choice2_ckeckbox.setBackground(Color.WHITE);
		HYDModel_checkboxJPanel.add(HYDModel_choice2_ckeckbox);

		HYDModel_desc = new JLabel("Type of hydrosol characterization");
		HYDModel_desc.setToolTipText("Type of hydrosol characterization");
		northFormPanel.add(HYDModel_desc, "9, 11");

		getFormFieldsPanel().add(northFormPanel, BorderLayout.NORTH);
		
		
		hydroModelContentFormJp = new JPanel( new CardLayout(5, 5) );
		hydroModelContentFormJp.setBackground(Color.WHITE);
		
		
		/////////////////////////////
		FormLayout hydroModel1ContentFormLayout = new FormLayout(new ColumnSpec[] {
				FormFactory.RELATED_GAP_COLSPEC,
				ColumnSpec.decode("max(67dlu;default):grow"),
				FormFactory.RELATED_GAP_COLSPEC,
				ColumnSpec.decode("max(145dlu;default):grow"),
				FormFactory.RELATED_GAP_COLSPEC,
				FormFactory.DEFAULT_COLSPEC,
				FormFactory.RELATED_GAP_COLSPEC,
				ColumnSpec.decode("max(4dlu;default)"),
				ColumnSpec.decode("default:grow"),},
			new RowSpec[] {
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
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,});
		
		JPanel hydroModel1ContentFormJp = new JPanel(hydroModel1ContentFormLayout);
		hydroModel1ContentFormJp.setBackground(Color.WHITE);
		hydroModelContentFormJp.add(hydroModel1ContentFormJp, "1");

		HYDModel_choice1_title = new JLabel(
				"    |--> Hydrosols characterization by models");
		HYDModel_choice1_title.setHorizontalAlignment(SwingConstants.LEFT);
		HYDModel_choice1_title.setForeground(Color.BLUE);
		HYDModel_choice1_title.setFont(new Font("Tahoma", Font.BOLD, 11));
		hydroModel1ContentFormJp.add(HYDModel_choice1_title, "2, 2, 3, 1");

		HYDModel_choice111_separator = new JSeparator();
		HYDModel_choice111_separator.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		hydroModel1ContentFormJp.add(HYDModel_choice111_separator, "2, 4");

		HYDModel_choice11_title = new JLabel(
				"    |--> Main mode of phytoplankton : Junge distribution");
		HYDModel_choice11_title.setHorizontalAlignment(SwingConstants.LEFT);
		HYDModel_choice11_title.setForeground(Color.BLUE);
		HYDModel_choice11_title.setFont(new Font("Tahoma", Font.PLAIN, 11));
		hydroModel1ContentFormJp.add(HYDModel_choice11_title, "4, 4");

		HYDModel_choice112_separator = new JSeparator();
		HYDModel_choice112_separator.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		hydroModel1ContentFormJp.add(HYDModel_choice112_separator, "9, 4");

		PHYTOJDMRwa_title = new JLabel("PHYTO.JD.MRwa *:");
		PHYTOJDMRwa_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(PHYTOJDMRwa_title, "2, 6, right, default");

		PHYTOJDMRwa_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.000"), new BigDecimal("0.000"), null, new BigDecimal("0.001")));
        PropertiesManager.getInstance().register(PHYTOJDMRwa_title, PHYTOJDMRwa_spinner);
		PHYTOJDMRwa_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePHYTOJDMRwa();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(PHYTOJDMRwa_spinner, "4, 6");

		PHYTOJDMRwa_desc = new JLabel(
				"Real part of the refractive index for the simulation wavelength ");
		PHYTOJDMRwa_desc
				.setToolTipText("Real part of the refractive index for the simulation wavelength ");
		hydroModel1ContentFormJp.add(PHYTOJDMRwa_desc, "9, 6");

		PHYTOJDMIwa_title = new JLabel("PHYTO.JD.MIwa *:");
		hydroModel1ContentFormJp.add(PHYTOJDMIwa_title, "2, 8, right, default");

		PHYTOJDMIwa_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.000"), new BigDecimal(-1), new BigDecimal("0.000"), new BigDecimal("0.001")));
		hydroModel1ContentFormJp.add(PHYTOJDMIwa_spinner, "4, 8");
        PropertiesManager.getInstance().register(PHYTOJDMIwa_title, PHYTOJDMIwa_spinner);
		PHYTOJDMIwa_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePHYTOJDMIwa();
				validateForm();
			}
		});

		PHYTOJDMIwa_desc = new JLabel(
				"Imaginary part of the refractive index (negative value) for the simulation wavelength  ");
		PHYTOJDMIwa_desc
				.setToolTipText("Imaginary part of the refractive index (negative value) for the simulation wavelength  ");
		hydroModel1ContentFormJp.add(PHYTOJDMIwa_desc, "9, 8");

		PHYTOJDslope_title = new JLabel("PHYTO.JD.slope *:");
		hydroModel1ContentFormJp.add(PHYTOJDslope_title, "2, 10, right, default");

		PHYTOJDslope_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("3.0"), new BigDecimal("3.0"), new BigDecimal("5.0"), new BigDecimal("0.1")));
        PropertiesManager.getInstance().register(PHYTOJDslope_title, PHYTOJDslope_spinner);
		PHYTOJDslope_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePHYTOJDslope();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(PHYTOJDslope_spinner, "4, 10");

		PHYTOJDslope_desc = new JLabel("Slope of Junge\u2019s law, a positive value in the range 3.0-5.0");
		hydroModel1ContentFormJp.add(PHYTOJDslope_desc, "9, 10");

		PHYTOJDrmin_title = new JLabel("PHYTO.JD.rmin *:");
		hydroModel1ContentFormJp.add(PHYTOJDrmin_title, "2, 12, right, default");

		PHYTOJDrmin_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"), new BigDecimal("0.00"), null, new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(PHYTOJDrmin_title, PHYTOJDrmin_spinner);
		PHYTOJDrmin_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePHYTOJDrmin();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(PHYTOJDrmin_spinner, "4, 12");

		PHYTOJDrmin_desc = new JLabel(
				"Minimal radius of phytoplankton particles (\u00B5m)");
		PHYTOJDrmin_desc
				.setToolTipText("Minimal radius of phytoplankton particles (\u00B5m)");
		hydroModel1ContentFormJp.add(PHYTOJDrmin_desc, "9, 12");

		PHYTOJDrmax_title = new JLabel("PHYTO.JD.rmax *:");
		PHYTOJDrmax_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(PHYTOJDrmax_title, "2, 14");

		PHYTOJDrmax_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.0"), new BigDecimal("0.0"), null, new BigDecimal("1.0")));
        PropertiesManager.getInstance().register(PHYTOJDrmax_title, PHYTOJDrmax_spinner);
		PHYTOJDrmax_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePHYTOJDrmax();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(PHYTOJDrmax_spinner, "4, 14");

		PHYTOJDrmax_desc = new JLabel(
				"Maximal radius of phytoplankton particles (\u00B5m)");
		PHYTOJDrmax_desc
				.setToolTipText("Maximal radius of phytoplankton particles (\u00B5m)");
		hydroModel1ContentFormJp.add(PHYTOJDrmax_desc, "9, 14");

		PHYTOJDrate_title = new JLabel("PHYTO.JD.rate *:");
		hydroModel1ContentFormJp.add(PHYTOJDrate_title, "2, 16, right, default");

		PHYTOJDrate_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"), new BigDecimal("0.00"), new BigDecimal("1.00"), new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(PHYTOJDrate_title, PHYTOJDrate_spinner);
		PHYTOJDrate_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePHYTOJDrate();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(PHYTOJDrate_spinner, "4, 16");

		PHYTOJDrate_desc = new JLabel(
				"Ratio of the main mode in the global distribution (as a proportion of the Junge distribution particles versus the global amount of particules)");
		hydroModel1ContentFormJp.add(PHYTOJDrate_desc, "9, 16");

		HYDModel_choice121_separator = new JSeparator();
		HYDModel_choice121_separator.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		hydroModel1ContentFormJp.add(HYDModel_choice121_separator, "2, 18");

		HYDModel_choice12_title = new JLabel(
				"    |--> Secondary mode of phytoplankton :LND parameters");
		HYDModel_choice12_title.setHorizontalAlignment(SwingConstants.LEFT);
		HYDModel_choice12_title.setForeground(Color.BLUE);
		HYDModel_choice12_title.setFont(new Font("Tahoma", Font.PLAIN, 11));
		hydroModel1ContentFormJp.add(HYDModel_choice12_title, "4, 18");

		HYDModel_choice122_separator = new JSeparator();
		HYDModel_choice122_separator.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		hydroModel1ContentFormJp.add(HYDModel_choice122_separator, "9, 18");

		PHYTOLNDSMMRwa_title = new JLabel("PHYTO.LND.SM.MRwa :");
		hydroModel1ContentFormJp.add(PHYTOLNDSMMRwa_title, "2, 20, right, default");

		panel_2 = new JPanel();
		hydroModel1ContentFormJp.add(panel_2, "4, 20, fill, fill");
		panel_2.setLayout(new BorderLayout(0, 0));

		PHYTOLNDSMMRwa_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.000"),
                new BigDecimal("0.000"), null, new BigDecimal("0.001")));
		panel_2.add(PHYTOLNDSMMRwa_spinner, BorderLayout.CENTER);
        PropertiesManager.getInstance().register(PHYTOLNDSMMRwa_title, PHYTOLNDSMMRwa_spinner);
		PHYTOLNDSMMRwa_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePHYTOLNDSMMRwa();
				validateForm();
			}
		});

		PHYTOLNDSMMRwa_desc = new JLabel(
				"Real part of the refractive index for the secondary LND mode particles at simulation wavelength ");
		PHYTOLNDSMMRwa_desc
				.setToolTipText("Real part of the refractive index for the secondary LND mode particles at simulation wavelength ");
		hydroModel1ContentFormJp.add(PHYTOLNDSMMRwa_desc, "9, 20");

		PHYTOLNDSMMIwa_title = new JLabel("PHYTO.LND.SM.MIwa :");
		PHYTOLNDSMMIwa_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(PHYTOLNDSMMIwa_title, "2, 22");

		PHYTOLNDSMMIwa_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.000"),
                new BigDecimal(-1), new BigDecimal("0.000"), new BigDecimal("0.001")));
        PropertiesManager.getInstance().register(PHYTOLNDSMMIwa_title, PHYTOLNDSMMIwa_spinner);
		PHYTOLNDSMMIwa_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePHYTOLNDSMMIwa();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(PHYTOLNDSMMIwa_spinner, "4, 22");

		PHYTOLNDSMMIwa_desc = new JLabel(
				"Imaginary part of the refractive index for the secondary LND mode particles at simulation wavelength");
		PHYTOLNDSMMIwa_desc
				.setToolTipText("Imaginary part of the refractive index for the secondary LND mode particles at simulation wavelength");
		hydroModel1ContentFormJp.add(PHYTOLNDSMMIwa_desc, "9, 22");

		PHYTOLNDSMSDradius_title = new JLabel("PHYTO.LND.SM.SDradius :");
		PHYTOLNDSMSDradius_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(PHYTOLNDSMSDradius_title, "2, 24, right, default");

		PHYTOLNDSMSDradius_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(
                new BigDecimal("0.00"), new BigDecimal("0.00"), null, new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(PHYTOLNDSMSDradius_title, PHYTOLNDSMSDradius_spinner);
		PHYTOLNDSMSDradius_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePHYTOLNDSMSDradius();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(PHYTOLNDSMSDradius_spinner, "4, 24");

		PHYTOLNDSMSDradius_desc = new JLabel(
				"Modal radius of LND size distribution for the secondary LND mode (\u00B5m)");
		PHYTOLNDSMSDradius_desc
				.setToolTipText("Modal radius of LND size distribution for the secondary LND mode (\u00B5m)");
		hydroModel1ContentFormJp.add(PHYTOLNDSMSDradius_desc, "9, 24");

		PHYTOLNDSMSDvar_title = new JLabel("PHYTO.LND.SM.SDvar :");
		PHYTOLNDSMSDvar_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(PHYTOLNDSMSDvar_title, "2, 26");

		PHYTOLNDSMSDvar_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"),
                new BigDecimal("0.00"), null, new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(PHYTOLNDSMSDvar_title, PHYTOLNDSMSDvar_spinner);
		PHYTOLNDSMSDvar_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePHYTOLNDSMSDvar();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(PHYTOLNDSMSDvar_spinner, "4, 26");

		PHYTOLNDSMSDvar_desc = new JLabel(
				"Standard deviation of LND size distribution for the secondary LND mode");
		PHYTOLNDSMSDvar_desc
				.setToolTipText("Standard deviation of LND size distribution for the secondary LND mode");
		hydroModel1ContentFormJp.add(PHYTOLNDSMSDvar_desc, "9, 26");

		PHYTOLNDSMrate_title = new JLabel("PHYTO.LND.SM.rate :");
		PHYTOLNDSMrate_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(PHYTOLNDSMrate_title, "2, 28, right, default");

		PHYTOLNDSMrate_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"),
                new BigDecimal("0.00"), new BigDecimal("1.00"), new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(PHYTOLNDSMrate_title, PHYTOLNDSMrate_spinner);
		PHYTOLNDSMrate_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePHYTOLNDSMrate();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(PHYTOLNDSMrate_spinner, "4, 28");

		PHYTOLNDSMrate_desc = new JLabel(
				"Ratio of the main mode in the global distribution (as a proportion of the secondary LND mode particles versus the global amount of particules)");
		PHYTOLNDSMrate_desc
				.setToolTipText("Ratio of the main mode in the global distribution (as a proportion of the secondary LND mode particles versus the global amount of particules)");
		hydroModel1ContentFormJp.add(PHYTOLNDSMrate_desc, "9, 28");

		HYDModel_choice131_separator = new JSeparator();
		HYDModel_choice131_separator.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		hydroModel1ContentFormJp.add(HYDModel_choice131_separator, "2, 30");

		HYDModel_choice13_title = new JLabel(
				"    |--> Tertiary mode of phytoplankton : LND parameters");
		HYDModel_choice13_title.setHorizontalAlignment(SwingConstants.LEFT);
		HYDModel_choice13_title.setForeground(Color.BLUE);
		HYDModel_choice13_title.setFont(new Font("Tahoma", Font.PLAIN, 11));
		hydroModel1ContentFormJp.add(HYDModel_choice13_title, "4, 30");

		HYDModel_choice132_separator = new JSeparator();
		HYDModel_choice132_separator.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		hydroModel1ContentFormJp.add(HYDModel_choice132_separator, "9, 30");

		PHYTOLNDTMMRwa_title = new JLabel("PHYTO.LND.TM.MRwa :");
		PHYTOLNDTMMRwa_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(PHYTOLNDTMMRwa_title, "2, 32, default, top");

		PHYTOLNDTMMRwa_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.000"),
                new BigDecimal("0.000"), null, new BigDecimal("0.001")));
        PropertiesManager.getInstance().register(PHYTOLNDTMMRwa_title, PHYTOLNDTMMRwa_spinner);
		PHYTOLNDTMMRwa_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePHYTOLNDTMMRwa();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(PHYTOLNDTMMRwa_spinner, "4, 32");

		PHYTOLNDTMMRwa_desc = new JLabel(
				"Real part of the refractive index for the tertiary LND mode particles at simulation wavelength ");
		PHYTOLNDTMMRwa_desc
				.setToolTipText("Real part of the refractive index for the tertiary LND mode particles at simulation wavelength ");
		hydroModel1ContentFormJp.add(PHYTOLNDTMMRwa_desc, "9, 32");

		PHYTOLNDTMMIwa_title = new JLabel("PHYTO.LND.TM.MIwa :");
		PHYTOLNDTMMIwa_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(PHYTOLNDTMMIwa_title, "2, 34");

		PHYTOLNDTMMIwa_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.000"),
                new BigDecimal(-1), new BigDecimal("0.000"), new BigDecimal("0.001")));
        PropertiesManager.getInstance().register(PHYTOLNDTMMIwa_title, PHYTOLNDTMMIwa_spinner);
		PHYTOLNDTMMIwa_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePHYTOLNDTMMIwa();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(PHYTOLNDTMMIwa_spinner, "4, 34");

		PHYTOLNDTMMIwa_desc = new JLabel(
				"Imaginary part of the refractive index for the tertiary LND mode particles at simulation wavelength");
		PHYTOLNDTMMIwa_desc
				.setToolTipText("Imaginary part of the refractive index for the tertiary LND mode particles at simulation wavelength");
		hydroModel1ContentFormJp.add(PHYTOLNDTMMIwa_desc, "9, 34");

		PHYTOLNDTMSDradius_title = new JLabel("PHYTO.LND.TM.SDradius :");
		PHYTOLNDTMSDradius_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(PHYTOLNDTMSDradius_title, "2, 36");

		PHYTOLNDTMSDradius_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(
                new BigDecimal("0.00"), null, null, new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(PHYTOLNDTMSDradius_title, PHYTOLNDTMSDradius_spinner);
		PHYTOLNDTMSDradius_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePHYTOLNDTMSDradius();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(PHYTOLNDTMSDradius_spinner, "4, 36");

		PHYTOLNDTMSDradius_desc = new JLabel(
				"Modal radius of LND size distribution for the tertiary LND mode (\u00B5m)");
		PHYTOLNDTMSDradius_desc
				.setToolTipText("Modal radius of LND size distribution for the tertiary LND mode (\u00B5m)");
		hydroModel1ContentFormJp.add(PHYTOLNDTMSDradius_desc, "9, 36");

		PHYTOLNDTMSDvar_title = new JLabel("PHYTO.LND.TM.SDvar :");
		PHYTOLNDTMSDvar_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(PHYTOLNDTMSDvar_title, "2, 38");

		PHYTOLNDTMSDvar_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"),
                new BigDecimal("0.00"), null, new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(PHYTOLNDTMSDvar_title, PHYTOLNDTMSDvar_spinner);
		PHYTOLNDTMSDvar_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePHYTOLNDTMSDvar();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(PHYTOLNDTMSDvar_spinner, "4, 38");

		PHYTOLNDTMSDvar_desc = new JLabel(
				"Standard deviation of LND size distribution for the tertiary LND mode");
		PHYTOLNDTMSDvar_desc
				.setToolTipText("Standard deviation of LND size distribution for the tertiary LND mode");
		hydroModel1ContentFormJp.add(PHYTOLNDTMSDvar_desc, "9, 38");

		PHYTOLNDTMrate_title = new JLabel("PHYTO.LND.TM.rate :");
		hydroModel1ContentFormJp.add(PHYTOLNDTMrate_title, "2, 40, right, default");

		PHYTOLNDTMrate_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"),
                new BigDecimal("0.00"), new BigDecimal("1.00"), new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(PHYTOLNDTMrate_title, PHYTOLNDTMrate_spinner);
		PHYTOLNDTMrate_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePHYTOLNDTMrate();
				validateForm();
			}
		});

		hydroModel1ContentFormJp.add(PHYTOLNDTMrate_spinner, "4, 40");

		PHYTOLNDTMrate_desc = new JLabel(
				"Ratio of the main mode in the global distribution (as a proportion of the secondary LND mode particles versus the global amount of particules)");
		PHYTOLNDTMrate_desc
				.setToolTipText("Ratio of the main mode in the global distribution (as a proportion of the secondary LND mode particles versus the global amount of particules)");
		hydroModel1ContentFormJp.add(PHYTOLNDTMrate_desc, "9, 40");
		
		HYDModel_choice1_mainSeparator = new JSeparator();
		HYDModel_choice1_mainSeparator.setForeground(new Color(230, 230, 250));
		hydroModel1ContentFormJp.add(HYDModel_choice1_mainSeparator, "2, 42, 8, 1");

		HYDModel_choice141_separator = new JSeparator();
		HYDModel_choice141_separator.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		hydroModel1ContentFormJp.add(HYDModel_choice141_separator, "2, 44");

		HYDModel_choice14_title = new JLabel(
				"    |--> Main mode of sediment  : Junge distribution");
		HYDModel_choice14_title.setHorizontalAlignment(SwingConstants.LEFT);
		HYDModel_choice14_title.setForeground(Color.BLUE);
		HYDModel_choice14_title.setFont(new Font("Tahoma", Font.PLAIN, 11));
		hydroModel1ContentFormJp.add(HYDModel_choice14_title, "4, 44");

		HYDModel_choice142_separator = new JSeparator();
		HYDModel_choice142_separator.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		hydroModel1ContentFormJp.add(HYDModel_choice142_separator, "9, 44");

		SEDJDMRwa_title = new JLabel("SED.JD.MRwa *:");
		SEDJDMRwa_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(SEDJDMRwa_title, "2, 46");

		SEDJDMRwa_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.000"), new BigDecimal("0.000"), null, new BigDecimal("0.001")));
        PropertiesManager.getInstance().register(SEDJDMRwa_title, SEDJDMRwa_spinner);
		SEDJDMRwa_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveSEDJDMRwa();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(SEDJDMRwa_spinner, "4, 46");

		SEDJDMRwa_desc = new JLabel(
				"Real part of the refractive index for the simulation wavelength ");
		SEDJDMRwa_desc
				.setToolTipText("Real part of the refractive index for the simulation wavelength ");
		hydroModel1ContentFormJp.add(SEDJDMRwa_desc, "9, 46");

		SEDJDMIwa_title = new JLabel("SED.JD.MIwa *:");
		SEDJDMIwa_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(SEDJDMIwa_title, "2, 48");

		SEDJDMIwa_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.000"), new BigDecimal(-1), new BigDecimal("0.000"), new BigDecimal("0.001")));
        PropertiesManager.getInstance().register(SEDJDMIwa_title, SEDJDMIwa_spinner);
		SEDJDMIwa_spinner.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                saveSEDJDMIwa();
                validateForm();
            }
        });
		hydroModel1ContentFormJp.add(SEDJDMIwa_spinner, "4, 48");

		SEDJDMIwa_desc = new JLabel(
				"Imaginary part of the refractive index (negative value) for the simulation wavelength  ");
		SEDJDMIwa_desc
				.setToolTipText("Imaginary part of the refractive index (negative value) for the simulation wavelength  ");
		hydroModel1ContentFormJp.add(SEDJDMIwa_desc, "9, 48");

		SEDJDslope_title = new JLabel("SED.JD.slope *:");
		hydroModel1ContentFormJp.add(SEDJDslope_title, "2, 50, right, default");

		SEDJDslope_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("3.0"), new BigDecimal("3.0"), new BigDecimal("5.0"), new BigDecimal("0.1")));
        PropertiesManager.getInstance().register(SEDJDslope_title, SEDJDslope_spinner);
		SEDJDslope_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveSEDJDslope();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(SEDJDslope_spinner, "4, 50");

		SEDJDslope_desc = new JLabel("Slope of Junge\u2019s law, a positive value in the range 3.0-5.0");
		hydroModel1ContentFormJp.add(SEDJDslope_desc, "9, 50");

		SEDJDrmin_title = new JLabel("SED.JD.rmin *:");
		SEDJDrmin_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(SEDJDrmin_title, "2, 52");

		SEDJDrmin_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"), new BigDecimal("0.00"), null, new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(SEDJDrmin_title, SEDJDrmin_spinner);
        SEDJDrmin_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveSEDJDrmin();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(SEDJDrmin_spinner, "4, 52");

		SEDJDrmin_desc = new JLabel(
				"Minimal radius of mineral-like particles (\u00B5m)");
		hydroModel1ContentFormJp.add(SEDJDrmin_desc, "9, 52");

		SEDJDrmax_title = new JLabel("SED.JD.rmax *:");
		SEDJDrmax_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(SEDJDrmax_title, "2, 54");

		SEDJDrmax_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.0"), new BigDecimal("0.0"), null, new BigDecimal("1.0")));
        PropertiesManager.getInstance().register(SEDJDrmax_title, SEDJDrmax_spinner);
        SEDJDrmax_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveSEDJDrmax();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(SEDJDrmax_spinner, "4, 54");

		SEDJDrmax_desc = new JLabel(
				"Maximal radius of mineral-like particles (\u00B5m)");
		hydroModel1ContentFormJp.add(SEDJDrmax_desc, "9, 54");

		SEDJDrate_title = new JLabel("SED.JD.rate *:");
		SEDJDrate_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(SEDJDrate_title, "2, 56");

		SEDJDrate_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"), new BigDecimal("0.00"), new BigDecimal("1.00"), new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(SEDJDrate_title, SEDJDrate_spinner);
		SEDJDrate_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveSEDJDrate();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(SEDJDrate_spinner, "4, 56");

		SEDJDrate_desc = new JLabel(
				"Ratio of the main mode in the global distribution (as a proportion of the Junge distribution particles versus the global amount of particules)");
		SEDJDrate_desc.setHorizontalAlignment(SwingConstants.LEFT);
		hydroModel1ContentFormJp.add(SEDJDrate_desc, "9, 56");

		HYDModel_choice151_separator = new JSeparator();
		HYDModel_choice151_separator.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		hydroModel1ContentFormJp.add(HYDModel_choice151_separator, "2, 58");

		HYDModel_choice15_title = new JLabel(
				"    |--> Secondary mode of sediment :LND parameters");
		HYDModel_choice15_title.setHorizontalAlignment(SwingConstants.LEFT);
		HYDModel_choice15_title.setForeground(Color.BLUE);
		HYDModel_choice15_title.setFont(new Font("Tahoma", Font.PLAIN, 11));
		hydroModel1ContentFormJp.add(HYDModel_choice15_title, "4, 58");

		HYDModel_choice152_separator = new JSeparator();
		HYDModel_choice152_separator.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		hydroModel1ContentFormJp.add(HYDModel_choice152_separator, "9, 58");

		SEDLNDSMMRwa_title = new JLabel("SED.LND.SM.MRwa :");
		SEDLNDSMMRwa_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(SEDLNDSMMRwa_title, "2, 60, right, default");

		SEDLNDSMMRwa_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.000"),
                new BigDecimal("0.000"), null, new BigDecimal("0.001")));
        PropertiesManager.getInstance().register(SEDLNDSMMRwa_title, SEDLNDSMMRwa_spinner);
		SEDLNDSMMRwa_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveSEDLNDSMMRwa();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(SEDLNDSMMRwa_spinner, "4, 60");

		SEDLNDSMMRwa_desc = new JLabel(
				"Real part of the refractive index for the secondary LND mode particles at simulation wavelength ");
		SEDLNDSMMRwa_desc
				.setToolTipText("Real part of the refractive index for the secondary LND mode particles at simulation wavelength ");
		hydroModel1ContentFormJp.add(SEDLNDSMMRwa_desc, "9, 60");

		SEDLNDSMMIwa_title = new JLabel("SED.LND.SM.MIwa :");
		hydroModel1ContentFormJp.add(SEDLNDSMMIwa_title, "2, 62, right, default");

		SEDLNDSMMIwa_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.000"),
                new BigDecimal(-1), new BigDecimal("0.000"), new BigDecimal("0.001")));
        PropertiesManager.getInstance().register(SEDLNDSMMIwa_title, SEDLNDSMMIwa_spinner);
        SEDLNDSMMIwa_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveSEDLNDSMMIwa();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(SEDLNDSMMIwa_spinner, "4, 62");

		SEDLNDSMMIwa_desc = new JLabel(
				"Imaginary part of the refractive index for the secondary LND mode particles at simulation wavelength");
		SEDLNDSMMIwa_desc
				.setToolTipText("Imaginary part of the refractive index for the secondary LND mode particles at simulation wavelength");
		hydroModel1ContentFormJp.add(SEDLNDSMMIwa_desc, "9, 62");

		SEDLNDSMSDradius_title = new JLabel("SED.LND.SM.SDradius :");
		SEDLNDSMSDradius_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(SEDLNDSMSDradius_title, "2, 64");

		SEDLNDSMSDradius_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"),
                new BigDecimal("0.00"), null, new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(SEDLNDSMSDradius_title, SEDLNDSMSDradius_spinner);
		SEDLNDSMSDradius_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveSEDLNDSMSDradius();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(SEDLNDSMSDradius_spinner, "4, 64");

		SEDLNDSMSDradius_desc = new JLabel(
				"Modal radius of LND size distribution for the secondary LND mode (\u00B5m)");
		SEDLNDSMSDradius_desc
				.setToolTipText("Modal radius of LND size distribution for the secondary LND mode (\u00B5m)");
		hydroModel1ContentFormJp.add(SEDLNDSMSDradius_desc, "9, 64");

		SEDLNDSMSDvar_title = new JLabel("SED.LND.SM.SDvar :");
		SEDLNDSMSDvar_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(SEDLNDSMSDvar_title, "2, 66");

		SEDLNDSMSDvar_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"),
                new BigDecimal("0.00"), null, new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(SEDLNDSMSDvar_title, SEDLNDSMSDvar_spinner);
		SEDLNDSMSDvar_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveSEDLNDSMSDvar();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(SEDLNDSMSDvar_spinner, "4, 66");

		SEDLNDSMSDvar_desc = new JLabel(
				"Standard deviation of LND size distribution");
		SEDLNDSMSDvar_desc
				.setToolTipText("Standard deviation of LND size distribution for the secondary LND mode");
		SEDLNDSMSDvar_desc
			.setToolTipText("Standard deviation of LND size distribution for the secondary LND mode");
		hydroModel1ContentFormJp.add(SEDLNDSMSDvar_desc, "9, 66");

		SEDLNDSMrate_title = new JLabel("SED.LND.SM.rate :");
		SEDLNDSMrate_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(SEDLNDSMrate_title, "2, 68");

		SEDLNDSMrate_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"),
                new BigDecimal("0.00"), new BigDecimal("1.00"), new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(SEDLNDSMrate_title, SEDLNDSMrate_spinner);
		SEDLNDSMrate_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveSEDLNDSMrate();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(SEDLNDSMrate_spinner, "4, 68");

		SEDLNDSMrate_desc = new JLabel(
				"Ratio of the main mode in the global distribution (as a proportion of the secondary LND mode particles versus the global amount of particules)");
		SEDLNDSMrate_desc
				.setToolTipText("Ratio of the main mode in the global distribution (as a proportion of the secondary LND mode particles versus the global amount of particules)");
		hydroModel1ContentFormJp.add(SEDLNDSMrate_desc, "9, 68");
		
		HYDModel_choice161_separator = new JSeparator();
		HYDModel_choice161_separator.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		hydroModel1ContentFormJp.add(HYDModel_choice161_separator, "2, 70");

		HYDModel_choice16_title = new JLabel(
				"    |--> Tertiary mode of sediment : LND parameters");
		HYDModel_choice16_title.setHorizontalAlignment(SwingConstants.LEFT);
		HYDModel_choice16_title.setForeground(Color.BLUE);
		HYDModel_choice16_title.setFont(new Font("Tahoma", Font.PLAIN, 11));
		hydroModel1ContentFormJp.add(HYDModel_choice16_title, "4, 70");

		HYDModel_choice162_separator = new JSeparator();
		HYDModel_choice162_separator.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		hydroModel1ContentFormJp.add(HYDModel_choice162_separator, "9, 70");
		
		SEDLNDTMMRwa_title = new JLabel("SED.LND.TM.MRwa :");
		SEDLNDTMMRwa_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(SEDLNDTMMRwa_title, "2, 74");

		SEDLNDTMMRwa_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.000"),
                new BigDecimal("0.000"), null, new BigDecimal("0.001")));
        PropertiesManager.getInstance().register(SEDLNDTMMRwa_title, SEDLNDTMMRwa_spinner);
		SEDLNDTMMRwa_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveSEDLNDTMMRwa();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(SEDLNDTMMRwa_spinner, "4, 74");

		SEDLNDTMMRwa_desc = new JLabel(
				"Real part of the refractive index for the tertiary LND mode particles at simulation wavelength ");
		SEDLNDTMMRwa_desc
				.setToolTipText("Real part of the refractive index for the tertiary LND mode particles at simulation wavelength");
		hydroModel1ContentFormJp.add(SEDLNDTMMRwa_desc, "9, 74");

		SEDLNDTMMIwa_title = new JLabel("SED.LND.TM.MIwa :");
		SEDLNDTMMIwa_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(SEDLNDTMMIwa_title, "2, 76");

		SEDLNDTMMIwa_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.000"),
                new BigDecimal(-1), new BigDecimal("0.000"), new BigDecimal("0.001")));
        PropertiesManager.getInstance().register(SEDLNDTMMIwa_title, SEDLNDTMMIwa_spinner);
		SEDLNDTMMIwa_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveSEDLNDTMMIwa();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(SEDLNDTMMIwa_spinner, "4, 76");

		SEDLNDTMMIwa_desc = new JLabel(
				"Imaginary part of the refractive index for the tertiary LND mode particles at simulation wavelength");
		SEDLNDTMMIwa_desc
				.setToolTipText("Imaginary part of the refractive index for the tertiary LND mode particles at simulation wavelength");
		hydroModel1ContentFormJp.add(SEDLNDTMMIwa_desc, "9, 76");

		SEDLNDTMSDradius_title = new JLabel("SED.LND.TM.SDradius :");
		SEDLNDTMSDradius_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(SEDLNDTMSDradius_title, "2, 78");

		SEDLNDTMSDradius_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"),
                new BigDecimal("0.00"), null, new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(SEDLNDTMSDradius_title, SEDLNDTMSDradius_spinner);
		SEDLNDTMSDradius_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveSEDLNDTMSDradius();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(SEDLNDTMSDradius_spinner, "4, 78");

		SEDLNDTMSDradius_desc = new JLabel(
				"Modal radius of LND size distribution for the tertiary LND mode (\u00B5m)");
		SEDLNDTMSDradius_desc
				.setToolTipText("Modal radius of LND size distribution for the tertiary LND mode (\u00B5m)");
		hydroModel1ContentFormJp.add(SEDLNDTMSDradius_desc, "9, 78");

		SEDLNDTMSDvar_title = new JLabel("SED.LND.TM.SDvar :");
		SEDLNDTMSDvar_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(SEDLNDTMSDvar_title, "2, 80");

		SEDLNDTMSDvar_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"),
                new BigDecimal("0.00"), null, new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(SEDLNDTMSDvar_title, SEDLNDTMSDvar_spinner);
		SEDLNDTMSDvar_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveSEDLNDTMSDvar();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(SEDLNDTMSDvar_spinner, "4, 80");

		SEDLNDTMSDvar_desc = new JLabel(
				"Standard deviation of LND size distribution for the tertiary LND mode");
		SEDLNDTMSDvar_desc
				.setToolTipText("Standard deviation of LND size distribution for the tertiary LND mode");
		hydroModel1ContentFormJp.add(SEDLNDTMSDvar_desc, "9, 80");

		SEDLNDTMrate_title = new JLabel("SED.LND.TM.rate :");
		SEDLNDTMrate_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel1ContentFormJp.add(SEDLNDTMrate_title, "2, 82");

		SEDLNDTMrate_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"),
                new BigDecimal("0.00"), new BigDecimal("1.00"), new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(SEDLNDTMrate_title, SEDLNDTMrate_spinner);
		SEDLNDTMrate_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveSEDLNDTMrate();
				validateForm();
			}
		});
		hydroModel1ContentFormJp.add(SEDLNDTMrate_spinner, "4, 82");

		SEDLNDTMrate_desc = new JLabel(
				"Ratio of the main mode in the global distribution (as a proportion of the secondary LND mode particles versus the global amount of particules)");
		SEDLNDTMrate_desc
				.setToolTipText("Ratio of the main mode in the global distribution (as a proportion of the secondary LND mode particles versus the global amount of particules)");
		hydroModel1ContentFormJp.add(SEDLNDTMrate_desc, "9, 82");

		//////////////////////////////////////
		FormLayout hydroModel2ContentFormLayout = new FormLayout(new ColumnSpec[] {
				FormFactory.RELATED_GAP_COLSPEC,
				ColumnSpec.decode("max(67dlu;default)"),
				FormFactory.RELATED_GAP_COLSPEC,
				ColumnSpec.decode("max(145dlu;default)"),
				FormFactory.RELATED_GAP_COLSPEC,
				FormFactory.DEFAULT_COLSPEC,
				FormFactory.RELATED_GAP_COLSPEC,
				ColumnSpec.decode("max(4dlu;default)"),
				ColumnSpec.decode("default:grow"),},
			new RowSpec[] {
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,});
		
		JPanel hydroModel2ContentFormJp = new JPanel(hydroModel2ContentFormLayout);
		hydroModel2ContentFormJp.setBackground(Color.WHITE);
		
		HYDModel_choice2_title = new JLabel(
				"    |--> Hydrosols characterization by external data");
		HYDModel_choice2_title.setHorizontalAlignment(SwingConstants.LEFT);
		HYDModel_choice2_title.setForeground(Color.BLUE);
		HYDModel_choice2_title.setFont(new Font("Tahoma", Font.BOLD, 11));
		hydroModel2ContentFormJp.add(HYDModel_choice2_title, "2, 1, 3, 1");
		
		HYDModel_choice2_separator = new JSeparator();
		HYDModel_choice2_separator.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		hydroModel2ContentFormJp.add(HYDModel_choice2_separator, "9, 1");
		
		HYDExtData_title = new JLabel("HYD.ExtData *:");
		HYDExtData_title.setHorizontalAlignment(SwingConstants.RIGHT);
		hydroModel2ContentFormJp.add(HYDExtData_title, "2, 3");

		HYDExtData_textField = new JTextField();
		HYDExtData_textField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				saveHYDExtData();
				validateForm();
			}
		});
		hydroModel2ContentFormJp.add(HYDExtData_textField, "4, 3");

		HYDExtData_desc = new JLabel(
				"Userfile for sea particles phase function (full path)");
		HYDExtData_desc
				.setToolTipText("Userfile for sea particles phase function (full path)");
		hydroModel2ContentFormJp.add(HYDExtData_desc, "9, 3");
		
		hydroModelContentFormJp.add(hydroModel2ContentFormJp, "2");
		
		getFormFieldsPanel().add(hydroModelContentFormJp, BorderLayout.CENTER);
        }
        catch (Throwable ex)
        {
            log.error(ex);
        }
		
	}
	
	protected void onHYDModelChanged() {
		checkChanges();
	}



	public String getHYDDirMie() {
		HYDDirMie_textField.validate();
		return HYDDirMie_textField.getText();
	}

	private void saveHYDDirMie(){
		if(getHYDDirMie()!=null){
			hydrogroundModel.setHYDDirMie( getHYDDirMie() );
		}
	}

	public String getHYDMieLog() {
		HYDMieLog_textField.validate();
		return HYDMieLog_textField.getText();
	}

	private void saveHYDMieLog(){
		if(getHYDMieLog()!=null){
			hydrogroundModel.setHYDMieLog( getHYDMieLog() );
		}
	}

	public String getHYDLog() {
		HYDLog_textField.validate();
		return HYDLog_textField.getText();
	}

	private void saveHYDLog(){
		if(getHYDLog()!=null){
			hydrogroundModel.setHYDLog( getHYDLog() );
		}
	}

	public String getPHYTOResFile() {
		PHYTOResFile_textField.validate();
		return PHYTOResFile_textField.getText();
	}

	private void savePHYTOResFile(){
		if(getPHYTOResFile()!=null){
			hydrogroundModel.setPHYTOResFile( getPHYTOResFile() );
		}
	}

	public String getMLPResFile() {
		MLPResFile_textField.validate();
		return MLPResFile_textField.getText();
	}

	private void saveMLPResFile(){
		if(getMLPResFile()!=null){
			hydrogroundModel.setMLPResFile( getMLPResFile() );
		}
	}

	public Integer getHYDModel() {
		HYDModel_choice1_ckeckbox.validate();
		return HYDModel_choice1_ckeckbox.isSelected() ? 1 : 2;
	}

	private void saveHYDModel(){
		if(getHYDModel()!=null){
			hydrogroundModel.setHYDModel( getHYDModel() );
		}
	}

	public BigDecimal getPHYTOJDMRwa() {
		PHYTOJDMRwa_spinner.validate();
		return (BigDecimal) PHYTOJDMRwa_spinner.getValue();
	}

	private void savePHYTOJDMRwa(){
		if(getPHYTOJDMRwa()!=null){
			hydrogroundModel.setPHYTOJDMRwa( getPHYTOJDMRwa() );
		}
	}

	public BigDecimal getPHYTOJDMIwa() {
		PHYTOJDMIwa_spinner.validate();
		return (BigDecimal) PHYTOJDMIwa_spinner.getValue();
	}

	private void savePHYTOJDMIwa(){
		if(getPHYTOJDMIwa()!=null){
			hydrogroundModel.setPHYTOJDMIwa( getPHYTOJDMIwa() );
		}
	}

	public BigDecimal getPHYTOJDslope() {
		PHYTOJDslope_spinner.validate();
		return (BigDecimal) PHYTOJDslope_spinner.getValue();
	}

	private void savePHYTOJDslope(){
		if(getPHYTOJDslope()!=null){
			hydrogroundModel.setPHYTOJDslope( getPHYTOJDslope() );
		}
	}

	public BigDecimal getPHYTOJDrmin() {
		PHYTOJDrmin_spinner.validate();
		return (BigDecimal) PHYTOJDrmin_spinner.getValue();
	}

	private void savePHYTOJDrmin(){
		if(getPHYTOJDrmin()!=null){
			hydrogroundModel.setPHYTOJDrmin( getPHYTOJDrmin() );
		}
	}

	public BigDecimal getPHYTOJDrmax() {
		PHYTOJDrmax_spinner.validate();
		return (BigDecimal) PHYTOJDrmax_spinner.getValue();
	}

	private void savePHYTOJDrmax(){
		if(getPHYTOJDrmax()!=null){
			hydrogroundModel.setPHYTOJDrmax( getPHYTOJDrmax() );
		}
	}

	public BigDecimal getPHYTOJDrate() {
		PHYTOJDrate_spinner.validate();
		return (BigDecimal) PHYTOJDrate_spinner.getValue();
	}

	private void savePHYTOJDrate(){
		if(getPHYTOJDrate()!=null){
			hydrogroundModel.setPHYTOJDrate( getPHYTOJDrate() );
		}
	}

	public BigDecimal getPHYTOLNDSMMRwa() {
		PHYTOLNDSMMRwa_spinner.validate();
		return (BigDecimal) PHYTOLNDSMMRwa_spinner.getValue();
	}

	private void savePHYTOLNDSMMRwa(){
		if(getPHYTOLNDSMMRwa()!=null){
			hydrogroundModel.setPHYTOLNDSMMRwa( getPHYTOLNDSMMRwa() );
		}
	}

	public BigDecimal getPHYTOLNDSMMIwa() {
		PHYTOLNDSMMIwa_spinner.validate();
		return (BigDecimal) PHYTOLNDSMMIwa_spinner.getValue();
	}

	private void savePHYTOLNDSMMIwa(){
		if(getPHYTOLNDSMMIwa()!=null){
			hydrogroundModel.setPHYTOLNDSMMIwa( getPHYTOLNDSMMIwa() );
		}
	}

	public BigDecimal getPHYTOLNDSMSDradius() {
		PHYTOLNDSMSDradius_spinner.validate();
		return (BigDecimal) PHYTOLNDSMSDradius_spinner.getValue();
	}

	private void savePHYTOLNDSMSDradius(){
		if(getPHYTOLNDSMSDradius()!=null){
			hydrogroundModel.setPHYTOLNDSMSDradius( getPHYTOLNDSMSDradius() );
		}
	}

	public BigDecimal getPHYTOLNDSMSDvar() {
		PHYTOLNDSMSDvar_spinner.validate();
		return (BigDecimal) PHYTOLNDSMSDvar_spinner.getValue();
	}

	private void savePHYTOLNDSMSDvar(){
		if(getPHYTOLNDSMSDvar()!=null){
			hydrogroundModel.setPHYTOLNDSMSDvar( getPHYTOLNDSMSDvar() );
		}
	}

	public BigDecimal getPHYTOLNDSMrate() {
		PHYTOLNDSMrate_spinner.validate();
		return (BigDecimal) PHYTOLNDSMrate_spinner.getValue();
	}

	private void savePHYTOLNDSMrate(){
		if(getPHYTOLNDSMrate()!=null){
			hydrogroundModel.setPHYTOLNDSMrate( getPHYTOLNDSMrate() );
		}
	}

	public BigDecimal getPHYTOLNDTMMRwa() {
		PHYTOLNDTMMRwa_spinner.validate();
		return (BigDecimal) PHYTOLNDTMMRwa_spinner.getValue();
	}

	private void savePHYTOLNDTMMRwa(){
		if(getPHYTOLNDTMMRwa()!=null){
			hydrogroundModel.setPHYTOLNDTMMRwa( getPHYTOLNDTMMRwa() );
		}
	}

	public BigDecimal getPHYTOLNDTMMIwa() {
		PHYTOLNDTMMIwa_spinner.validate();
		return (BigDecimal) PHYTOLNDTMMIwa_spinner.getValue();
	}

	private void savePHYTOLNDTMMIwa(){
		if(getPHYTOLNDTMMIwa()!=null){
			hydrogroundModel.setPHYTOLNDTMMIwa( getPHYTOLNDTMMIwa() );
		}
	}

	public BigDecimal getPHYTOLNDTMSDradius() {
		PHYTOLNDTMSDradius_spinner.validate();
		return (BigDecimal) PHYTOLNDTMSDradius_spinner.getValue();
	}

	private void savePHYTOLNDTMSDradius(){
		if(getPHYTOLNDTMSDradius()!=null){
			hydrogroundModel.setPHYTOLNDTMSDradius( getPHYTOLNDTMSDradius() );
		}
	}

	public BigDecimal getPHYTOLNDTMSDvar() {
		PHYTOLNDTMSDvar_spinner.validate();
		return (BigDecimal) PHYTOLNDTMSDvar_spinner.getValue();
	}

	private void savePHYTOLNDTMSDvar(){
		if(getPHYTOLNDTMSDvar()!=null){
			hydrogroundModel.setPHYTOLNDTMSDvar( getPHYTOLNDTMSDvar() );
		}
	}

	public BigDecimal getPHYTOLNDTMrate() {
		PHYTOLNDTMrate_spinner.validate();
		return (BigDecimal) PHYTOLNDTMrate_spinner.getValue();
	}

	private void savePHYTOLNDTMrate(){
		if(getPHYTOLNDTMrate()!=null){
			hydrogroundModel.setPHYTOLNDTMrate( getPHYTOLNDTMrate() );
		}
	}

	public BigDecimal getSEDJDMRwa() {
		SEDJDMRwa_spinner.validate();
		return (BigDecimal) SEDJDMRwa_spinner.getValue();
	}

	private void saveSEDJDMRwa(){
		if(getSEDJDMRwa()!=null){
			hydrogroundModel.setSEDJDMRwa( getSEDJDMRwa() );
		}
	}

	public BigDecimal getSEDJDMIwa() {
		SEDJDMIwa_spinner.validate();
		return (BigDecimal) SEDJDMIwa_spinner.getValue();
	}

	private void saveSEDJDMIwa(){
		if(getSEDJDMIwa()!=null){
			hydrogroundModel.setSEDJDMIwa( getSEDJDMIwa() );
		}
	}

	public BigDecimal getSEDJDslope() {
		SEDJDslope_spinner.validate();
		return (BigDecimal) SEDJDslope_spinner.getValue();
	}

	private void saveSEDJDslope(){
		if(getSEDJDslope()!=null){
			hydrogroundModel.setSEDJDslope( getSEDJDslope() );
		}
	}

	public BigDecimal getSEDJDrmin() {
		SEDJDrmin_spinner.validate();
		return (BigDecimal) SEDJDrmin_spinner.getValue();
	}

	private void saveSEDJDrmin(){
		if(getSEDJDrmin()!=null){
			hydrogroundModel.setSEDJDrmin( getSEDJDrmin() );
		}
	}

	public BigDecimal getSEDJDrmax() {
		SEDJDrmax_spinner.validate();
		return (BigDecimal) SEDJDrmax_spinner.getValue();
	}

	private void saveSEDJDrmax(){
		if(getSEDJDrmax()!=null){
			hydrogroundModel.setSEDJDrmax( getSEDJDrmax() );
		}
	}

	public BigDecimal getSEDJDrate() {
		SEDJDrate_spinner.validate();
		return (BigDecimal) SEDJDrate_spinner.getValue();
	}

	private void saveSEDJDrate(){
		if(getSEDJDrate()!=null){
			hydrogroundModel.setSEDJDrate( getSEDJDrate() );
		}
	}

	public BigDecimal getSEDLNDSMMRwa() {
		SEDLNDSMMRwa_spinner.validate();
		return (BigDecimal) SEDLNDSMMRwa_spinner.getValue();
	}

	private void saveSEDLNDSMMRwa(){
		if(getSEDLNDSMMRwa()!=null){
			hydrogroundModel.setSEDLNDSMMRwa( getSEDLNDSMMRwa() );
		}
	}

	public BigDecimal getSEDLNDSMMIwa() {
		SEDLNDSMMIwa_spinner.validate();
		return (BigDecimal) SEDLNDSMMIwa_spinner.getValue();
	}

	private void saveSEDLNDSMMIwa(){
		if(getSEDLNDSMMIwa()!=null){
			hydrogroundModel.setSEDLNDSMMIwa( getSEDLNDSMMIwa() );
		}
	}

	public BigDecimal getSEDLNDSMSDradius() {
		SEDLNDSMSDradius_spinner.validate();
		return (BigDecimal) SEDLNDSMSDradius_spinner.getValue();
	}

	private void saveSEDLNDSMSDradius(){
		if(getSEDLNDSMSDradius()!=null){
			hydrogroundModel.setSEDLNDSMSDradius( getSEDLNDSMSDradius() );
		}
	}

	public BigDecimal getSEDLNDSMSDvar() {
		SEDLNDSMSDvar_spinner.validate();
		return (BigDecimal) SEDLNDSMSDvar_spinner.getValue();
	}

	private void saveSEDLNDSMSDvar(){
		if(getSEDLNDSMSDvar()!=null){
			hydrogroundModel.setSEDLNDSMSDvar( getSEDLNDSMSDvar() );
		}
	}

	public BigDecimal getSEDLNDSMrate() {
		SEDLNDSMrate_spinner.validate();
		return (BigDecimal) SEDLNDSMrate_spinner.getValue();
	}

	private void saveSEDLNDSMrate(){
		if(getSEDLNDSMrate()!=null){
			hydrogroundModel.setSEDLNDSMrate( getSEDLNDSMrate() );
		}
	}

	public BigDecimal getSEDLNDTMMRwa() {
		SEDLNDTMMRwa_spinner.validate();
		return (BigDecimal) SEDLNDTMMRwa_spinner.getValue();
	}

	private void saveSEDLNDTMMRwa(){
		if(getSEDLNDTMMRwa()!=null){
			hydrogroundModel.setSEDLNDTMMRwa( getSEDLNDTMMRwa() );
		}
	}

	public BigDecimal getSEDLNDTMMIwa() {
		SEDLNDTMMIwa_spinner.validate();
		return (BigDecimal) SEDLNDTMMIwa_spinner.getValue();
	}

	private void saveSEDLNDTMMIwa(){
		if(getSEDLNDTMMIwa()!=null){
			hydrogroundModel.setSEDLNDTMMIwa( getSEDLNDTMMIwa() );
		}
	}

	public BigDecimal getSEDLNDTMSDradius() {
		SEDLNDTMSDradius_spinner.validate();
		return (BigDecimal) SEDLNDTMSDradius_spinner.getValue();
	}

	private void saveSEDLNDTMSDradius(){
		if(getSEDLNDTMSDradius()!=null){
			hydrogroundModel.setSEDLNDTMSDradius( getSEDLNDTMSDradius() );
		}
	}

	public BigDecimal getSEDLNDTMSDvar() {
		SEDLNDTMSDvar_spinner.validate();
		return (BigDecimal) SEDLNDTMSDvar_spinner.getValue();
	}

	private void saveSEDLNDTMSDvar(){
		if(getSEDLNDTMSDvar()!=null){
			hydrogroundModel.setSEDLNDTMSDvar( getSEDLNDTMSDvar() );
		}
	}

	public BigDecimal getSEDLNDTMrate() {
		SEDLNDTMrate_spinner.validate();
		return (BigDecimal) SEDLNDTMrate_spinner.getValue();
	}

	private void saveSEDLNDTMrate(){
		if(getSEDLNDTMrate()!=null){
			hydrogroundModel.setSEDLNDTMrate( getSEDLNDTMrate() );
		}
	}

	public String getHYDExtData() {
		HYDExtData_textField.validate();
		return (String) HYDExtData_textField.getText();
	}

	private void saveHYDExtData(){
		if(getHYDExtData()!=null){
			hydrogroundModel.setHYDExtData( getHYDExtData() );
		}
	}
	
	protected void setHYDDirMieVisible(boolean isVisible_) {
		HYDDirMie_title.setVisible(isVisible_);
		HYDDirMie_textField.setVisible(isVisible_);
		HYDDirMie_desc.setVisible(isVisible_);
	}

	protected void setHYDMieLogVisible(boolean isVisible_) {
		HYDMieLog_title.setVisible(isVisible_);
		HYDMieLog_textField.setVisible(isVisible_);
		HYDMieLog_desc.setVisible(isVisible_);
	}

	protected void setHYDLogVisible(boolean isVisible_) {
		HYDLog_title.setVisible(isVisible_);
		HYDLog_textField.setVisible(isVisible_);
		HYDLog_desc.setVisible(isVisible_);
	}

	protected void setPHYTOResFileVisible(boolean isVisible_) {
		PHYTOResFile_title.setVisible(isVisible_);
		PHYTOResFile_textField.setVisible(isVisible_);
		PHYTOResFile_desc.setVisible(isVisible_);
	}

	protected void setMLPResFileVisible(boolean isVisible_) {
		MLPResFile_title.setVisible(isVisible_);
		MLPResFile_textField.setVisible(isVisible_);
		MLPResFile_desc.setVisible(isVisible_);
	}

	protected void setHYDModelVisible(boolean isVisible_) {
		HYDModel_title.setVisible(isVisible_);
		HYDModel_choice1_ckeckbox.setVisible(isVisible_);
		HYDModel_choice2_ckeckbox.setVisible(isVisible_);
		HYDModel_checkboxJPanel.setVisible(isVisible_);
		HYDModel_desc.setVisible(isVisible_);
	}

	protected void setPHYTOJDMRwaVisible(boolean isVisible_) {
		PHYTOJDMRwa_title.setVisible(isVisible_);
		PHYTOJDMRwa_spinner.setVisible(isVisible_);
		PHYTOJDMRwa_desc.setVisible(isVisible_);
	}

	protected void setPHYTOJDMIwaVisible(boolean isVisible_) {
		PHYTOJDMIwa_title.setVisible(isVisible_);
		PHYTOJDMIwa_spinner.setVisible(isVisible_);
		PHYTOJDMIwa_desc.setVisible(isVisible_);
	}

	protected void setPHYTOJDslopeVisible(boolean isVisible_) {
		PHYTOJDslope_title.setVisible(isVisible_);
		PHYTOJDslope_spinner.setVisible(isVisible_);
		PHYTOJDslope_desc.setVisible(isVisible_);
	}

	protected void setPHYTOJDrminVisible(boolean isVisible_) {
		PHYTOJDrmin_title.setVisible(isVisible_);
		PHYTOJDrmin_spinner.setVisible(isVisible_);
		PHYTOJDrmin_desc.setVisible(isVisible_);
	}

	protected void setPHYTOJDrmaxVisible(boolean isVisible_) {
		PHYTOJDrmax_title.setVisible(isVisible_);
		PHYTOJDrmax_spinner.setVisible(isVisible_);
		PHYTOJDrmax_desc.setVisible(isVisible_);
	}

	protected void setPHYTOJDrateVisible(boolean isVisible_) {
		PHYTOJDrate_title.setVisible(isVisible_);
		PHYTOJDrate_spinner.setVisible(isVisible_);
		PHYTOJDrate_desc.setVisible(isVisible_);
	}

	protected void setPHYTOLNDSMMRwaVisible(boolean isVisible_) {
		PHYTOLNDSMMRwa_title.setVisible(isVisible_);
		PHYTOLNDSMMRwa_spinner.setVisible(isVisible_);
		PHYTOLNDSMMRwa_desc.setVisible(isVisible_);
	}

	protected void setPHYTOLNDSMMIwaVisible(boolean isVisible_) {
		PHYTOLNDSMMIwa_title.setVisible(isVisible_);
		PHYTOLNDSMMIwa_spinner.setVisible(isVisible_);
		PHYTOLNDSMMIwa_desc.setVisible(isVisible_);
	}

	protected void setPHYTOLNDSMSDradiusVisible(boolean isVisible_) {
		PHYTOLNDSMSDradius_title.setVisible(isVisible_);
		PHYTOLNDSMSDradius_spinner.setVisible(isVisible_);
		PHYTOLNDSMSDradius_desc.setVisible(isVisible_);
	}

	protected void setPHYTOLNDSMSDvarVisible(boolean isVisible_) {
		PHYTOLNDSMSDvar_title.setVisible(isVisible_);
		PHYTOLNDSMSDvar_spinner.setVisible(isVisible_);
		PHYTOLNDSMSDvar_desc.setVisible(isVisible_);
	}

	protected void setPHYTOLNDSMrateVisible(boolean isVisible_) {
		PHYTOLNDSMrate_title.setVisible(isVisible_);
		PHYTOLNDSMrate_spinner.setVisible(isVisible_);
		PHYTOLNDSMrate_desc.setVisible(isVisible_);
	}

	protected void setPHYTOLNDTMMRwaVisible(boolean isVisible_) {
		PHYTOLNDTMMRwa_title.setVisible(isVisible_);
		PHYTOLNDTMMRwa_spinner.setVisible(isVisible_);
		PHYTOLNDTMMRwa_desc.setVisible(isVisible_);
	}

	protected void setPHYTOLNDTMMIwaVisible(boolean isVisible_) {
		PHYTOLNDTMMIwa_title.setVisible(isVisible_);
		PHYTOLNDTMMIwa_spinner.setVisible(isVisible_);
		PHYTOLNDTMMIwa_desc.setVisible(isVisible_);
	}

	protected void setPHYTOLNDTMSDradiusVisible(boolean isVisible_) {
		PHYTOLNDTMSDradius_title.setVisible(isVisible_);
		PHYTOLNDTMSDradius_spinner.setVisible(isVisible_);
		PHYTOLNDTMSDradius_desc.setVisible(isVisible_);
	}

	protected void setPHYTOLNDTMSDvarVisible(boolean isVisible_) {
		PHYTOLNDTMSDvar_title.setVisible(isVisible_);
		PHYTOLNDTMSDvar_spinner.setVisible(isVisible_);
		PHYTOLNDTMSDvar_desc.setVisible(isVisible_);
	}

	protected void setPHYTOLNDTMrateVisible(boolean isVisible_) {
		PHYTOLNDTMrate_title.setVisible(isVisible_);
		PHYTOLNDTMrate_spinner.setVisible(isVisible_);
		PHYTOLNDTMrate_desc.setVisible(isVisible_);
	}

	protected void setSEDJDMRwaVisible(boolean isVisible_) {
		SEDJDMRwa_title.setVisible(isVisible_);
		SEDJDMRwa_spinner.setVisible(isVisible_);
		SEDJDMRwa_desc.setVisible(isVisible_);
	}

	protected void setSEDJDMIwaVisible(boolean isVisible_) {
		SEDJDMIwa_title.setVisible(isVisible_);
		SEDJDMIwa_spinner.setVisible(isVisible_);
		SEDJDMIwa_desc.setVisible(isVisible_);
	}

	protected void setSEDJDslopeVisible(boolean isVisible_) {
		SEDJDslope_title.setVisible(isVisible_);
		SEDJDslope_spinner.setVisible(isVisible_);
		SEDJDslope_desc.setVisible(isVisible_);
	}

	protected void setSEDJDrminVisible(boolean isVisible_) {
		SEDJDrmin_title.setVisible(isVisible_);
		SEDJDrmin_spinner.setVisible(isVisible_);
		SEDJDrmin_desc.setVisible(isVisible_);
	}

	protected void setSEDJDrmaxVisible(boolean isVisible_) {
		SEDJDrmax_title.setVisible(isVisible_);
		SEDJDrmax_spinner.setVisible(isVisible_);
		SEDJDrmax_desc.setVisible(isVisible_);
	}

	protected void setSEDJDrateVisible(boolean isVisible_) {
		SEDJDrate_title.setVisible(isVisible_);
		SEDJDrate_spinner.setVisible(isVisible_);
		SEDJDrate_desc.setVisible(isVisible_);
	}

	protected void setSEDLNDSMMRwaVisible(boolean isVisible_) {
		SEDLNDSMMRwa_title.setVisible(isVisible_);
		SEDLNDSMMRwa_spinner.setVisible(isVisible_);
		SEDLNDSMMRwa_desc.setVisible(isVisible_);
	}

	protected void setSEDLNDSMMIwaVisible(boolean isVisible_) {
		SEDLNDSMMIwa_title.setVisible(isVisible_);
		SEDLNDSMMIwa_spinner.setVisible(isVisible_);
		SEDLNDSMMIwa_desc.setVisible(isVisible_);
	}

	protected void setSEDLNDSMSDradiusVisible(boolean isVisible_) {
		SEDLNDSMSDradius_title.setVisible(isVisible_);
		SEDLNDSMSDradius_spinner.setVisible(isVisible_);
		SEDLNDSMSDradius_desc.setVisible(isVisible_);
	}

	protected void setSEDLNDSMSDvarVisible(boolean isVisible_) {
		SEDLNDSMSDvar_title.setVisible(isVisible_);
		SEDLNDSMSDvar_spinner.setVisible(isVisible_);
		SEDLNDSMSDvar_desc.setVisible(isVisible_);
	}

	protected void setSEDLNDSMrateVisible(boolean isVisible_) {
		SEDLNDSMrate_title.setVisible(isVisible_);
		SEDLNDSMrate_spinner.setVisible(isVisible_);
		SEDLNDSMrate_desc.setVisible(isVisible_);
	}

	protected void setSEDLNDTMMRwaVisible(boolean isVisible_) {
		SEDLNDTMMRwa_title.setVisible(isVisible_);
		SEDLNDTMMRwa_spinner.setVisible(isVisible_);
		SEDLNDTMMRwa_desc.setVisible(isVisible_);
	}

	protected void setSEDLNDTMMIwaVisible(boolean isVisible_) {
		SEDLNDTMMIwa_title.setVisible(isVisible_);
		SEDLNDTMMIwa_spinner.setVisible(isVisible_);
		SEDLNDTMMIwa_desc.setVisible(isVisible_);
	}

	protected void setSEDLNDTMSDradiusVisible(boolean isVisible_) {
		SEDLNDTMSDradius_title.setVisible(isVisible_);
		SEDLNDTMSDradius_spinner.setVisible(isVisible_);
		SEDLNDTMSDradius_desc.setVisible(isVisible_);
	}

	protected void setSEDLNDTMSDvarVisible(boolean isVisible_) {
		SEDLNDTMSDvar_title.setVisible(isVisible_);
		SEDLNDTMSDvar_spinner.setVisible(isVisible_);
		SEDLNDTMSDvar_desc.setVisible(isVisible_);
	}

	protected void setSEDLNDTMrateVisible(boolean isVisible_) {
		SEDLNDTMrate_title.setVisible(isVisible_);
		SEDLNDTMrate_spinner.setVisible(isVisible_);
		SEDLNDTMrate_desc.setVisible(isVisible_);
	}

	protected void setHYDExtDataVisible(boolean isVisible_) {
		HYDExtData_title.setVisible(isVisible_);
		HYDExtData_textField.setVisible(isVisible_);
		HYDExtData_desc.setVisible(isVisible_);
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
				PreferencesFactory.getInstance().getHydrogroundModel().reset();
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
		CardLayout cl = (CardLayout)(hydroModelContentFormJp.getLayout());
		cl.show(hydroModelContentFormJp, "" + getHYDModel());
		
		BigDecimal phytoChlValue = atmosphericAndSeaProfiles.getPhytoChl();
        boolean phytoFieldsEnabled = getHYDModel() == 1;
		setPHYTOJDMRwaVisible( phytoFieldsEnabled );
		setPHYTOJDMIwaVisible( phytoFieldsEnabled );
		setPHYTOJDslopeVisible( phytoFieldsEnabled );
		setPHYTOJDrminVisible( phytoFieldsEnabled );
		setPHYTOJDrmaxVisible( phytoFieldsEnabled );
		setPHYTOJDrateVisible( phytoFieldsEnabled );
		setPHYTOLNDSMMRwaVisible( phytoFieldsEnabled );
		setPHYTOLNDSMMIwaVisible( phytoFieldsEnabled );
		setPHYTOLNDSMSDradiusVisible( phytoFieldsEnabled );
		setPHYTOLNDSMSDvarVisible( phytoFieldsEnabled );
		setPHYTOLNDSMrateVisible( phytoFieldsEnabled );
		setPHYTOLNDTMMRwaVisible( phytoFieldsEnabled );
		setPHYTOLNDTMMIwaVisible( phytoFieldsEnabled );
		setPHYTOLNDTMSDradiusVisible( phytoFieldsEnabled );
		setPHYTOLNDTMSDvarVisible( phytoFieldsEnabled );
		setPHYTOLNDTMrateVisible( phytoFieldsEnabled );
		
		BigDecimal sedCsedValue = atmosphericAndSeaProfiles.getSEDCsed();
		setSEDJDMRwaVisible( phytoFieldsEnabled );
		setSEDJDMIwaVisible( phytoFieldsEnabled );
		setSEDJDslopeVisible( phytoFieldsEnabled );
		setSEDJDrminVisible( phytoFieldsEnabled );
		setSEDJDrmaxVisible( phytoFieldsEnabled );
		setSEDJDrateVisible( phytoFieldsEnabled );
		setSEDLNDSMMRwaVisible( phytoFieldsEnabled );
		setSEDLNDSMMIwaVisible( phytoFieldsEnabled );
		setSEDLNDSMSDradiusVisible( phytoFieldsEnabled );
		setSEDLNDSMSDvarVisible( phytoFieldsEnabled );
		setSEDLNDSMrateVisible( phytoFieldsEnabled );
		setSEDLNDTMMRwaVisible( phytoFieldsEnabled );
		setSEDLNDTMMIwaVisible( phytoFieldsEnabled );
		setSEDLNDTMSDradiusVisible( phytoFieldsEnabled );
		setSEDLNDTMSDvarVisible( phytoFieldsEnabled );
		setSEDLNDTMrateVisible( phytoFieldsEnabled );
		

		HYDModel_choice1_title.setVisible( phytoFieldsEnabled );
//		HYDModel_choice1_separator.setVisible( getHYDModel() == 1 && phytoChlValue > 0.0 );
		
		HYDModel_choice11_title.setVisible( phytoFieldsEnabled );
		HYDModel_choice111_separator.setVisible( phytoFieldsEnabled );
		HYDModel_choice112_separator.setVisible( phytoFieldsEnabled );
		
		HYDModel_choice12_title.setVisible( phytoFieldsEnabled );
		HYDModel_choice121_separator.setVisible( phytoFieldsEnabled );
		HYDModel_choice122_separator.setVisible( phytoFieldsEnabled );
		
		HYDModel_choice13_title.setVisible( phytoFieldsEnabled );
		HYDModel_choice131_separator.setVisible( phytoFieldsEnabled );
		HYDModel_choice132_separator.setVisible( phytoFieldsEnabled );
		
		HYDModel_choice14_title.setVisible( phytoFieldsEnabled );
		HYDModel_choice141_separator.setVisible( phytoFieldsEnabled );
		HYDModel_choice142_separator.setVisible( phytoFieldsEnabled );
		
		HYDModel_choice15_title.setVisible( phytoFieldsEnabled );
		HYDModel_choice151_separator.setVisible( phytoFieldsEnabled );
		HYDModel_choice152_separator.setVisible( phytoFieldsEnabled );
		
		HYDModel_choice16_title.setVisible( phytoFieldsEnabled );
		HYDModel_choice161_separator.setVisible( phytoFieldsEnabled );
		HYDModel_choice162_separator.setVisible( phytoFieldsEnabled );
		
		HYDModel_choice1_mainSeparator.setVisible( phytoFieldsEnabled );
		
		HYDModel_choice2_title.setVisible( getHYDModel() == 2 );
		HYDModel_choice2_separator.setVisible( getHYDModel() == 2 );

		setHYDExtDataVisible( getHYDModel() == 2 );
	}

}
