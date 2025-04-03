package osoaa.usl.forms;

import java.awt.*;
import java.awt.event.*;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

import javax.swing.*;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.DefaultFormatter;
import javax.swing.text.MaskFormatter;

import org.apache.log4j.Logger;

import osoaa.bll.domain.ViewLevelEnum;
import osoaa.bll.exception.InitException;
import osoaa.bll.preferences.IOutputSpecificities;
import osoaa.bll.preferences.PreferencesFactory;
import osoaa.bll.properties.PropertiesManager;
import osoaa.usl.common.ui.forms.FormUtils;

import com.jgoodies.forms.factories.DefaultComponentFactory;
import com.jgoodies.forms.factories.FormFactory;
import com.jgoodies.forms.layout.ColumnSpec;
import com.jgoodies.forms.layout.FormLayout;
import com.jgoodies.forms.layout.RowSpec;
import osoaa.usl.common.ui.jspinner.JSpinnerOptionalRangedValue;
import osoaa.usl.common.ui.jspinner.JSpinnerOptionalValue;
import osoaa.usl.common.ui.jspinner.JSpinnerRangedValue;
import osoaa.usl.common.ui.jspinner.SpinnerBigDecimalModel;

public class OutputSpecificitiesJPanel extends AbstractForm {
    private final JLabel OSOAALog_title;
    private final JTextField OSOAALog_textField;
    private final JLabel OSOAALog_desc;
    private final JComboBox ViewLevel_combobox;
    private final JLabel ViewLevel_title;
    /**
	 * 
	 */
	private Logger log = Logger.getLogger(this.getClass().getCanonicalName());
	
	private static final long serialVersionUID = 1L;
	
	private IOutputSpecificities outputSpecificities;
	private JLabel OSOAAViewVZA_desc;
	private JLabel OSOAAViewPhi_title;
	private JLabel OSOAAResFilevsVZA_title;
	private JLabel OSOAAViewZ_desc;
	private JLabel OSOAAViewPhi_desc;
	private JSpinner OSOAAViewPhi_spinner;

	private JLabel OSOAAViewZ_title;

	private JLabel OSOAAViewVZA_title;
	private JTextField OSOAAResFilevsVZA_textField;
	private JSpinner SOSIGmax_spinner;
	private JTextField SOSLog_textField;
	private JTextField SOSResFileBin_textField;
	private JLabel OSOAAResFilevsVZA_desc;
	private JLabel SOSLog_desc;
	private JLabel SOSResFileBin_desc;
	private JLabel SOSIGmax_title;
	private JLabel SOSLog_title;
	private JLabel SOSResFileBin_title;
	private JSpinner OSOAAViewZ_spinner;
	private JSpinner OSOAAViewVZA_spinner;
	private JLabel lblChoiceOf;
	private JLabel lblOutputFiles;
	private JSeparator separator;
	private JSeparator separator_1;
	private JLabel SOSIGmax_desc;
	private JTextField OSOAAResFilevsZ_textField;
	private JTextField OSOAAResFileAdv_textField;
	private JLabel OSOAAResFilevsZ_desc;
	private JLabel OSOAAResFileAdv_desc;
	private JLabel OSOAAResFilevsZ_title;
	private JLabel OSOAAResFileAdv_title;
	
	private JTextField OSOAAResFileAdvDown_textField;
	private JLabel OSOAAResFileAdvDown_desc;
	private JLabel OSOAAResFileAdvDown_title;

	private Integer currentY = 1;

	public void init() {
		outputSpecificities = PreferencesFactory.getInstance().getOutputSpecificities();
		
		OSOAAViewPhi_spinner.setValue( outputSpecificities.getOSOAAViewPhi());
		OSOAAViewZ_spinner.setValue( outputSpecificities.getOSOAAViewZ());
		OSOAAViewVZA_spinner.setValue( outputSpecificities.getOSOAAViewVZA());
		SOSIGmax_spinner.setValue( outputSpecificities.getSOSIGmax());
		SOSLog_textField.setText( outputSpecificities.getSOSLog());
		SOSResFileBin_textField.setText( outputSpecificities.getSOSResFileBin());
		OSOAAResFilevsVZA_textField.setText( outputSpecificities.getOSOAAResFilevsVZA());
		OSOAAResFilevsZ_textField.setText( outputSpecificities.getOSOAAResFilevsZ());
		OSOAAResFileAdv_textField.setText( outputSpecificities.getOSOAAResFileAdv());
        OSOAAResFileAdvDown_textField.setText( outputSpecificities.getOSOAAResFileAdvDown());
        OSOAALog_textField.setText(outputSpecificities.getOSOAALog());

        ViewLevel_combobox.setSelectedIndex(Math.max(0, outputSpecificities.getOSOAAViewLevel() - 1));

        if(outputSpecificities.getOSOAAViewLevel() == 5)
        {
            OSOAAViewZ_spinner.setEnabled(true);
            outputSpecificities.enablePreferences("OSOAA.View.Z");
            saveViewModel();
        }
        else
        {
            OSOAAViewZ_spinner.setEnabled(false);
            outputSpecificities.disablePreferences("OSOAA.View.Z");
            saveViewModel();
        }

		validateForm();
	}
	
	private String getStr(String mask) {
		return String.format(mask,currentY);
	}

	private void validateFormInitToFalse() {
		FormUtils.setFieldState(false, OSOAAViewPhi_title);
		FormUtils.setFieldState(false, OSOAAViewZ_title);
		FormUtils.setFieldState(false, OSOAAViewVZA_title);
		FormUtils.setFieldState(false, SOSIGmax_title);
		FormUtils.setFieldState(false, SOSLog_title);
		FormUtils.setFieldState(false, SOSResFileBin_title);
		FormUtils.setFieldState(false, OSOAAResFilevsVZA_title);
		FormUtils.setFieldState(false, OSOAAResFilevsZ_title);
		FormUtils.setFieldState(false, OSOAAResFileAdv_title);
		FormUtils.setFieldState(false, OSOAAResFileAdvDown_title);
        FormUtils.setFieldState(false, ViewLevel_title);
	}
	private boolean validateHydrogroundModelForm() {
		validateFormInitToFalse();
		boolean isValid = true;

		isValid &= FormUtils.setFieldState(getOSOAAViewPhi() == null, OSOAAViewPhi_title);
		isValid &= FormUtils.setFieldState(getOSOAAViewZ() == null, OSOAAViewZ_title);
		isValid &= FormUtils.setFieldState(getSOSResFileBin() == null || getSOSResFileBin().trim().length()<=0, SOSResFileBin_title);
		isValid &= FormUtils.setFieldState(getOSOAAResFilevsVZA() == null || getOSOAAResFilevsVZA().trim().length()<=0, OSOAAResFilevsVZA_title);

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
	public OutputSpecificitiesJPanel() {
		super("Output specificities");
		
		FormLayout formLayout = new FormLayout(new ColumnSpec[] {
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
		getFormFieldsPanel().setLayout(formLayout);
		
		SOSIGmax_title = new JLabel("SOS.IGmax :");
		SOSIGmax_title.setToolTipText("");
		getFormFieldsPanel().add(SOSIGmax_title, getStr("2, %d, right, default"));

        SOSIGmax_spinner = new JSpinnerRangedValue(new SpinnerNumberModel(Integer.valueOf(1),
		Integer.valueOf(1), null, Integer.valueOf(1)));
        PropertiesManager.getInstance().register(SOSIGmax_title, SOSIGmax_spinner);
		SOSIGmax_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
                    saveSOSIGmax();
                    validateForm();
			}
		});

		getFormFieldsPanel().add(SOSIGmax_spinner, getStr("4, %d, fill, default"));
		
		SOSIGmax_desc = new JLabel("Scattering maximum order");
		SOSIGmax_desc.setToolTipText("Scattering maximum order");
		getFormFieldsPanel().add(SOSIGmax_desc, getStr("9, %d"));

		currentY += 2;
		lblChoiceOf = new JLabel("    |--> Choice of output type");
		lblChoiceOf.setHorizontalAlignment(SwingConstants.LEFT);
		lblChoiceOf.setForeground(Color.BLUE);
		lblChoiceOf.setFont(new Font("Tahoma", Font.BOLD, 11));
		getFormFieldsPanel().add(lblChoiceOf, getStr("2, %d, 3, 1"));
		
		separator = new JSeparator();
		separator.setForeground(UIManager.getColor("InternalFrame.inactiveTitleGradient"));
		getFormFieldsPanel().add(separator, getStr("9, %d"));

		currentY += 2;
		OSOAAViewPhi_title = DefaultComponentFactory.getInstance().createLabel(
				"OSOAA.View.Phi *:");
		getFormFieldsPanel().add(OSOAAViewPhi_title, getStr("2, %d, right, default"));
		
		OSOAAViewPhi_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.0"), null, null, new BigDecimal("1.0")));
        PropertiesManager.getInstance().register(OSOAAViewPhi_title, OSOAAViewPhi_spinner);
		OSOAAViewPhi_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveOSOAAViewPhi();
				validateForm();
			}
		});
		getFormFieldsPanel().add(OSOAAViewPhi_spinner, getStr("4, %d"));

		OSOAAViewPhi_desc = DefaultComponentFactory.getInstance()
				.createLabel("Relative azimuth angle  (degrees)");
		OSOAAViewPhi_desc
				.setToolTipText("Relative azimuth angle  (degrees)");
		getFormFieldsPanel().add(OSOAAViewPhi_desc, getStr("9, %d"));

        // here there is the OSOAA.View.Level combobox

		currentY += 4;
		OSOAAViewZ_title = new JLabel("OSOAA.View.Z *:");
		OSOAAViewZ_title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(OSOAAViewZ_title, getStr("2, %d, right, default"));
		
		OSOAAViewZ_spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.0"), null, null, new BigDecimal("1.0")));
        PropertiesManager.getInstance().register(OSOAAViewZ_title, OSOAAViewZ_spinner);
		OSOAAViewZ_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveOSOAAViewZ();
				validateForm();
			}
		});
		getFormFieldsPanel().add(OSOAAViewZ_spinner, getStr("4, %d"));

		OSOAAViewZ_desc = DefaultComponentFactory.getInstance()
				.createLabel("Altitude or depth (meters) for which the radiance will be provided as a function of the viewing zenith angle (for the given relative azimuth angle)");
		OSOAAViewZ_desc
				.setToolTipText("Altitude or depth (meters) for which the radiance will be provided as a function of the viewing zenith angle (for the given relative azimuth angle)");
		getFormFieldsPanel().add(OSOAAViewZ_desc, getStr("9, %d"));

		currentY += 2;
		OSOAAViewVZA_title = new JLabel("OSOAA.View.VZA :");
		OSOAAViewVZA_title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(OSOAAViewVZA_title, getStr("2, %d, right, default"));
		
		OSOAAViewVZA_spinner = new JSpinnerOptionalRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.0"),
                null, null, new BigDecimal("0.1")));
        PropertiesManager.getInstance().register(OSOAAViewVZA_title, OSOAAViewVZA_spinner);
		OSOAAViewVZA_spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveOSOAAViewVZA();
				validateForm();
			}
		});
		getFormFieldsPanel().add(OSOAAViewVZA_spinner, getStr("4, %d"));

		OSOAAViewVZA_desc = new JLabel("Viewing zenith angle (from 0 to 180 degrees) for which the radiance will be provided as function of the depth (for the given relative azimuth angle)");
		OSOAAViewVZA_desc
				.setToolTipText("Viewing zenith angle (from 0 to 180 degrees) for which the radiance will be provided as function of the depth (for the given relative azimuth angle)");
		getFormFieldsPanel().add(OSOAAViewVZA_desc, getStr("9, %d"));
		
		currentY += 2;
		lblOutputFiles = new JLabel("    |--> Output files");
		lblOutputFiles.setHorizontalAlignment(SwingConstants.LEFT);
		lblOutputFiles.setForeground(Color.BLUE);
		lblOutputFiles.setFont(new Font("Tahoma", Font.BOLD, 11));
		getFormFieldsPanel().add(lblOutputFiles, getStr("2, %d, 3, 1"));
		
		separator_1 = new JSeparator();
		separator_1.setForeground(UIManager.getColor("InternalFrame.inactiveTitleGradient"));
		getFormFieldsPanel().add(separator_1, getStr("9, %d"));
		
		currentY += 2;
		SOSLog_title = new JLabel("SOS.Log :");
		SOSLog_title.setToolTipText("");
		getFormFieldsPanel().add(SOSLog_title, getStr("2, %d, right, default"));
		
		SOSLog_textField = new JTextField();
		SOSLog_textField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				saveSOSLog();
				validateForm();
			}
		});
		SOSLog_textField.setColumns(10);
		getFormFieldsPanel().add(SOSLog_textField, getStr("4, %d, fill, default"));
		
		SOSLog_desc = new JLabel("Name of log file for model core radiative transfer calculations");
		SOSLog_desc.setToolTipText("Name of log file for model core radiative transfer calcultations");
		getFormFieldsPanel().add(SOSLog_desc, getStr("9, %d"));
		
		currentY += 2;
		SOSResFileBin_title = new JLabel("SOS.ResFile.Bin *:");
		SOSResFileBin_title.setToolTipText("");
		getFormFieldsPanel().add(SOSResFileBin_title, getStr("2, %d, right, default"));
		
		SOSResFileBin_textField = new JTextField();
		SOSResFileBin_textField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				saveSOSResFileBin();
				validateForm();
			}
		});
		SOSResFileBin_textField.setColumns(10);
		getFormFieldsPanel().add(SOSResFileBin_textField, getStr("4, %d, fill, default"));
		
		SOSResFileBin_desc = new JLabel("Filename of the SOS binary output including Fourier series expansions");
		SOSResFileBin_desc.setToolTipText("Filename of SOS binary output including Fourier series expansions");
		getFormFieldsPanel().add(SOSResFileBin_desc, getStr("9, %d"));
        
                
		currentY += 2;
		OSOAAResFilevsVZA_textField = new JTextField();
		OSOAAResFilevsVZA_textField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				saveOSOAAResFilevsVZA();
				validateForm();
			}
		});
		
		OSOAAResFilevsVZA_title = DefaultComponentFactory.getInstance().createLabel(
			"OSOAA.ResFile.vsVZA *:");
		getFormFieldsPanel().add(OSOAAResFilevsVZA_title, getStr("2, %d, right, default"));
		OSOAAResFilevsVZA_textField.setColumns(10);
		getFormFieldsPanel().add(OSOAAResFilevsVZA_textField, getStr("4, %d, fill, default"));
		
		OSOAAResFilevsVZA_desc = new JLabel("Output ascii file providing the radiance versus the viewing zenith angle (for the given level defined by OSOAA.View.Z)");
		OSOAAResFilevsVZA_desc.setToolTipText("Output ascii file providing the radiance versus the viewing zenith angle (for the given level defined by OSOAA.View.Z)");
		getFormFieldsPanel().add(OSOAAResFilevsVZA_desc, getStr("9, %d"));
		
		currentY += 2;
		OSOAAResFilevsZ_title = new JLabel("OSOAA.ResFile.vsZ :");
		OSOAAResFilevsZ_title.setToolTipText("");
		getFormFieldsPanel().add(OSOAAResFilevsZ_title, getStr("2, %d, right, default"));
		
		OSOAAResFilevsZ_textField = new JTextField();
		OSOAAResFilevsZ_textField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				saveOSOAAResFilevsZ();
				validateForm();
			}
		});
		OSOAAResFilevsZ_textField.setColumns(10);
		getFormFieldsPanel().add(OSOAAResFilevsZ_textField, getStr("4, %d, fill, default"));
		
		OSOAAResFilevsZ_desc = new JLabel("Output ascii file providing the radiance versus the depth (for the given viewing zenith angle defined by OSOAA.View.VZA)");
		OSOAAResFilevsZ_desc.setToolTipText("Output ascii file providing the radiance versus the depth (for the given viewing zenith angle defined by OSOAA.View.VZA)");
		getFormFieldsPanel().add(OSOAAResFilevsZ_desc, getStr("9, %d"));

		currentY += 2;
        // OSOAA.Log begin
        OSOAALog_title = new JLabel("OSOAA.Log :");
        OSOAALog_title.setToolTipText("");
        getFormFieldsPanel().add(OSOAALog_title, getStr("2, %d, right, default"));

        OSOAALog_textField = new JTextField();
        OSOAALog_textField.addCaretListener(new CaretListener() {
            public void caretUpdate(CaretEvent e) {
                saveOSOAALog();
                validateForm();
            }
        });
        OSOAALog_textField.setColumns(10);
        getFormFieldsPanel().add(OSOAALog_textField, getStr("4, %d, fill, default"));

        OSOAALog_desc = new JLabel("Name of the main log file");
        OSOAALog_desc.setToolTipText("Name of the main log file");
        getFormFieldsPanel().add(OSOAALog_desc, getStr("9, %d"));

        // OSOAA.Log end

		currentY += 2;
		lblOutputFiles = new JLabel("    |--> Advanced outputs");
		lblOutputFiles.setHorizontalAlignment(SwingConstants.LEFT);
		lblOutputFiles.setForeground(Color.BLUE);
		lblOutputFiles.setFont(new Font("Tahoma", Font.BOLD, 11));
		getFormFieldsPanel().add(lblOutputFiles, getStr("2, %d, 3, 1"));

		separator_1 = new JSeparator();
		separator_1.setForeground(UIManager.getColor("InternalFrame.inactiveTitleGradient"));
		getFormFieldsPanel().add(separator_1, getStr("9, %d"));
		
		currentY += 2;
		lblOutputFiles = new JLabel("files providing the radiance as a function of the altitude or depth, and viewing zenith angle (for the given relative azimuth angle)");
		lblOutputFiles.setHorizontalAlignment(SwingConstants.LEFT);
		lblOutputFiles.setForeground(Color.BLUE);
		lblOutputFiles.setFont(new Font("Tahoma", Font.BOLD, 11));
		getFormFieldsPanel().add(lblOutputFiles, getStr("9, %d"));

		currentY += 2;
		OSOAAResFileAdv_title = new JLabel("OSOAA.ResFile.Adv.Up :");
		OSOAAResFileAdv_title.setToolTipText("");
		getFormFieldsPanel().add(OSOAAResFileAdv_title, getStr("2, %d, right, default"));
		
		OSOAAResFileAdv_textField = new JTextField();
		OSOAAResFileAdv_textField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				saveOSOAAResFileAdv();
				validateForm();
			}
		});
		OSOAAResFileAdv_textField.setColumns(10);
		getFormFieldsPanel().add(OSOAAResFileAdv_textField, getStr("4, %d, fill, default"));
		
		OSOAAResFileAdv_desc = new JLabel("Filename of the output as an ascii file that provides the UPWARD radiance field");
		OSOAAResFileAdv_desc.setToolTipText("Filename of the output as an ascii file that provides the UPWARD radiance field");
		getFormFieldsPanel().add(OSOAAResFileAdv_desc, getStr("9, %d"));

		currentY += 2;
		OSOAAResFileAdvDown_title = new JLabel("OSOAA.ResFile.Adv.Down :");
		OSOAAResFileAdvDown_title.setToolTipText("");
		getFormFieldsPanel().add(OSOAAResFileAdvDown_title, getStr("2, %d, right, default"));
		
		OSOAAResFileAdvDown_textField = new JTextField();
		OSOAAResFileAdvDown_textField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				saveOSOAAResFileAdvDown();
				validateForm();
			}
		});
		OSOAAResFileAdvDown_textField.setColumns(10);
		getFormFieldsPanel().add(OSOAAResFileAdvDown_textField, getStr("4, %d, fill, default"));
		
		OSOAAResFileAdvDown_desc = new JLabel("Filename of the output as an ascii file that provides the DOWNWARD radiance field");
		OSOAAResFileAdvDown_desc.setToolTipText("Filename of the output as an ascii file that provides the DOWNWARD radiance field");
		getFormFieldsPanel().add(OSOAAResFileAdvDown_desc, getStr("9, %d"));

		// OSOAA View.Level combobox begin

		currentY = 7;
        ViewLevel_title = new JLabel("OSOAA.View.Level *:");
        ViewLevel_title.setHorizontalAlignment(SwingConstants.RIGHT);
        getFormFieldsPanel().add(ViewLevel_title, getStr("2, %d, right, default"));

        ViewLevel_combobox = new JComboBox();
        ViewLevel_combobox.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e_) {
                JComboBox cb = (JComboBox) e_.getSource();
                int selIndex = cb.getSelectedIndex();

                onViewLevelItemChanged(selIndex);

                saveViewModel();
                validateForm();
            }
        });

        Map<ViewLevelEnum, String> aMap = new TreeMap<ViewLevelEnum, String>();
        aMap.put(ViewLevelEnum.TOP_OF_ATMOSPHERE, "Top of Atmosphere");
        aMap.put(ViewLevelEnum.SEA_BOTTOM, "Sea bottom");
        aMap.put(ViewLevelEnum.SEA_SURFACE_O_PLUS, "Above Sea surface O+");
        aMap.put(ViewLevelEnum.SEA_SURFACE_O_MINUS, "Under Sea surface O-");
        aMap.put(ViewLevelEnum.USERDEF, "User's definition of altitude or depth");

        ViewLevel_combobox.setModel(new DefaultComboBoxModel(
                aMap.values().toArray()));

        getFormFieldsPanel().add(ViewLevel_combobox, getStr("4, %d, fill, default"));

        JLabel ViewLevel_title = new JLabel("Output level definition");
        getFormFieldsPanel().add(ViewLevel_title, getStr("9, %d"));
        // OSOAA View.Level combobox end

	}

    private void saveViewModel()
    {
        int index = Math.max(0, outputSpecificities.getOSOAAViewLevel() - 1);
        outputSpecificities.setOSOAAViewLevel(index + 1);
    }

    private void onViewLevelItemChanged(int selIndex) {
        outputSpecificities.setOSOAAViewLevel(selIndex + 1);

        if(selIndex == 4)
        {
            OSOAAViewZ_spinner.setEnabled(true);
            outputSpecificities.enablePreferences("OSOAA.View.Z");
        }
        else
        {
            OSOAAViewZ_spinner.setEnabled(false);
            outputSpecificities.disablePreferences("OSOAA.View.Z");
        }
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
				PreferencesFactory.getInstance().getOutputSpecificities().reset();
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
	
	public BigDecimal getOSOAAViewPhi() {
		OSOAAViewPhi_spinner.validate();
		return (BigDecimal) OSOAAViewPhi_spinner.getValue();
	}

	private void saveOSOAAViewPhi(){
		if(getOSOAAViewPhi()!=null){
			outputSpecificities.setOSOAAViewPhi( getOSOAAViewPhi() );
		}
	}

	public BigDecimal getOSOAAViewZ() {
		OSOAAViewZ_spinner.validate();
		return (BigDecimal) OSOAAViewZ_spinner.getValue();
	}

	private void saveOSOAAViewZ(){
		if(getOSOAAViewZ()!=null){
			outputSpecificities.setOSOAAViewZ( getOSOAAViewZ() );
		}
	}

	public BigDecimal getOSOAAViewVZA() {
		OSOAAViewVZA_spinner.validate();
		return (BigDecimal) OSOAAViewVZA_spinner.getValue();
	}

	private void saveOSOAAViewVZA()
    {
        if(OSOAAViewVZA_spinner.getValue() == null)
        {
            outputSpecificities.setOSOAAViewVZA( null );
        }
        else
        {
            if(getOSOAAViewVZA()!=null){
                outputSpecificities.setOSOAAViewVZA( getOSOAAViewVZA() );
            }
        }
	}

	public Integer getSOSIGmax() {
		SOSIGmax_spinner.validate();
		return (Integer) SOSIGmax_spinner.getValue();
	}

	private void saveSOSIGmax(){
		if(getSOSIGmax()!=null)
        {
			outputSpecificities.setSOSIGmax( getSOSIGmax() );
		}
	}

	public String getSOSLog() {
		SOSLog_textField.validate();
		return SOSLog_textField.getText();
	}

	private void saveSOSLog(){
		if(getSOSLog()!=null){
			outputSpecificities.setSOSLog( getSOSLog() );
		}
	}

	public String getSOSResFileBin() {
		SOSResFileBin_textField.validate();
		return SOSResFileBin_textField.getText();
	}

	private void saveSOSResFileBin(){
		if(getSOSResFileBin()!=null){
			outputSpecificities.setSOSResFileBin( getSOSResFileBin() );
		}
	}

	public String getOSOAAResFilevsVZA() {
		OSOAAResFilevsVZA_textField.validate();
		return OSOAAResFilevsVZA_textField.getText();
	}

	private void saveOSOAAResFilevsVZA(){
		if(getOSOAAResFilevsVZA()!=null){
			outputSpecificities.setOSOAAResFilevsVZA( getOSOAAResFilevsVZA() );
		}
	}

	public String getOSOAAResFilevsZ() {
		OSOAAResFilevsZ_textField.validate();
		return OSOAAResFilevsZ_textField.getText();
	}

	private void saveOSOAAResFilevsZ(){
		if(getOSOAAResFilevsZ()!=null){
			outputSpecificities.setOSOAAResFilevsZ( getOSOAAResFilevsZ() );
		}
	}

	public String getOSOAAResFileAdv() {
		OSOAAResFileAdv_textField.validate();
		return OSOAAResFileAdv_textField.getText();
	}
	
	public String getOSOAAResFileAdvDown() {
		OSOAAResFileAdvDown_textField.validate();
		return OSOAAResFileAdvDown_textField.getText();
	}

	private void saveOSOAAResFileAdv(){
		if(getOSOAAResFileAdv()!=null){
			outputSpecificities.setOSOAAResFileAdv( getOSOAAResFileAdv() );
		}
	}
	
	private void saveOSOAAResFileAdvDown(){
		if(getOSOAAResFileAdvDown()!=null){
			outputSpecificities.setOSOAAResFileAdvDown( getOSOAAResFileAdvDown() );
		}
	}

    public String getOSOAALog() {
        OSOAALog_textField.validate();
        return OSOAALog_textField.getText();
    }

    private void saveOSOAALog()
    {
        if(getOSOAALog() != null)
        {
            outputSpecificities.setOSOAALog(getOSOAALog());
        }
    }
	
	protected void setOSOAAViewPhiVisible(boolean isVisible_) {
		OSOAAViewPhi_title.setVisible(isVisible_);
		OSOAAViewPhi_spinner.setVisible(isVisible_);
		OSOAAViewPhi_desc.setVisible(isVisible_);
	}

	protected void setOSOAAViewZVisible(boolean isVisible_) {
		OSOAAViewZ_title.setVisible(isVisible_);
		OSOAAViewZ_spinner.setVisible(isVisible_);
		OSOAAViewZ_desc.setVisible(isVisible_);
	}

	protected void setOSOAAViewVZAVisible(boolean isVisible_) {
		OSOAAViewVZA_title.setVisible(isVisible_);
		OSOAAViewVZA_spinner.setVisible(isVisible_);
		OSOAAViewVZA_desc.setVisible(isVisible_);
	}

	protected void setSOSIGmaxVisible(boolean isVisible_) {
		SOSIGmax_title.setVisible(isVisible_);
		SOSIGmax_spinner.setVisible(isVisible_);
		SOSIGmax_desc.setVisible(isVisible_);
	}

	protected void setSOSLogVisible(boolean isVisible_) {
		SOSLog_title.setVisible(isVisible_);
		SOSLog_textField.setVisible(isVisible_);
		SOSLog_desc.setVisible(isVisible_);
	}

	protected void setSOSResFileBinVisible(boolean isVisible_) {
		SOSResFileBin_title.setVisible(isVisible_);
		SOSResFileBin_textField.setVisible(isVisible_);
		SOSResFileBin_desc.setVisible(isVisible_);
	}

	protected void setOSOAAResFilevsVZAVisible(boolean isVisible_) {
		OSOAAResFilevsVZA_title.setVisible(isVisible_);
		OSOAAResFilevsVZA_textField.setVisible(isVisible_);
		OSOAAResFilevsVZA_desc.setVisible(isVisible_);
	}

	protected void setOSOAAResFilevsZVisible(boolean isVisible_) {
		OSOAAResFilevsZ_title.setVisible(isVisible_);
		OSOAAResFilevsZ_textField.setVisible(isVisible_);
		OSOAAResFilevsZ_desc.setVisible(isVisible_);
	}

	protected void setOSOAAResFileAdvVisible(boolean isVisible_) {
		OSOAAResFileAdv_title.setVisible(isVisible_);
		OSOAAResFileAdv_textField.setVisible(isVisible_);
		OSOAAResFileAdv_desc.setVisible(isVisible_);
	}
	
	protected void setOSOAAResFileAdvDownVisible(boolean isVisible_) {
		OSOAAResFileAdvDown_title.setVisible(isVisible_);
		OSOAAResFileAdvDown_textField.setVisible(isVisible_);
		OSOAAResFileAdvDown_desc.setVisible(isVisible_);
	}
}
