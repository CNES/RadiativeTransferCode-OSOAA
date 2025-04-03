package osoaa.usl;

import net.miginfocom.swing.MigLayout;
import org.apache.log4j.Logger;
import osoaa.bll.command.CommandManager;
import osoaa.bll.command.ICommandManager;
import osoaa.bll.preferences.ICommonPreferences;
import osoaa.bll.preferences.PreferencesFactory;
import osoaa.bll.properties.PropertiesManager;
import osoaa.usl.command.CommandPanel;
import osoaa.usl.command.RunCommandPanel;
import osoaa.usl.common.ui.forms.FormUtils;
import osoaa.usl.common.ui.forms.IFormValidation;
import osoaa.usl.common.ui.forms.IFormValidationListener;
import osoaa.usl.common.ui.jlabel.ImageJLabel;
import osoaa.usl.common.ui.jspinner.JSpinnerRangedValue;
import osoaa.usl.common.ui.jspinner.SpinnerBigDecimalModel;
import osoaa.usl.forms.*;
import osoaa.usl.resources.icons.IconResources;
import osoaa.usl.resources.images.ImageResources;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;


public class MainJFrame extends JFrame {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private JSpinner radianceWaveLengthSpinner;
	private JSpinner solarZenithAngleSpinner;
	private JScrollPane formScrollPane;
	private ICommonPreferences commonPref;
	private ICommandManager commandManager;
    private boolean expertMode;
	
	private RunCommandPanel runCommandPanel;
	
	private Logger log = Logger.getLogger("osoaa.usl.MainJFrame");
	
	
	
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		
		EventQueue.invokeLater(new Runnable() {
			Logger log = Logger.getLogger("osoaa.usl");
			public void run() {
				try {
					MainJFrame frame = new MainJFrame();
					frame.setVisible(true);
				} catch (Exception e) {
					log.error(e.getMessage(), e);
				}
			}
		});
	}
	
	private void initLookAndFeel(){
		boolean lookAndFeelIsSettedOK = false;
		try {
			UIManager.setLookAndFeel("com.sun.java.swing.plaf.windows.WindowsLookAndFeel");
			lookAndFeelIsSettedOK = true;
		} catch (Throwable e) {
			// nothing to do
		}
		
		if(!lookAndFeelIsSettedOK)
		{
			try {
				UIManager.setLookAndFeel("com.sun.java.swing.plaf.motif.MotifLookAndFeel");
				lookAndFeelIsSettedOK = true;
			} catch (Throwable e) {
				// nothing to do
			}	
		}
		
		if(!lookAndFeelIsSettedOK)
		{
			log.warn("Problems setting LookAndFeel...");
		}
	}

	private void initAtmosphericAndSeaProfilesJPanel(){
		if(atmosphericAndSeaProfilesJPanel==null){
			atmosphericAndSeaProfilesJPanel = new AtmosphericAndSeaProfilesJPanel();
			atmosphericAndSeaProfilesJPanel.addFormValidationListener(new IFormValidationListener() {
				
				@Override
				public void onFormValidated(boolean isFormValid_) {
					if(itemsList!=null){
						itemsList.repaint();
					}
				}
			});
			atmosphericAndSeaProfilesJPanel.init();
			mapFormValidation.put(1, atmosphericAndSeaProfilesJPanel);
		}
	}
	
	private void initAerosolsModelJPanel(){
		if(aerosolsModelJPanel==null){
			aerosolsModelJPanel = new AerosolsModelJPanel();
			aerosolsModelJPanel.addFormValidationListener(new IFormValidationListener() {
				
				@Override
				public void onFormValidated(boolean isFormValid_) {
					if(itemsList!=null){
						itemsList.repaint();
					}
				}
			});
			aerosolsModelJPanel.init();
			mapFormValidation.put(2, aerosolsModelJPanel);
		}
	}
	
	private void initHydrogroundModelJPanel(){
		if(hydrogroundModelJPanel==null){
			hydrogroundModelJPanel = new HydroGroundModelJPanel();
			hydrogroundModelJPanel.addFormValidationListener(new IFormValidationListener() {
				
				@Override
				public void onFormValidated(boolean isFormValid_) {
					if(itemsList!=null){
						itemsList.repaint();
					}
				}
			});
			hydrogroundModelJPanel.init();
			mapFormValidation.put(3, hydrogroundModelJPanel);
		}
	}
	
	private void initSeaAndAtmosphereModelJPanel(){
		if(seaAndAtmosphereModelJPanel==null){
			seaAndAtmosphereModelJPanel = new SeaAndAtmosphereModelJPanel();
			seaAndAtmosphereModelJPanel.addFormValidationListener(new IFormValidationListener() {
				
				@Override
				public void onFormValidated(boolean isFormValid_) {
					if(itemsList!=null){
						itemsList.repaint();
					}
				}
			});
			seaAndAtmosphereModelJPanel.init();
			mapFormValidation.put(4, seaAndAtmosphereModelJPanel);
		}
	}
	
	private void initGeometricConditionsModelJPanel(){
		if(geometricConditionsModelJPanel==null){
			geometricConditionsModelJPanel = new GeometricConditionsModelJPanel();
			geometricConditionsModelJPanel.addFormValidationListener(new IFormValidationListener() {
				
				@Override
				public void onFormValidated(boolean isFormValid_) {
					if(itemsList!=null){
						itemsList.repaint();
					}
				}
			});
			geometricConditionsModelJPanel.init();
			mapFormValidation.put(5, geometricConditionsModelJPanel);
		}
	}
	
	private void initOutputSpecificitiesJPanel(){
		if(outputSpecificitiesJPanel==null){
			outputSpecificitiesJPanel = new OutputSpecificitiesJPanel();
			outputSpecificitiesJPanel.addFormValidationListener(new IFormValidationListener() {
				
				@Override
				public void onFormValidated(boolean isFormValid_) {
					if(itemsList!=null){
						itemsList.repaint();
					}
				}
			});
			outputSpecificitiesJPanel.init();
			mapFormValidation.put(6, outputSpecificitiesJPanel);
		}
	}

	private void updateExpert(JCheckBox jche) {
		expertMode = jche.isSelected();

                {
                    Map<String, Object> buttonz = PropertiesManager.getButtons();
                    Collection col = buttonz.values();
                    for (Object jsp: col)
                    {
                        JSpinner ajsp = (JSpinner) jsp;
                        ajsp.setEnabled(expertMode);
                    }
                }

                commonPref.setExpert((Boolean) expertMode);
	}
	/**
	 * Create the frame.
	 */
	public MainJFrame() {
		initLookAndFeel();
		
		commandManager = new CommandManager();
		commonPref = PreferencesFactory.getInstance().getCommonPreferences();
		mapFormValidation = new HashMap<Integer, IFormValidation>();
		initAtmosphericAndSeaProfilesJPanel();
		initAerosolsModelJPanel();
		initHydrogroundModelJPanel();
		initSeaAndAtmosphereModelJPanel();
		initGeometricConditionsModelJPanel();
		initOutputSpecificitiesJPanel();
		
		runCommandPanel = new RunCommandPanel();
			
		setBackground(Color.WHITE);

        setTitle("OSOAA :: Ocean Successive Orders with Atmosphere Advanced");
		setIconImage(IconResources.getFavIcon());
		setBounds(100, 100, 800, 600);
		setExtendedState(JFrame.MAXIMIZED_BOTH);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		JPanel contentPane = new JPanel();
		contentPane.setBorder(new EmptyBorder(5, 5, 5, 5));
		setContentPane(contentPane);
		contentPane.setLayout(new BorderLayout(0, 0));
		
		JPanel panel = new JPanel();
		panel.setBackground(Color.WHITE);
		contentPane.add(panel, BorderLayout.NORTH);
		panel.setLayout(new MigLayout("", "[][grow,left][][][][][53px]", "[23px]"));
		
		workingPahtJtextField = new JTextField();
		workingPahtJtextField.setColumns(200);
		workingPahtJtextField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				validateWorkingPathField();
			}
		});
		workingPahtJtextField.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent arg0) {
				onWorkingPathClicked();
			}
		});
				
				
		panel.add(workingPahtJtextField, "cell 1 0");
		
		JButton showCommandBtn = new JButton("Show command");
		showCommandBtn.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				itemsList.clearSelection();
				
				if( commandPanel == null){
					commandPanel = new CommandPanel();
				}
				commandPanel.setCommandLine(commandManager.getCommand());
				updateMainContentPanel(commandPanel);
			}
		});
		showCommandBtn.setIcon( IconResources.getIconFromImage(IconResources.getShowCommandIcon()));
		showCommandBtn.setToolTipText("Show command");
		panel.add(showCommandBtn, "cell 3 0");
		
		final JButton btnRun = new JButton("RUN");
		btnRun.addActionListener(new ActionListener() {
			
			public void actionPerformed(ActionEvent e) {
				boolean areFormsValid = true;
				for( IFormValidation fv : mapFormValidation.values()){
					if(fv!=null && ! fv.isFormValid() ){
						areFormsValid = false;
					}
				}

                String OS = System.getProperty("os.name");
                boolean isWindows = OS.startsWith("Windows");

				String the_command = commandManager.getCommand();
				log.info(the_command);

                try
                {
                    // create working dir if doesn't exist
                    if(! new File(commonPref.getWorkingAbsolutePathDir()).exists())
                    {
                        new File(commonPref.getWorkingAbsolutePathDir()).mkdirs();
                    }
                } catch (Exception exc) {
                    log.error(exc.getMessage(), exc);
                }

				try {
					String base = PreferencesFactory.getInstance().getAdminConfiguration().getPreferences().CONF_DIR_PATH.getCanonicalPath();

					File f = new File(base, "lastRun.sh");
					PrintWriter pw = new PrintWriter(f);

                    String shell = null;

                    // csh check
                    shell = System.getenv("shell");
                    if(shell != null)
                    {
                        if(!shell.isEmpty())
                        {
                            pw.write("#!/usr/bin/env csh");
                        }
                    }

                    // ksh check
                    if(shell == null)
                    {
                        shell = System.getenv("KSH_VERSION");
                        if(shell != null)
                        {
                            if(!shell.isEmpty())
                            {
                                pw.write("#!/usr/bin/env ksh");
                            }
                        }
                    }

                    // bash check
                    if(shell == null)
                    {
                        shell = System.getenv("SHELL");
                        if(shell != null)
                        {
                            if(!shell.isEmpty())
                            {
                                pw.write("#!/usr/bin/env bash");
                            }
                        }
                    }

                    // default bash
                    if(shell == null)
                    {
                        pw.write("#!/usr/bin/env bash");
                    }

					pw.write("\n");
                    pw.write("cd " + commonPref.getWorkingAbsolutePathDir() + "\n");
                    pw.write("echo \"Starting OSOAA...\"\n");
					pw.write(the_command);
                    pw.write("2>&1");
					pw.close();

                    if(!isWindows)
                    {
                        Runtime.getRuntime().exec("chmod u+x " + f.getCanonicalPath());
                        Thread.sleep(1000);
                    }
				} catch (Exception exc) {
					log.error(exc.getMessage(), exc);
				}

                if(!areFormsValid){
					JOptionPane.showMessageDialog(
						    MainJFrame.this,
						    "Ooops... Some forms contain errors. You must validate all forms before running the program.",
						    "Errors on forms",
						    JOptionPane.INFORMATION_MESSAGE);
					
				} else {
					itemsList.setSelectedIndex( itemsList.getModel().getSize()-1 );
					runCommandPanel.setCommandLine(commandManager.getCommand());
					if( ! commandManager.isProcessRunning() ){
						final Process p;
						try {
							p = commandManager.startProcess();
							new Thread("RunBtn : Waiting end of process"){

								@Override
								public void run() {
									try{
										p.waitFor();
									} catch(InterruptedException e){
										log.error(e.getMessage(), e);
									}
									btnRun.setIcon( IconResources.getIconFromImage(IconResources.getRunIcon()) );
								}
								
							}.start();
							
							runCommandPanel.setRunningProcess(p);
							btnRun.setIcon( IconResources.getIconFromImage(IconResources.getLoadingAnimateIcon()) );
						} catch (IOException e1) {
							runCommandPanel.setErrorOnRunningProcess(e1);
							log.error(e1.getMessage(), e1);
						}
					}
					updateMainContentPanel(runCommandPanel);
				}
			}
		});
		btnRun.setIcon( IconResources.getIconFromImage(IconResources.getRunIcon()) );
		btnRun.setToolTipText("Run the program");
		panel.add(btnRun, "cell 6 0,alignx left,aligny top");
		
		JLabel lblNewLabel = new JLabel("Radiance wavelength :");
		lblNewLabel.setForeground(Color.BLUE);
		panel.add(lblNewLabel, "flowx,cell 0 0,alignx trailing");
		
		radianceWaveLengthSpinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.000"), null, null, new BigDecimal("0.001")));
		radianceWaveLengthSpinner.setMinimumSize(new Dimension(60, 20));
		radianceWaveLengthSpinner.setPreferredSize(new Dimension(60, 20));
		radianceWaveLengthSpinner.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                radianceWaveLengthSpinner.validate();
                saveRadianceWaveLength((BigDecimal) radianceWaveLengthSpinner.getValue());

                aerosolsModelJPanel.checkChanges();
            }
        });
		panel.add(radianceWaveLengthSpinner, "cell 0 0,alignx left");
		
		JLabel radianceWaveLengthUnit = new JLabel("\u00B5m");
		radianceWaveLengthUnit.setForeground(Color.BLUE);
		panel.add(radianceWaveLengthUnit, "cell 0 0");
		
		JLabel separatorLabel = new JLabel(" | ");
		panel.add(separatorLabel, "cell 0 0");
		
		JLabel solarZenithAngleTitle = new JLabel("Solar zenith angle :");
		solarZenithAngleTitle.setForeground(Color.BLUE);
		panel.add(solarZenithAngleTitle, "cell 0 0");
		
		solarZenithAngleSpinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("30.0"), new BigDecimal("0.1"), null, new BigDecimal("0.1")));
		solarZenithAngleSpinner.setMinimumSize(new Dimension(50, 20));
		solarZenithAngleSpinner.setPreferredSize(new Dimension(50, 20));
		solarZenithAngleSpinner.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent arg0) {
                solarZenithAngleSpinner.validate();
                saveSolarZenithAngle((BigDecimal) solarZenithAngleSpinner.getValue());
            }
        });
		panel.add(solarZenithAngleSpinner, "cell 0 0,alignx left,aligny center");
		
		JLabel solarZenithAngleHint = new JLabel("deg");
		solarZenithAngleHint.setForeground(Color.BLUE);
		panel.add(solarZenithAngleHint, "cell 0 0");

        JLabel asep = new JLabel(" | ");
        panel.add(asep, "cell 0 0");

        JLabel sep = new JLabel("Expert mode :");
        sep.setForeground(Color.BLUE);
        panel.add(sep, "cell 0 0");
        sep.setToolTipText("Expert fields are defined in file OSOAA.h, unmodified values are shown in green, modified fields are shown in orange.");

        final JCheckBox jche = new JCheckBox();
        panel.add(jche, "cell 0 0");
        jche.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                updateExpert(jche);
            }
        });
		updateExpert(jche);
        jche.setSelected(commonPref.isExpert());
		
		JLabel label = new JLabel(" | ");
		panel.add(label, "cell 0 0");
		
		lblWorkingPath = new JLabel("Working directory :");
		lblWorkingPath.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent arg0) {
				onWorkingPathClicked();
			}
		});
		lblWorkingPath.setForeground(Color.BLUE);
		panel.add(lblWorkingPath, "cell 0 0");
		JSplitPane splitPane = new JSplitPane();
		contentPane.add(splitPane, BorderLayout.CENTER);
		
		JScrollPane itemListScrollPane = new JScrollPane();
		itemListScrollPane.setMinimumSize(new Dimension(190, 19));
		splitPane.setLeftComponent(itemListScrollPane);
		
		itemsList = new JList();
		itemsList.addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent arg0) {
				onItemsSelected( itemsList.getSelectedIndex() );
			}
		});
		
		itemsList.setCellRenderer(new ListCellRenderer() {
			
			@Override
			public Component getListCellRendererComponent(JList list, Object value,
					int index, boolean isSelected, boolean cellHasFocus) {
				JLabel jlRenderer = new JLabel( value.toString() );
				jlRenderer.setPreferredSize(new Dimension(200, 19));
				if(index==0){
					//Home
					jlRenderer.setIcon(IconResources.getIconFromImage(IconResources.getHomeIcon()));
					jlRenderer.setBorder(new MatteBorder(0, 0, 1, 0, UIManager
							.getColor("InternalFrame.inactiveTitleGradient")));
				} else if(index == itemsList.getModel().getSize()-1){
					jlRenderer.setIcon(IconResources.getIconFromImage(IconResources.getProcessIcon()));
					jlRenderer.setBorder(new MatteBorder(1, 0, 0, 0, UIManager
							.getColor("InternalFrame.inactiveTitleGradient")));
				} else {
					IFormValidation formValidation = mapFormValidation.get( index );
					Icon iconForm = IconResources.getIconFromImage(IconResources.getFormNormalIcon());
					if(formValidation!=null){
						if( formValidation.isFormValid() ){
							iconForm = IconResources.getIconFromImage(IconResources.getItemCheckOKIcon());
						} else {
							iconForm = IconResources.getIconFromImage(IconResources.getFormWarningIcon());
						}
					}
					jlRenderer.setIcon(iconForm);
				}
				
				if( isSelected ){
					jlRenderer.setFont(new Font("Tahoma", Font.BOLD, 12));
					jlRenderer.setForeground(Color.BLACK);
				} else {
					jlRenderer.setFont(new Font("Tahoma", Font.PLAIN, 12));
					jlRenderer.setForeground(Color.BLUE);
				}
				
				return jlRenderer;
			}
		});
		itemsList.setModel(
				new AbstractListModel() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;
			String[] values = new String[] {"Home", "Atmospheric & sea profiles", "Aerosols model", "Hydrosols model", "Sea/atmosphere interface", "Geometric parameters", "Output specificities", "Process tracker"};
			public int getSize() {
				return values.length;
			}
			public Object getElementAt(int index) {
				return values[index];
			}
		});
		itemsList.setVisibleRowCount(6);
		itemListScrollPane.setViewportView(itemsList);
		
		formScrollPane = new JScrollPane();
		formScrollPane.setBackground(Color.WHITE);
		splitPane.setRightComponent(formScrollPane);
		
		welcomePanel = new JPanel();
		welcomePanel.setBackground(Color.WHITE);
		formScrollPane.setViewportView(welcomePanel);
		welcomePanel.setLayout(new BorderLayout(0, 0));
		
		JPanel jpCenter = new JPanel(new BorderLayout());
		jpCenter.setBackground(Color.WHITE);
		
		JPanel panel_1 = new JPanel();
		panel_1.setBorder(new EmptyBorder(70, 0, 0, 0));
		panel_1.setBackground(Color.WHITE);
		jpCenter.add(panel_1, BorderLayout.CENTER);
		panel_1.setLayout(new BoxLayout(panel_1, BoxLayout.Y_AXIS));
		
		JLabel lblNewLabel_2 = new JLabel("Welcome to OSOAA");
		lblNewLabel_2.setAlignmentX(Component.CENTER_ALIGNMENT);
		lblNewLabel_2.setHorizontalAlignment(SwingConstants.CENTER);
		panel_1.add(lblNewLabel_2);
		lblNewLabel_2.setFont(new Font("Tahoma", Font.PLAIN, 18));
		lblNewLabel_2.setForeground(Color.BLUE);

		JLabel labelVersion = new JLabel("version " + "2.0" );
		labelVersion.setBorder(new EmptyBorder(20, 0, 0, 0));
		labelVersion.setAlignmentX(Component.CENTER_ALIGNMENT);
		labelVersion.setForeground(Color.BLUE);
		panel_1.add(labelVersion);
		
		ImageJLabel oceanImgLabel = new ImageJLabel(ImageResources.getIconFromImage( ImageResources.getOceanImg() ) );
		oceanImgLabel.setBorder(new EmptyBorder(20, 0, 0, 0));
		oceanImgLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		panel_1.add(oceanImgLabel);
		
		welcomimgPanels = new JPanel();
		welcomimgPanels.setBackground(Color.WHITE);
		welcomimgPanels.setBorder(new EmptyBorder(0, 0, 50, 0));
		welcomimgPanels.add( new ImageJLabel(ImageResources.getIconFromImage( ImageResources.getCNRSImage() ) ) );
		welcomimgPanels.add( new ImageJLabel(ImageResources.getIconFromImage( ImageResources.getLogIUF2015Image() ) ) );
		welcomimgPanels.add( new ImageJLabel(ImageResources.getIconFromImage( ImageResources.getLogRepFrancaiseImage() ) ) );
		welcomimgPanels.add( new ImageJLabel(ImageResources.getIconFromImage( ImageResources.getLogoCNESNewImageH110() ) ) );
		welcomimgPanels.add( new ImageJLabel(ImageResources.getIconFromImage( ImageResources.getSorbonneImage() ) ) );
		welcomimgPanels.add( new ImageJLabel(ImageResources.getIconFromImage( ImageResources.getCSBigLogoImage() ) ) );
		
		jpCenter.add(welcomimgPanels, BorderLayout.SOUTH);
		
		welcomePanel.add(jpCenter, BorderLayout.CENTER);
		
		
		JPanel csPanels = new JPanel();
		csPanels.setBorder(new MatteBorder(1, 0, 0, 0, (Color) Color.LIGHT_GRAY));
		csPanels.setBackground(Color.WHITE);
		welcomePanel.add(csPanels, BorderLayout.SOUTH);
		//csPanels.add( new ImageJLabel(ImageResources.getIconFromImage( ImageResources.getCSLogoImage() ) ) );
		//csPanels.add( new JLabel( "<html><u>Scientific Manager :</u>&nbsp;&nbsp;&nbsp;Malik CHAMI (LOV)<BR/><u>Project Supervision :</u>&nbsp;&nbsp;&nbsp;Bertrand FOUGNIE (CNES)<BR/><u>Development Project Manager :</u>&nbsp;&nbsp;&nbsp;Bruno LAFRANCE (CSSI)</html>" ) );
		//csPanels.add( new JLabel( "<html><u>Scientific Principal Investigator :</u>&nbsp;&nbsp;&nbsp;Malik CHAMI (Sorbonne Universit&eacute)<BR/><u>Technical Principal Investigator :</u>&nbsp;&nbsp;&nbsp;Bruno LAFRANCE (CSSI company)<BR/><u>Project Officer :</u>&nbsp;&nbsp;&nbsp;Bertrand FOUGNIE (CNES)<BR/><u>Chief Project Officer :</u>&nbsp;&nbsp;&nbsp;Aim&eacute MEYGRET (CNES)</html>" ) );
		csPanels.add( new JLabel( "<html><u>Scientific Principal Investigator :</u>&nbsp;&nbsp;&nbsp;Malik CHAMI (Sorbonne Universit&eacute)<BR/><u>Technical Principal Investigator :</u>&nbsp;&nbsp;&nbsp;Bruno LAFRANCE (CS GROUP company)<BR/><u>Chief Project Officer :</u>&nbsp;&nbsp;&nbsp;Aim&eacute MEYGRET (CNES)</html>" ) );
		
		initFields();
		itemsList.setSelectedIndex(0);
	}

	private void initFields() {
		workingPahtJtextField.setText( commonPref.getWorkingAbsolutePathDir() );
		radianceWaveLengthSpinner.setValue( commonPref.getRadianceWaveLength() );
		solarZenithAngleSpinner.setValue( commonPref.getSolarZenithAngle() );
	}

	protected void validateWorkingPathField() {
		File f = new File(workingPahtJtextField.getText());
		FormUtils.setFieldState(!f.exists(), lblWorkingPath, Color.blue);
		
		saveWorkingPath(f.getAbsolutePath());
	}
	
	private void saveRadianceWaveLength(BigDecimal value_){
			commonPref.setRadianceWaveLength(value_);
	}
	
	private void saveSolarZenithAngle(BigDecimal value_){
		commonPref.setSolarZenithAngle(value_);
    }
	
	private void saveWorkingPath(String path_){
		if(path_!=null){
			commonPref.setWorkingAbsolutePathDir(path_);
		}
	}

	protected void onWorkingPathClicked() {
		final JFileChooser fc = new JFileChooser();
		fc.setApproveButtonText("OK");
		fc.setDialogTitle("Select your working directory");
		fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		int returnVal = fc.showOpenDialog(MainJFrame.this);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            //This is where a real application would open the file.
            workingPahtJtextField.setText(file.getAbsolutePath());
            workingPahtJtextField.setToolTipText(file.getAbsolutePath());
            
            validateWorkingPathField();
        }
	}

	private AtmosphericAndSeaProfilesJPanel atmosphericAndSeaProfilesJPanel;
	private AerosolsModelJPanel aerosolsModelJPanel;
	private HydroGroundModelJPanel hydrogroundModelJPanel;
	private SeaAndAtmosphereModelJPanel seaAndAtmosphereModelJPanel;
	private GeometricConditionsModelJPanel geometricConditionsModelJPanel;
	private OutputSpecificitiesJPanel outputSpecificitiesJPanel;
	
	private JPanel welcomimgPanels;
	private JPanel welcomePanel;
	private JTextField workingPahtJtextField;
	private JLabel lblWorkingPath;
	private Map<Integer, IFormValidation> mapFormValidation;
	private JList itemsList;
	private CommandPanel commandPanel;
	
	protected void onItemsSelected(final int selectedIndex) {
		Component componentToAdd;
		switch(selectedIndex){
			case 0: 
				componentToAdd = welcomePanel;
				break;
			case 1: 
				componentToAdd = atmosphericAndSeaProfilesJPanel;
				break;
			case 2: 
				componentToAdd = aerosolsModelJPanel;
				break;
			case 3: 
				componentToAdd = hydrogroundModelJPanel;
				break;
			case 4: 
				componentToAdd = seaAndAtmosphereModelJPanel;
				break;
			case 5: 
				componentToAdd = geometricConditionsModelJPanel;
				break;
			case 6 :
				componentToAdd = outputSpecificitiesJPanel;
				break;
			case 7 :
				componentToAdd = runCommandPanel;
				break;
			default :
				componentToAdd = new JLabel("Unknown panel at index  : " + selectedIndex);
		}
		
		if(componentToAdd!=null){
			updateMainContentPanel( componentToAdd );
			if( componentToAdd instanceof AbstractForm ){
				((AbstractForm) componentToAdd).checkChanges();
			}
		}
	}
	
	private void updateMainContentPanel(final Component jc_){
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
                try
                {
                    formScrollPane.setViewportView( jc_ );
                    formScrollPane.updateUI();
                }
                catch(Exception ex)
                {
                    log.error(ex);
                }
			}
		});
	}

}
