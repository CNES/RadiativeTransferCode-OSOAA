package osoaa.usl.forms;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;
import javax.swing.UIManager;

import osoaa.usl.common.ui.forms.IFormValidation;
import osoaa.usl.common.ui.forms.IFormValidationListener;
import osoaa.usl.resources.icons.IconResources;

import com.jgoodies.forms.factories.FormFactory;
import com.jgoodies.forms.layout.ColumnSpec;
import com.jgoodies.forms.layout.FormLayout;
import com.jgoodies.forms.layout.RowSpec;

public abstract class AbstractForm extends JPanel implements IFormValidation  {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	private List<IFormValidationListener> formValidationList;

	private JPanel titlePanel;
	private JPanel formFieldsPanel;

	public AbstractForm(String title_){
		super();
		setAlignmentY( JPanel.TOP_ALIGNMENT );
		formValidationList = new ArrayList<IFormValidationListener>();
		initTilePanel(title_);
		initFormFieldsPanel();
	}
	
	private void initTilePanel(String title_){
		titlePanel = new JPanel();
		titlePanel.setBackground(Color.WHITE);
		titlePanel.setLayout(new FormLayout(new ColumnSpec[] {
				FormFactory.RELATED_GAP_COLSPEC,
				ColumnSpec.decode("max(67dlu;default)"),
				FormFactory.RELATED_GAP_COLSPEC,
				ColumnSpec.decode("max(119dlu;default):grow"),
				FormFactory.RELATED_GAP_COLSPEC,
				FormFactory.DEFAULT_COLSPEC,
				FormFactory.RELATED_GAP_COLSPEC,
				ColumnSpec.decode("max(4dlu;default)"),
				FormFactory.DEFAULT_COLSPEC,
				FormFactory.RELATED_GAP_COLSPEC,
				FormFactory.GLUE_COLSPEC,},
			new RowSpec[] {
				FormFactory.RELATED_GAP_ROWSPEC,
				RowSpec.decode("max(8dlu;default)"),
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				RowSpec.decode("max(17dlu;default)"),
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				RowSpec.decode("max(4dlu;default)"),}));

		
		JLabel lblNewLabel = new JLabel(title_);
		lblNewLabel.setHorizontalAlignment(SwingConstants.CENTER);
		lblNewLabel.setBackground(Color.WHITE);
		lblNewLabel.setForeground(Color.BLUE);
		lblNewLabel.setFont(new Font("Tahoma", Font.BOLD, 14));
		titlePanel.add(lblNewLabel, "2, 4, 8, 1");
		
		JLabel lblNewLabel_1 = new JLabel("Note : * means required field");
		lblNewLabel_1.setFont(new Font("Tahoma", Font.ITALIC, 11));
		lblNewLabel_1.setForeground(Color.BLUE);
		titlePanel.add(lblNewLabel_1, "2, 6, 3, 1");
		
		JPanel panel = new JPanel();
		panel.setBackground(Color.WHITE);
		FlowLayout flowLayout = (FlowLayout) panel.getLayout();
		flowLayout.setVgap(0);
		flowLayout.setHgap(0);
		flowLayout.setAlignment(FlowLayout.RIGHT);
		titlePanel.add(panel, "9, 6, 1, 2, fill, top");
		
		JButton resetBtn = new JButton("Reset form");
		resetBtn.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				onResetBtnAction();
			}
		});
		setLayout(new BorderLayout(0, 0));
		resetBtn.setIcon(IconResources.getIconFromImage(IconResources.getResetFormIcon()));
		panel.add(resetBtn);

		JSeparator separator_2 = new JSeparator();
		separator_2.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleBackground"));
		titlePanel.add(separator_2, "2, 8, 8, 1");
		
		add(titlePanel, BorderLayout.NORTH);
	}

	private void initFormFieldsPanel(){
		formFieldsPanel = new JPanel();
		formFieldsPanel.setBackground(Color.WHITE);
		add(formFieldsPanel, BorderLayout.CENTER);
	}
	
	protected JPanel getFormFieldsPanel(){
		return formFieldsPanel;
	}
	
	abstract void onResetBtnAction();

	@Override
	public void addFormValidationListener(IFormValidationListener listener_) {
		formValidationList.add(listener_);
	}

	@Override
	public void removeFormValidationListener(IFormValidationListener listener_) {
		formValidationList.remove(listener_);
	}
	
	public void notifyFormValidated(){
		boolean isFormValid = isFormValid();
		for(IFormValidationListener listener : formValidationList ){
			listener.onFormValidated(isFormValid);
		}
	}
	

	@Override
	public abstract boolean isFormValid();

	/**
	 * This method is called each time the panel is displayed
	 */
	public void checkChanges() {
		
	}
}
