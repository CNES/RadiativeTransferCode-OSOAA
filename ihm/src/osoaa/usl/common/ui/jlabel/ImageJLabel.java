package osoaa.usl.common.ui.jlabel;

import java.awt.Insets;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.border.EmptyBorder;

public class ImageJLabel extends JLabel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private void init(){
		setBorder(new EmptyBorder(new Insets(10, 10, 10, 10)));
	}
	
	public ImageJLabel() {
		super();
		init();
	}

	public ImageJLabel(String arg0) {
		super(arg0);
		init();
	}

	public ImageJLabel(Icon arg0) {
		super(arg0);
		init();
	}

	public ImageJLabel(String arg0, int arg1) {
		super(arg0, arg1);
		init();
	}

	public ImageJLabel(Icon arg0, int arg1) {
		super(arg0, arg1);
		init();
	}

	public ImageJLabel(String arg0, Icon arg1, int arg2) {
		super(arg0, arg1, arg2);
		init();
	}

}
