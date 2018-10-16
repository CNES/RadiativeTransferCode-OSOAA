package osoaa.usl.common.ui.jtextpane;

import java.awt.Color;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.text.AttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;

import org.apache.log4j.Logger;

public class JTextPaneColor extends JTextPane {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public void append(Color c, String s) { // better implementation--uses
		// StyleContext
		StyleContext sc = StyleContext.getDefaultStyleContext();
		AttributeSet aset = sc.addAttribute(SimpleAttributeSet.EMPTY,
				StyleConstants.Foreground, c);

		int len = getDocument().getLength(); // same value as
		// getText().length();
		setCaretPosition(len); // place caret at the end (with no selection)
		setCharacterAttributes(aset, false);
		replaceSelection(s); // there is no selection, so inserts at caret
	}

	public static void main(String argv[]) {

		final JTextPaneColor pane = new JTextPaneColor();
		
		new Thread(){
			Logger log = Logger.getLogger("osoaa.usl.common.ui.jtextpane");
			public void run(){
				for(int i=0; i<5; i++){
					try {
						Thread.sleep(2000);
					} catch (InterruptedException e) {
						log.error(e.getMessage(), e);
					}
					for (int n = 1; n <= 400; n += 1) {
						if (isPrime(n)) {
							pane.append(Color.red, String.valueOf(n) + ' ');
						} else if (isPerfectSquare(n)) {
							pane.append(Color.blue, String.valueOf(n) + ' ');
						} else {
							pane.append(Color.black, String.valueOf(n) + ' ');
						}
					}
				}
			}
		}.start();
		
		

		JFrame f = new JFrame("ColorPane example");
		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		f.setContentPane(new JScrollPane(pane));
		f.setSize(600, 400);
		f.setVisible(true);
	}

	public static boolean isPrime(int n) {
		if (n < 2)
			return false;
		double max = Math.sqrt(n);
		for (int j = 2; j <= max; j += 1)
			if (n % j == 0)
				return false; // j is a factor
		return true;
	}

	public static boolean isPerfectSquare(int n) {
		int j = 1;
		while (j * j < n && j * j > 0)
			j += 1;
		return (j * j == n);
	}

}
