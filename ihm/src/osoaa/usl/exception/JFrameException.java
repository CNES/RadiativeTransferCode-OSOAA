package osoaa.usl.exception;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Toolkit;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingConstants;
import javax.swing.border.EmptyBorder;

import osoaa.common.string.StringUtils;
import osoaa.usl.resources.icons.IconResources;

public class JFrameException extends JFrame {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private JTextArea textArea;
	public JFrameException() {
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setTitle("OSOAA : Exception");
		setIconImage(IconResources.getFavIcon());
		getContentPane().setBackground(Color.WHITE);
		
		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setBorder(new EmptyBorder(10, 10, 10, 10));
		getContentPane().add(scrollPane, BorderLayout.CENTER);
		
		textArea = new JTextArea();
		textArea.setForeground(new Color(178, 34, 34));
		scrollPane.setViewportView(textArea);
		
		JLabel lblNewLabel = new JLabel("Ooops : an exception has occured...");
		lblNewLabel.setBorder(new EmptyBorder(10, 10, 10, 10));
		lblNewLabel.setForeground(Color.BLUE);
		lblNewLabel.setHorizontalAlignment(SwingConstants.CENTER);
		lblNewLabel.setFont(new Font("Tahoma", Font.BOLD, 14));
		lblNewLabel.setBackground(Color.WHITE);
		getContentPane().add(lblNewLabel, BorderLayout.NORTH);
		setPreferredSize(new Dimension(800, 600));
		pack();
		
		//Center on screen
		Toolkit toolkit = Toolkit.getDefaultToolkit();
	    Dimension ssize = toolkit.getScreenSize();
	    Dimension framesize = getSize();
	    int x = (int) ( ssize.getWidth() - framesize.getWidth() ) / 2;
	    int y = (int) ( ssize.getHeight() - framesize.getHeight() ) / 2;
	    setLocation( x, y );
	}

	public void setException(Exception e_){
		
		textArea.setText(
						"-------------------------\t" + SimpleDateFormat.getDateTimeInstance().format(new Date()) + "\t------------------------- \n" 
						+ e_.getMessage() 
						+ "\n--------------------------------------------------------------------------------------------------------------------------------\n" 
						+ StringUtils.getStackTrace(e_)
						+ "--------------------------------------------------------------------------------------------------------------------------------\n" 
				
				);
	}
	}
