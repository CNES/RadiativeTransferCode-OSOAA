package osoaa.usl.command;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.MessageFormat;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingConstants;
import javax.swing.TransferHandler;
import javax.swing.border.EmptyBorder;

import osoaa.usl.resources.icons.IconResources;
import javax.swing.JToggleButton;

public class CommandPanel extends JPanel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private JTextArea cmdLineTxtArea;
	public final static String UNDEFINED_CMD = "No command defined.";
	private JToggleButton formatCommandLineToogleBut;
	private JLabel lblGeneratedCommandLine;
	private String lblGeneratedCommandLinePattern = "Generated command line {0}";
    private String unformatted_command_line;

    public CommandPanel() {
		setBackground(Color.WHITE);
		setLayout(new BorderLayout(0, 0));
		
		JScrollPane scrollPane = new JScrollPane();
		add(scrollPane, BorderLayout.CENTER);
		
		cmdLineTxtArea = new JTextArea();
		cmdLineTxtArea.setLineWrap(true);
		cmdLineTxtArea.setText("Loading...");
		scrollPane.setViewportView(cmdLineTxtArea);
		
		JPanel panel = new JPanel();
		panel.setMaximumSize(new Dimension(19, 19));
		panel.setBackground(Color.WHITE);
		add(panel, BorderLayout.NORTH);
		panel.setLayout(new BorderLayout(0, 0));
		
		lblGeneratedCommandLine = new JLabel(MessageFormat.format(lblGeneratedCommandLinePattern, ""));
		panel.add(lblGeneratedCommandLine, BorderLayout.CENTER);
		lblGeneratedCommandLine.setBackground(Color.WHITE);
		lblGeneratedCommandLine.setBorder(new EmptyBorder(10, 10, 10, 10));
		lblGeneratedCommandLine.setFont(new Font("Tahoma", Font.PLAIN, 14));
		lblGeneratedCommandLine.setForeground(Color.BLUE);
		lblGeneratedCommandLine.setHorizontalAlignment(SwingConstants.CENTER);
		
		JPanel panel_1 = new JPanel();
		panel_1.setBackground(Color.WHITE);
		panel.add(panel_1, BorderLayout.EAST);
		
		JButton copyBtn = new JButton("");
		panel_1.add(copyBtn);
		copyBtn.setBorder(new EmptyBorder(5, 5, 5, 5));
		copyBtn.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				TransferHandler th = cmdLineTxtArea.getTransferHandler();
				cmdLineTxtArea.selectAll();
				th.exportToClipboard(cmdLineTxtArea, getToolkit().getSystemClipboard(), TransferHandler.COPY);
			}
		});
		copyBtn.setIcon( IconResources.getIconFromImage(IconResources.getCopyIcon()));
		copyBtn.setToolTipText("Copy the command line");
		
		formatCommandLineToogleBut = new JToggleButton("Format");
		formatCommandLineToogleBut.addActionListener(new ActionListener() {
			
			public void actionPerformed(ActionEvent e) {
				onFormatCmdLine();
			}
		});
		formatCommandLineToogleBut.setBorder(new EmptyBorder(5, 5, 5, 5));
		formatCommandLineToogleBut.setIcon( IconResources.getIconFromImage(IconResources.getFormatCommandIcon()));
		panel_1.add(formatCommandLineToogleBut);
	}
	
	protected void onFormatCmdLine()
    {
        if(!formatCommandLineToogleBut.isSelected())
        {
            setCommandLine(unformatted_command_line);
            cmdLineTxtArea.setText(unformatted_command_line);
        }
        else
        {
            String aSrc = cmdLineTxtArea.getText();

            if(aSrc.contains("\\\n"))
            {
                cmdLineTxtArea.setText(aSrc);
            }
            else
            {
                String[] fragments = aSrc.split(" ");

                String newLineSeparator = " \\\n";
                StringBuffer sbu = new StringBuffer();
                sbu.append(fragments[0]);
                sbu.append(newLineSeparator);
                for(int i=1; i<fragments.length; i=i+2)
                {
                    sbu.append(fragments[i]);
                    sbu.append(" ");
                    sbu.append(fragments[i+1]);
                    if (i+2 < fragments.length)
                    {
                        sbu.append(newLineSeparator);
                    }
                }

                cmdLineTxtArea.setText(sbu.toString());
            }
        }
	}

	public void setCommandLine(String cmd_) {
        this.unformatted_command_line = cmd_;

		if( cmd_ == null || cmd_.length()<=0){
			cmdLineTxtArea.setText( UNDEFINED_CMD );
			formatCommandLineToogleBut.setEnabled(false);
		}
        else
        {
			int nbParameters = (cmd_.split(" ").length - 1)/ 2;
			String paramMsg;
			if(nbParameters>1){
				paramMsg = "(" + nbParameters + " parameters)";
			} else {
				paramMsg = "(" + nbParameters + " parameter)";
			}
			lblGeneratedCommandLine.setText( MessageFormat.format(lblGeneratedCommandLinePattern, paramMsg) );

			cmdLineTxtArea.setText(cmd_);
			if( ! formatCommandLineToogleBut.isEnabled() ){
				formatCommandLineToogleBut.setEnabled(true);
			}
		}
		
		if( formatCommandLineToogleBut.isSelected() ){
			onFormatCmdLine();
		}
	}

	
}
