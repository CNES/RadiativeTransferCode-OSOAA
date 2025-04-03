package osoaa.usl.command;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.*;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JToggleButton;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.TransferHandler;

import org.apache.log4j.Logger;

import net.miginfocom.swing.MigLayout;
import osoaa.usl.common.ui.jtextpane.JTextPaneColor;
import osoaa.usl.common.ui.time.TimeUtils;
import osoaa.usl.resources.icons.IconResources;

public class RunCommandPanel extends JPanel {
	/**
	 * 
	 */
	private Logger log = Logger.getLogger(this.getClass().getCanonicalName());
	
	private static final long serialVersionUID = 1L;
	private JTextArea cmdLineTxtArea;
	private JToggleButton formatCommandLineToogleBut;
	private JTextPaneColor processOutput_textArea;
	private JLabel startDateLabel;
	private JLabel endDateLabel;
	private JLabel timeElapsedLabel;
    private String unformatted_command_line;

    public RunCommandPanel() {
		processThreadList = new ArrayList<Thread>(3);
		setLayout(new BorderLayout(0, 0));

		JPanel panel = new JPanel();
		panel.setMaximumSize(new Dimension(19, 19));
		panel.setBackground(Color.WHITE);
		add(panel, BorderLayout.NORTH);
		panel.setLayout(new BorderLayout(0, 0));

		JLabel lblProcessTracker = new JLabel("Process tracker");
		lblProcessTracker.setHorizontalAlignment(SwingConstants.CENTER);
		lblProcessTracker.setForeground(Color.BLUE);
		lblProcessTracker.setFont(new Font("Tahoma", Font.BOLD, 14));
		lblProcessTracker.setBackground(Color.WHITE);
		panel.add(lblProcessTracker, BorderLayout.CENTER);

		JPanel panel_1 = new JPanel();
		panel_1.setBackground(Color.WHITE);
		panel.add(panel_1, BorderLayout.EAST);

		JButton stopButton = new JButton("");
		stopButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(runningProcess!=null){
					try {
                        log.debug("Looking for OSOAA_MAIN.exe pid...");
                        // String[] lookupCmd = new String[]{"/bin/bash","-c","/bin/ps -ef | /bin/grep OSOAA_MAIN.exe | /bin/grep -v grep | /bin/awk '{print $2}'" };
                        String pidToKill = null;
                        String[] lookupCmd = new String[]{"/bin/bash","-c","/bin/ps -ef" };
                        String[] killCmd = null;

                        try {
                            Runtime runtime = Runtime.getRuntime();
                            Process lookupProc = runtime.exec(lookupCmd);
                            lookupProc.waitFor();
                            BufferedReader reader = new BufferedReader(new InputStreamReader(lookupProc.getInputStream()));
                            String line = reader.readLine();
                            while (line != null) {
                                if(line.indexOf("OSOAA_MAIN.exe") != -1)
                                {
                                    String[] result=line.split(" +");
                                    log.debug("OSOAA_MAIN.exe pid : " + result[1]);
                                    pidToKill = result[1];
                                    killCmd = new String[]{"/bin/bash","-c","/bin/kill -9 " + pidToKill };
                                    break;
                                }
                                line = reader.readLine();
                            }
                        } catch (Exception exx) {
                            log.error(exx.getMessage(), exx);
                        }

                        if(pidToKill != null)
                        {
                            try {
                                Runtime runtime = Runtime.getRuntime();
                                Process killProc = runtime.exec(killCmd);
                                killProc.waitFor();
                                log.debug("Process " + pidToKill + " killed successfully");

                                processOutput_textArea.append(Color.RED, "\nProcess OSOAA_MAIN.exe killed successfully !");
                            } catch (Exception exx) {
                                log.error(exx.getMessage(), exx);
                            }
                        }

                        runningProcess.exitValue();
					} catch (IllegalThreadStateException e_) {
                        log.debug(e_.getMessage(), e_);
						runningProcess.destroy();
					}
                }
			}
		});
		stopButton.setIcon(IconResources.getIconFromImage(IconResources
				.getStopCommandIcon()));
		stopButton.setToolTipText("Stop the process");
		panel_1.add(stopButton);

		JPanel panel_2 = new JPanel();
		panel_2.setBackground(Color.WHITE);
		add(panel_2, BorderLayout.CENTER);
		panel_2.setLayout(new MigLayout("", "[176.00][::900px,grow]", "[][][][grow][grow]"));

		JLabel lblStartingDate = new JLabel("Start date :");
		panel_2.add(lblStartingDate, "cell 0 0,alignx right");

		startDateLabel = new JLabel("No process running");
		panel_2.add(startDateLabel, "cell 1 0");

		JLabel lblEndDate = new JLabel("End date :");
		panel_2.add(lblEndDate, "cell 0 1,alignx right");

		endDateLabel = new JLabel("/");
		panel_2.add(endDateLabel, "cell 1 1");

		JLabel lblTimeElapsed = new JLabel("Time elapsed :");
		panel_2.add(lblTimeElapsed, "cell 0 2,alignx right");

		timeElapsedLabel = new JLabel("/");
		panel_2.add(timeElapsedLabel, "cell 1 2");

		JPanel panel_3 = new JPanel();
		panel_3.setBackground(Color.WHITE);
		panel_2.add(panel_3, "cell 0 3,alignx right,aligny top");
		panel_3.setLayout(new BoxLayout(panel_3, BoxLayout.Y_AXIS));

		JLabel label = new JLabel("Executed command :");
		panel_3.add(label);

		JPanel panel_4 = new JPanel();
		FlowLayout flowLayout = (FlowLayout) panel_4.getLayout();
		flowLayout.setVgap(15);
		panel_4.setBackground(Color.WHITE);
		panel_3.add(panel_4);

		JButton copyBtn = new JButton("");
		copyBtn.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				TransferHandler th = cmdLineTxtArea.getTransferHandler();
				cmdLineTxtArea.selectAll();
				th.exportToClipboard(cmdLineTxtArea, getToolkit()
						.getSystemClipboard(), TransferHandler.COPY);
			}
		});
		copyBtn.setIcon(IconResources.getIconFromImage(IconResources
				.getCopyIcon()));
		copyBtn.setToolTipText("Copy the command line");
		panel_4.add(copyBtn);

		formatCommandLineToogleBut = new JToggleButton("Format");
		formatCommandLineToogleBut.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
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
							System.out.println("i=" + i + " -> "+ fragments[i]);
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
		});
		formatCommandLineToogleBut.setIcon(IconResources
				.getIconFromImage(IconResources.getFormatCommandIcon()));
		panel_4.add(formatCommandLineToogleBut);

		JScrollPane scrollPane = new JScrollPane();
		panel_2.add(scrollPane, "cell 1 3,grow");

		cmdLineTxtArea = new JTextArea();
		cmdLineTxtArea.setEditable(false);
		scrollPane.setViewportView(cmdLineTxtArea);
		cmdLineTxtArea.setText("No process running");
		cmdLineTxtArea.setLineWrap(true);

		JPanel panel_5 = new JPanel();
		panel_5.setBackground(Color.WHITE);
		panel_2.add(panel_5, "cell 0 4,alignx right,aligny top");
		panel_5.setLayout(new BoxLayout(panel_5, BoxLayout.Y_AXIS));

		JLabel lblNewLabel = new JLabel("Process output :");
		lblNewLabel.setBackground(Color.WHITE);
		panel_5.add(lblNewLabel);

		JPanel panel_6 = new JPanel();
		panel_6.setBackground(Color.WHITE);
		panel_5.add(panel_6);

		JButton copyOutprocessButton = new JButton("");
		copyOutprocessButton.setToolTipText("Copy the output process");
		copyOutprocessButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				TransferHandler th = processOutput_textArea.getTransferHandler();
				processOutput_textArea.selectAll();
				th.exportToClipboard(processOutput_textArea, getToolkit()
						.getSystemClipboard(), TransferHandler.COPY);
			}
		});
		copyOutprocessButton.setIcon(IconResources
				.getIconFromImage(IconResources.getCopyIcon()));
		panel_6.add(copyOutprocessButton);

		JScrollPane scrollPane_1 = new JScrollPane();
		panel_2.add(scrollPane_1, "cell 1 4,grow");

		processOutput_textArea = new JTextPaneColor();
		processOutput_textArea.addKeyListener(new KeyAdapter() {
			@Override
			public void keyTyped(KeyEvent arg0) {
				arg0.consume();
			}
		});
		scrollPane_1.setViewportView(processOutput_textArea);
	}

	protected void onStopAction() {
		if (runningProcess != null) {
			try {
				runningProcess.exitValue();
			} catch (IllegalThreadStateException e) {
				runningProcess.destroy();
			}

			for (Thread t : processThreadList) {
				t.interrupt();
			}
			processThreadList.clear();

			final long now = System.currentTimeMillis();
			long timeElapsedMs = now - startDate;
			final String durationStr = TimeUtils
					.millisToLongDHMS(timeElapsedMs);

            try {
                Thread.sleep(1500);
            } catch (InterruptedException e) {
                // wait stdout flush
                // do nothing
            }

            SwingUtilities.invokeLater(new Runnable() {

				@Override
				public void run() {
                    try
                    {
                        timeElapsedLabel.setText(durationStr);
                        String dateStr = SimpleDateFormat.getDateTimeInstance().format(
                                new Date(now));
                        endDateLabel.setText(dateStr);

                        processOutput_textArea.append(Color.BLACK, "\n");

                        if(runningProcess.exitValue() == 0)
                        {
                            processOutput_textArea.append(Color.BLACK, "Program terminated successfully.");
                        }
                        else
                        {
                            processOutput_textArea.append(Color.BLACK, "Program terminated abnormally: EXIT CODE=" + runningProcess.exitValue());
                        }
                    }
                    catch(Exception ex)
                    {
                        log.error(ex);
                    }
				}
			});
		}
	}

	public void setCommandLine(String command) {
        this.unformatted_command_line = command;
		cmdLineTxtArea.setText(command);
	}

	private long startDate;
	private List<Thread> processThreadList;
	private Process runningProcess;

	public void setRunningProcess(Process runningProcess_) {
		
		runningProcess = runningProcess_;
        startDate = System.currentTimeMillis();

		SwingUtilities.invokeLater(new Runnable() {

			@Override
			public void run() {
                try
                {
				endDateLabel.setText("/");
				processOutput_textArea.setText("");
				
				startDate = System.currentTimeMillis();
				String dateStr = SimpleDateFormat.getDateTimeInstance().format(
						new Date(startDate));
				startDateLabel.setText(dateStr);
                }
                catch(Exception ex)
                {
                    log.error(ex);
                }

			}
		});

		Thread t0 = new Thread("Waiting the end of the process") {
			@Override
			public void run() {
				try {
					runningProcess.waitFor();
				} catch (InterruptedException e) {
					log.debug(e.getMessage(), e);
				}

				onStopAction();
			}
		};
		t0.start();

		Thread t1 = new Thread("Running process time elapsed") {

			@Override
			public void run() {
				boolean isStopping = false;
				while(!isStopping){
					try {
						Thread.sleep(500);
						
						long timeElapsedMs = System.currentTimeMillis() - startDate;
						final String durationStr = TimeUtils
								.millisToLongDHMS(timeElapsedMs);
						SwingUtilities.invokeLater(new Runnable() {
		
							@Override
							public void run()
                            {
                                try
                                {
								    timeElapsedLabel.setText(durationStr);
                                }
                                catch(Exception ex)
                                {
                                    log.error(ex);
                                }
							}
						});
					} catch (InterruptedException e) {
						log.debug(e.getMessage(), e);
						isStopping = true;
					}
                    catch (Exception ex)
                    {
                        log.error(ex);
                    }
				}
			}
		};
		processThreadList.add(t1);

		Thread t2 = new Thread("Running process error stream") {
			private int ch;

			@Override
			public void run() {

				InputStream ipSt = runningProcess.getErrorStream();
				try { // in the spec., read(), write() could throw IOException
                    final StringBuffer errorResult = new StringBuffer();
                    StringBuffer theLine = new StringBuffer();

					while ((ch = ipSt.read()) != -1)
                    {
                        errorResult.append((char) ch);
                        theLine.append((char) ch);

                        if(((char) ch) == '\n' || ((char) ch) == '\r')
                        {
                            String partial = theLine.toString();
                            SwingUtilities.invokeLater( new StreamConsumer(Color.RED, processOutput_textArea, partial) );
                            theLine.setLength(0);
                            log.info(partial);
                        }
					}
                    // log.debug("[ERROR] : " + errorResult.toString());
				} catch (Exception ex_) {
					log.error(ex_.getMessage(), ex_);
				}
			}

		};
		processThreadList.add(t2);

		Thread t3 = new Thread("Running process output stream")
        {
			private int ch;

			@Override
			public void run() {
				InputStream ipSt = runningProcess.getInputStream();
				try { // in the spec., read(), write() could throw IOException
					final StringBuffer commandResult = new StringBuffer();
                    StringBuffer theLine = new StringBuffer();

                    int index = 0;
					while ((ch = ipSt.read()) != -1)
					{
						commandResult.append((char) ch);
                        theLine.append((char) ch);

                        if(((char) ch) == '\n' || ((char) ch) == '\r')
                        {
                            String partial = theLine.toString();
                            SwingUtilities.invokeLater( new StreamConsumer(Color.BLUE, processOutput_textArea, partial) );
                            theLine.setLength(0);
                            log.info(partial);
                        }
					}

                    // log.debug("[OUTPUT] : " + commandResult.toString());
				} catch (Exception ex_) {
					log.error(ex_.getMessage(), ex_);
				}
			}

		};
		processThreadList.add(t3);

		for (Thread t : processThreadList) {
			t.setDaemon(true);
			t.start();
		}
	}

	public void setErrorOnRunningProcess(final IOException e1) {
		SwingUtilities.invokeLater(new Runnable() {

			@Override
			public void run() {
                try
                {
				    processOutput_textArea.append(Color.red,
						"\nERROR : \n" + e1.toString());
                }
                catch(Exception ex)
                {
                    log.error(ex);
                }
			}
		});
	}

}

class StreamConsumer implements Runnable
{

    private Logger log = Logger.getLogger(this.getClass().getCanonicalName());

    private final JTextPaneColor panel;
    private final String buffer;
    private final Color color;

    public StreamConsumer(final Color col, final JTextPaneColor _panel, final String sbu)
    {
        assert(_panel != null);
        this.panel = _panel;
        assert(sbu != null);
        this.buffer = sbu;
        this.color = col;
    }

    @Override
    public void run()
    {
        try
        {
            panel.append(color, buffer);
        }
        catch (Exception e)
        {
            log.error(e);
        }
    }
}
