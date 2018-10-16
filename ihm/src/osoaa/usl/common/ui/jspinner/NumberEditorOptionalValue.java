package osoaa.usl.common.ui.jspinner;

import org.apache.log4j.Logger;

import javax.swing.*;
import javax.swing.text.DefaultFormatterFactory;
import javax.swing.text.NumberFormatter;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.Locale;

public class NumberEditorOptionalValue extends JSpinner.DefaultEditor {
    private static final long serialVersionUID = 1L;


    public NumberEditorOptionalValue(JSpinner spinner) {
        this(spinner, new DecimalFormat());
    }

    public NumberEditorOptionalValue(JSpinner spinner,
                                     String decimalFormatPattern) {
        this (spinner, new DecimalFormat(decimalFormatPattern));
    }

    private NumberEditorOptionalValue(JSpinner spinner, DecimalFormat format) {
        super (spinner);

        Logger log = Logger.getLogger("osoaa.usl.common.ui.jspinner.NumberEditorOptionalValue");
        SpinnerNumberModel model = ((OptionalSpinnerNumberModel) spinner.getModel()).getSpinnerNumberModel();
        NumberFormatter formatter = new NumberEditorFormatter(model, format){

            /**
             *
             */
            private static final long serialVersionUID = 1L;

            @Override
            public Object stringToValue(String arg0) throws ParseException {
                if( arg0 == null || arg0.trim().length() == 0){
                    return null;
                } else {
                    return super.stringToValue(arg0);
                }
            }

            @Override
            public String valueToString(Object arg0) throws ParseException {
                if(arg0 == null){
                    return "";
                } else {
                    return super.valueToString(arg0);
                }
            }

        };
        DefaultFormatterFactory factory = new DefaultFormatterFactory(formatter);
        JFormattedTextField ftf = getTextField();
        ftf.setEditable(true);
        ftf.setFormatterFactory(factory);
        ftf.setHorizontalAlignment(JTextField.RIGHT);

        try {
            String maxString = formatter.valueToString(model
                    .getMinimum());
            String minString = formatter.valueToString(model
                    .getMaximum());
            ftf.setColumns(Math.max(maxString.length(), minString
                    .length()));
        } catch (ParseException e) {
            log.error(e.getMessage(), e);
        }

    }

    /**
     * Returns the <code>java.text.DecimalFormat</code> object the
     * <code>JFormattedTextField</code> uses to parse and format
     * numbers.
     *
     * @return the value of <code>getTextField().getFormatter().getFormat()</code>.
     * @see #getTextField
     * @see java.text.DecimalFormat
     */
    public DecimalFormat getFormat() {
        return (DecimalFormat) ((NumberFormatter) (getTextField()
                .getFormatter())).getFormat();
    }

    /**
     * Return our spinner ancestor's <code>SpinnerNumberModel</code>.
     *
     * @return <code>getSpinner().getModel()</code>
     * @see #getSpinner
     * @see #getTextField
     */
    public SpinnerNumberModel getModel() {
        return (SpinnerNumberModel) (getSpinner().getModel());
    }



    /**
     * This subclass of javax.swing.NumberFormatter maps the minimum/maximum
     * properties to a SpinnerNumberModel and initializes the valueClass
     * of the NumberFormatter to match the type of the initial models value.
     */
    private static class NumberEditorFormatter extends NumberFormatter {
        /**
         *
         */
        private static final long serialVersionUID = 1L;
        private final SpinnerNumberModel model;

        NumberEditorFormatter(SpinnerNumberModel model,
                              NumberFormat format) {
            super(format);
            this.model = model;
            setValueClass(model.getValue().getClass());
        }

        public void setMinimum(Comparable min) {
            model.setMinimum(min);
        }

        public Comparable getMinimum() {
            return model.getMinimum();
        }

        public void setMaximum(Comparable max) {
            model.setMaximum(max);
        }

        public Comparable getMaximum() {
            return model.getMaximum();
        }
    }

}