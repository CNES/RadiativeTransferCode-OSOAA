package osoaa.usl.common.ui.jspinner;


import java.math.BigDecimal;
import java.math.BigInteger;

import javax.swing.SpinnerNumberModel;

public abstract class SpinnerTypedNumberModel<T extends Number> extends
	SpinnerNumberModel
{

	private final Class<T> type;

	private T value;
	private Comparable<T> min;
	private Comparable<T> max;
	private T stepSize;

	public SpinnerTypedNumberModel(final Class<T> type, final T value,
		final Comparable<T> min, final Comparable<T> max, final T stepSize)
	{
		super(value, min, max, stepSize);
		this.type = type;
		this.value = value;
		this.min = min;
		this.max = max;
		this.stepSize = stepSize;
	}

	protected abstract T stepUp();
	protected abstract T stepDown();

	@Override
	public Comparable<T> getMaximum() {
		return max;
	}

	@Override
	public Comparable<T> getMinimum() {
		return min;
	}

	@Override
	public T getNextValue() {
		final T newValue = stepUp();
		if (max != null && max.compareTo(newValue) < 0) return null;
		return newValue;
	}

	@Override
	public T getNumber() {
		return value;
	}

	@Override
	public T getPreviousValue() {
		final T newValue = stepDown();
		if (min != null && min.compareTo(newValue) > 0) return null;
		return newValue;
	}

	@Override
	public T getStepSize() {
		return stepSize;
	}

	@Override
	public T getValue() {
		return value;
	}

	@Override
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public void setMaximum(final Comparable maximum) {
		max = maximum;
		super.setMaximum(maximum);
	}

	@Override
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public void setMinimum(final Comparable minimum) {
		min = minimum;
		super.setMinimum(minimum);
	}

	@Override
	public void setStepSize(final Number stepSize) {
		if (stepSize == null || !type.isInstance(stepSize)) {
			throw new IllegalArgumentException("illegal value");
		}
		@SuppressWarnings("unchecked")
		final T typedStepSize = (T) stepSize;
		this.stepSize = typedStepSize;
		super.setStepSize(stepSize);
	}

	@Override
	public void setValue(final Object value) {
        if (value == null) {
            throw new IllegalArgumentException("null value");
        }

		if (!type.isInstance(value)) {
			throw new IllegalArgumentException("illegal value");
		}
		@SuppressWarnings("unchecked")
		final T typedValue = (T) value;
		this.value = typedValue;
		super.setValue(value);
	}

}
