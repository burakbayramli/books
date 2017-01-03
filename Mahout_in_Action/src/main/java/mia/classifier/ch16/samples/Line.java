package mia.classifier.ch16.samples;

import java.util.List;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;

public class Line {
	private static final String SEPARATOR = "\t ";
	private static final Splitter onTabs = Splitter.on(SEPARATOR);
	private List<String> data;

	Line(String line) {
		data = Lists.newArrayList(onTabs.split(line));
	}

	public double getDouble(int field) {
		return Double.parseDouble(data.get(field));
	}
}