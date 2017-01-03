/*
 * Source code for Listing 16.2
 * 
 */
package mia.classifier.ch16.samples;

import java.nio.ByteBuffer;

import org.apache.mahout.math.list.IntArrayList;

public class FastLine {
	private static final int SEPARATOR_CHAR = '\t';
	private ByteBuffer base;
	private IntArrayList start = new IntArrayList();
	private IntArrayList length = new IntArrayList();

	private FastLine(ByteBuffer base) {
		this.base = base;
	}

	public static FastLine read(ByteBuffer buf) {
		FastLine r = new FastLine(buf);
		r.start.add(buf.position());
		int offset = buf.position();
		while (offset < buf.limit()) {
			int ch = buf.get();
			switch (ch) {
			case '\n':
				r.length.add(offset - r.start.get(r.length.size()) - 1);
				return r;
			case SEPARATOR_CHAR:
				r.length.add(offset - r.start.get(r.length.size()) - 1);
				r.start.add(offset);
				break;
			default:
			}
		}
		// TODO: how it's better to handle end of buffer?
		//  Should it be implemented in FastLineReader?
		if(!buf.hasRemaining())
			return null;
		throw new IllegalArgumentException("Not enough bytes in buffer");
	}
	
	public double getDouble(int field) {
		int offset = start.get(field);
		int size = length.get(field);
		switch (size) {
		case 1:
			return base.get(offset) - '0';
		case 2:
			return (base.get(offset) - '0') * 10 + base.get(offset + 1) - '0';
		default:
			double r = 0;
			for (int i = 0; i < size; i++) {
				r = 10 * r + base.get(offset + i) - '0';
			}
			return r;
		}
	}
}