/*
 * Source Code for Listing 10.4
 * 
 */
package mia.clustering.ch10;

import java.io.Reader;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.LengthFilter;
import org.apache.lucene.analysis.LowerCaseFilter;
import org.apache.lucene.analysis.PorterStemFilter;
import org.apache.lucene.analysis.StopFilter;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.analysis.standard.StandardTokenizer;
import org.apache.lucene.util.Version;

public class MyAnalyzer extends Analyzer {
  
  @SuppressWarnings("deprecation")
  @Override
  public TokenStream tokenStream(String fieldName, Reader reader) {
    TokenStream result = new StandardTokenizer(
        Version.LUCENE_CURRENT, reader);
    result = new LowerCaseFilter(result);
    result = new LengthFilter(result, 3, 50);
    result = new StopFilter(true, result, StandardAnalyzer.STOP_WORDS_SET);
    result = new PorterStemFilter(result);
    return result;
  }
}
