package com.manning.hip.ch6;

import org.apache.commons.lang.StringUtils;

import java.util.*;

public class StringTokBench {

  public static void main(String... args) {
    try {
      benchmark();
    } catch (Throwable t) {
      t.printStackTrace();
    }
  }


  public static void benchmark() {
    int numRuns = 10;
    int tokenLen = 5;

    int numStrings = 100000;

    StringBuilder sb = new StringBuilder();
    sb.append("# NUM-TOKENS")
        .append("\t").append("SCANNER")
        .append("\t").append("SPLIT")
        .append("\t").append("TOKENIZER")
        .append("\t").append("STRINGUTIL");
    System.out.println(sb);

    for (int numTokens = 1; numTokens <= 20; numTokens++) {
        benchmark(numRuns, numStrings, numTokens, tokenLen);
    }
  }

  public static void benchmark(int numRuns, int numStrings,
                               int numTokens, int tokenLen) {
    List<String> strings = genStrings(numStrings, numTokens, tokenLen);

    // warmup
    //
    for (int i = 0; i < 3; i++) {
      runScanner(strings);
      runSplit(strings);
      runTokenizer(strings);
      runStringUtils(strings);
    }

    // timed
    //
    long scanner = 0;
    long split = 0;
    long tok = 0;
    long strutil = 0;
    for (int i = 0; i < numRuns; i++) {
      scanner += runScanner(strings);
      split += runSplit(strings);
      tok += runTokenizer(strings);
      strutil += runStringUtils(strings);
    }

    StringBuilder sb = new StringBuilder();
    sb.append(numTokens)
        .append("\t").append(scanner / numRuns)
        .append("\t").append(split / numRuns)
        .append("\t").append(tok / numRuns)
        .append("\t").append(strutil / numRuns);

    System.out.println(sb);
  }

  public static long runScanner(List<String> strings) {
    long start = System.currentTimeMillis();

    for (String s : strings) {
      Scanner scanner = new Scanner(s);
      scanner.useDelimiter("\\s+");
      while (scanner.hasNext()) {
        String t = scanner.next();
        //System.out.println("runScanner: " + t);
      }
    }

    return System.currentTimeMillis() - start;
  }

  public static long runSplit(List<String> strings) {
    long start = System.currentTimeMillis();

    for (String s : strings) {
      String[] parts = s.split("\\s+");
      for (String p : parts) {
        //System.out.println("runSplit: " + p);
      }
    }

    return System.currentTimeMillis() - start;
  }

  public static long runTokenizer(List<String> strings) {
    long start = System.currentTimeMillis();

    for (String s : strings) {
      StringTokenizer tok = new StringTokenizer(s, " ");
      while (tok.hasMoreTokens()) {
        String t = tok.nextToken();
        //System.out.println("runTokenizer: " + t);
      }
    }

    return System.currentTimeMillis() - start;
  }

  public static long runStringUtils(List<String> strings) {
    long start = System.currentTimeMillis();

    for (String s : strings) {
      String[] parts = StringUtils.split(s, ' ');
      for (String p : parts) {
        //System.out.println("runStringUtils: " + p);
      }
    }

    return System.currentTimeMillis() - start;
  }


  public static List<String> genStrings(int numStrings, int numTokens,
                                        int tokenLen) {
    List<String> strings = new ArrayList<String>(numStrings);
    for (int i = 0; i < numStrings; i++) {
      strings.add(generateString(numTokens, tokenLen));
    }
    return strings;
  }


  public static String generateString(int numTokens, int tokenLen) {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < numTokens; i++) {
      if (i > 0) {
        sb.append(" ");
      }
      sb.append(generateToken(tokenLen));
    }
    return sb.toString();
  }

  public static Random rand = new Random();

  public static String generateToken(int tokenLen) {
    char[] chars = new char[tokenLen];
    for (int i = 0; i < tokenLen; i++) {
      chars[i] = (char) ('a' + rand.nextInt('z' - 'a'));
    }
    return new String(chars);
  }
}
