package mia.recommender.ch03;

import org.apache.mahout.cf.taste.impl.model.GenericUserPreferenceArray;
import org.apache.mahout.cf.taste.model.Preference;
import org.apache.mahout.cf.taste.model.PreferenceArray;

class CreatePreferenceArray {

  private CreatePreferenceArray() {
  }

  public static void main(String[] args) {
    PreferenceArray user1Prefs = new GenericUserPreferenceArray(2);
    user1Prefs.setUserID(0, 1L);
    user1Prefs.setItemID(0, 101L);
    user1Prefs.setValue(0, 2.0f);
    user1Prefs.setItemID(1, 102L);
    user1Prefs.setValue(1, 3.0f);
    Preference pref = user1Prefs.get(1);
    System.out.println(pref);
  }

}
