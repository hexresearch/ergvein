package org.ergvein;

import android.view.WindowManager;
import systems.obsidian.HaskellActivity;

public class ScreenManager {
  private static void setScreenFlag(final HaskellActivity a) {
    a.getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
  }

  private static void clearScreenFlag(final HaskellActivity a) {
    a.getWindow().clearFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
  }
}
