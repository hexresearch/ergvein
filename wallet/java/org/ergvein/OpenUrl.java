package org.ergvein;

import android.content.Intent;
import android.net.Uri;

import systems.obsidian.HaskellActivity;

public class OpenUrl {

  private static void openUrl(final HaskellActivity a, String url) {
    final Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
    a.startActivity(intent);
  }

}
