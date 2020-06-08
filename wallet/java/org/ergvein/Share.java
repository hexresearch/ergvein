package org.ergvein;

import android.content.Intent;
import android.net.Uri;

import systems.obsidian.HaskellActivity;

public class Share {

  private static void shareUrl(final HaskellActivity a, String url) {
    final Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));

    final Intent intentText = new Intent(Intent.ACTION_SEND);
    intentText.setType("text/plain");
    intentText.putExtra(Intent.EXTRA_SUBJECT, "My address");
    intentText.putExtra(Intent.EXTRA_TEXT, url);

    Intent chooserIntent = Intent.createChooser(intentText, "Open in...");
    chooserIntent.putExtra(Intent.EXTRA_INITIAL_INTENTS, new Intent[] { intent });
    a.startActivity(chooserIntent);
  }

}
