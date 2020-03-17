package org.ergvein;

import android.content.ClipboardManager;
import android.content.ClipData;
import android.content.ClipDescription;
import android.content.Context;
import android.content.Intent;
import android.util.Log;
import android.net.Uri;
import android.net.Uri.Builder;
import android.text.Html;

import java.util.ArrayList;
import java.io.File;
import java.io.FileOutputStream;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.util.Base64;
import android.provider.MediaStore;

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
