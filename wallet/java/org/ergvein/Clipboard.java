package org.ergvein;

import android.content.ClipboardManager;
import android.content.ClipData;
import android.content.ClipDescription;
import android.content.Context;
import android.util.Log;
import android.net.Uri;

import systems.obsidian.HaskellActivity;

public class Clipboard {
  private static void copyStr(final HaskellActivity a, String url) {

  }

  private static String pasteStr(final HaskellActivity activity) {
    ClipboardManager clipboard = (ClipboardManager) activity.getSystemService(Context.CLIPBOARD_SERVICE);
    if (!(clipboard.hasPrimaryClip())) {
      return "";
    } else if (clipboard.getPrimaryClipDescription().hasMimeType(ClipDescription.MIMETYPE_TEXT_PLAIN)) {
      ClipData.Item item = clipboard.getPrimaryClip().getItemAt(0);
      String pasteData = item.getText().toString();
      if (pasteData != null) {
        return pasteData;
      } else {
        return "";
      }
    } else if (clipboard.getPrimaryClipDescription().hasMimeType(ClipDescription.MIMETYPE_TEXT_URILIST)) {
      ClipData.Item item = clipboard.getPrimaryClip().getItemAt(0);
      Uri pasteUri = item.getUri();
      if (pasteUri != null) {
        String pasteData = resolveUri(pasteUri);
        if (pasteData != null) {
          return pasteData;
        } else {
          return "";
        }
      } else {
        return "";
      }
    } else {
      return "";
    }
  }

  private static String resolveUri(Uri uri) {
    return uri.toString();
  }
}
