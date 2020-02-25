package org.ergvein;

import android.content.ClipboardManager;
import android.content.ClipData;
import android.content.ClipDescription;
import android.content.Context;
import android.content.Intent;
import android.util.Log;
import android.net.Uri;
//import android.net.Uri.Builder;

import java.util.ArrayList;

import systems.obsidian.HaskellActivity;

public class Share {

  private static void shareUrl(final HaskellActivity a, String url) {
    final Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
    a.startActivity(Intent.createChooser(intent, "Share test to.."));

//    ArrayList<Uri> listUris = new ArrayList<Uri>();
    //listUris.add(Uri.parse("http://www.google.com"));
//    listUris.add(Uri.parse(url));

//    Intent shareIntent = new Intent();
//    shareIntent.setAction(Intent.ACTION_SEND_MULTIPLE);
    //sendIntent.putExtra(Intent.EXTRA_TEXT, url);
//    shareIntent.putParcelableArrayListExtra(Intent.EXTRA_STREAM, listUris);
//    shareIntent.setType("application/bitcoin-paymentrequest");
    //shareIntent.setType("*/*");

    //Intent shareIntent = Intent.createChooser(sendIntent, null);
//    a.startActivity(Intent.createChooser(shareIntent, "Share test to.."));
  }

}
