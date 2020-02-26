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
//import java.io.File;
//import java.io.FileOutputStream;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.util.Base64;
import android.provider.MediaStore;

import systems.obsidian.HaskellActivity;

public class Share {

  private static void shareUrl(final HaskellActivity a, String url) {
    // For open other app
    final Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
    a.startActivity(Intent.createChooser(intent, "Share .."));

    //Open image as Base64
    //decode base64 string to image
//    byte[] imageBytes = Base64.decode(url, Base64.DEFAULT);
//    Bitmap bitmap = BitmapFactory.decodeByteArray(imageBytes, 0, imageBytes.length);

//    Intent intentBase64 = new Intent(Intent.ACTION_SEND);
//    String urlQR= MediaStore.Images.Media.insertImage(a.getContentResolver(), bitmap, "title", "description");
//    intentBase64.putExtra(Intent.EXTRA_STREAM, Uri.parse(urlQR));
//    intentBase64.setType("image/*");
    //intentBase64.setType("data:image/png;base64");
    //Intent intentBase64 = new Intent(Intent.EXTRA_STREAM, Uri.parse(url));
    //intentBase64.putExtra(Intent.EXTRA_STREAM, bitmap);
    //intentBase64.putExtra(Intent.EXTRA_STREAM, Uri.parse(url));
    //intentBase64.setType("image/png");
//    a.startActivity(Intent.createChooser(intentBase64, "Share test image to.."));
    //try {
    //  File file = new File(a.getExternalCacheDir(),"tempqr.png");
    //  FileOutputStream fOut = new FileOutputStream(file);
    //  bitmap.compress(Bitmap.CompressFormat.PNG, 100, fOut);
    //  fOut.flush();
    //  fOut.close();
    //  file.setReadable(true, false);
    //  final Intent intent = new Intent(android.content.Intent.ACTION_SEND);
    //  intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
    //  intent.putExtra(Intent.EXTRA_STREAM, Uri.fromFile(file));
    //  intent.setType("image/png");
    //  a.startActivity(Intent.createChooser(intent, "Share image via"));
    //} catch (Exception e) {
    //  e.printStackTrace();
    //}

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

  private static void sendUrl(final HaskellActivity a, String url) {
    Intent intentText = new Intent(Intent.ACTION_SEND);
    intentText.setType("text/plain");
    intentText.putExtra(Intent.EXTRA_SUBJECT, "My address");
    intentText.putExtra(Intent.EXTRA_TEXT, url);
    a.startActivity(Intent.createChooser(intentText, "Share .."));
  }

}
