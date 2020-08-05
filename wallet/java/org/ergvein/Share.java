package org.ergvein;

import android.content.ContentProvider;
import androidx.core.content.FileProvider;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.net.Uri;
import android.util.Base64;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

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

  private static void shareJpeg(final HaskellActivity a, String imageString) {
    Context context = a.getApplicationContext();

    // Make the bitmap from the image string
    final String pureBase64Encoded = imageString.substring(imageString.indexOf(",")  + 1);
    byte[] decodedString = Base64.decode(pureBase64Encoded, Base64.DEFAULT);

    // Store the image bitmap to internal storage
    File path= new File(context.getFilesDir(), File.separator + "images");
    if(!path.exists()){
        path.mkdirs();
    }
    File outFile = new File(path, "ergvein_qr_share" + ".jpeg");
    FileOutputStream fos = null;
    try {
        fos = new FileOutputStream(outFile);
        fos.write(decodedString);
    } catch (Exception e) {
          e.printStackTrace();
    } finally {
        try {
          fos.close();
        } catch (IOException e) {
          e.printStackTrace();
        }
    }

    // Get the uri to the image
    Uri imageUri = FileProvider.getUriForFile(context, "org.ergvein.fileprovider", outFile);

    // Create and activate the intent to share
    Intent shareIntent = new Intent();
    shareIntent.setAction(Intent.ACTION_SEND);
    shareIntent.putExtra(Intent.EXTRA_STREAM, imageUri);
    shareIntent.setType("image/jpeg");
    shareIntent.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);
    a.startActivity(Intent.createChooser(shareIntent, "Send via..."));
  }
}
