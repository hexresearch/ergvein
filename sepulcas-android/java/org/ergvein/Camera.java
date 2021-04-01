package org.ergvein;

import android.content.Intent;
import android.net.Uri;

import systems.obsidian.HaskellActivity;

public class Camera {

  private static void cameraOpen(final HaskellActivity a, String txt) {
    try {
      Intent intent = new Intent("com.google.zxing.client.android.SCAN");
      intent.putExtra("SCAN_MODE", "QR_CODE_MODE"); // "PRODUCT_MODE for bar codes
      a.startActivityForResult(intent, 450);
    } catch (Exception e) {
      Uri marketUri = Uri.parse("market://details?id=com.google.zxing.client.android");
      Intent marketIntent = new Intent(Intent.ACTION_VIEW,marketUri);
      a.startActivity(marketIntent);
    }
  }

  private static String cameraGetResult(final HaskellActivity a) {
    return a.getResultScanQR();
  }

}
