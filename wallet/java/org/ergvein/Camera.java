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

import android.app.Fragment;

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
