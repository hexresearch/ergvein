package haskell.x509android;

import java.io.IOException;
import android.util.Log;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Enumeration;

import systems.obsidian.HaskellActivity;

public class Certificates {
  private static ArrayList<X509Certificate> certs = new ArrayList<X509Certificate>();

  private static void loadCerts(final HaskellActivity activity) throws KeyStoreException, CertificateException, NoSuchAlgorithmException, IOException {
    KeyStore ks = KeyStore.getInstance("AndroidCAStore");

    if (ks != null) {
      ks.load(null, null);
      Enumeration<String> aliases = ks.aliases();

      while (aliases.hasMoreElements()) {
        String alias = (String) aliases.nextElement();
        X509Certificate cert = (X509Certificate) ks.getCertificate(alias);
        if (cert != null) {
          Log.d("Certificates", "Adding certificate " + alias);
          certs.add(cert);
        }
      }
    }
  }

  private static int length() {
    return certs.size();
  }

  private static String getCert(int i) throws CertificateEncodingException {
    return Certificates.formatCrtFileContents(certs.get(i));
  }

  public static final String BEGIN_CERT = "-----BEGIN CERTIFICATE-----";
  public static final String END_CERT = "-----END CERTIFICATE-----";
  public final static String LINE_SEPARATOR = System.getProperty("line.separator");

  private static String formatCrtFileContents(final X509Certificate certificate) throws CertificateEncodingException {
    final Base64.Encoder encoder = Base64.getMimeEncoder(64, LINE_SEPARATOR.getBytes());

    final byte[] rawCrtText = certificate.getEncoded();
    final String encodedCertText = new String(encoder.encode(rawCrtText));
    final String prettified_cert = BEGIN_CERT + LINE_SEPARATOR + encodedCertText + LINE_SEPARATOR + END_CERT;
    return prettified_cert;
  }
}
