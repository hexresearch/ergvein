package haskell.x509android;

import java.security.cert.CertStore;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;

public class CertificateStore {
  private static ArrayList<X509Certificate> certs = new ArrayList<X509Certificate>();

  private static void loadCerts(final HaskellActivity activity) {
    KeyStore ks = KeyStore.getInstance("AndroidCAStore");

    if (ks != null) {
      ks.load(null, null);
      Enumeration<String> aliases = ks.aliases();

      while (aliases.hasMoreElements()) {
        String alias = (String) aliases.nextElement();
        X509Certificate cert = (X509Certificate) ks.getCertificate(alias);
        if (cert != null) {
          certs.add(cert);
        }
      }
    }
  }

  private static int length() {
    return certs.size();
  }

  private static String getCert(int i) {
    return certs.get(i).toString();
  }
}