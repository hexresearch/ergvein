# UI prototyping

* Start the server:
    * ``cd ui-playground``
    * ``./watch.sh``
* Implement the design in pure HTML+CSS in ``index.html``
* You may use ``css/extra.css`` for fast CSS changes
* Alternatevly use Style.hs to mimic the Ergvein.Style module with Clay
   1. Change Style.hs
   2. ``./generate-css.sh``
   3. Sometimes hakyll fails to detect changes from the ``generate-css`` script. In this case, open ``css/style.css`` and press Ctrl+S

The page is accessible at ``127.0.0.1:8000``

Finally, transfer design choices and extra classes to the wallet.
