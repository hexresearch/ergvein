# wallet-types

## Migrations

We have a system of migrations based on [safecopy](https://hackage.haskell.org/package/safecopy) package.

Basic types for which migrations are enabled are collected in
``Ergvein.Types.Keys.*`` and ``Ergvein.Types.Storage.*`` modules.

Let's say we need to add a new Boolean field to ``WalletStorage`` record.

### Stage 1. Rename the previous version
Before:
``` haskell
data WalletStorage = WalletStorage {
    _storage'encryptedPrvStorage :: EncryptedPrvStorage
  , _storage'pubStorage          :: PubStorage
  , _storage'walletName          :: Text
  }
```
After:
``` haskell
data WalletStorage_V1 = WalletStorage_V1 {
    _storageV1'encryptedPrvStorage :: EncryptedPrvStorage
  , _storageV1'pubStorage          :: PubStorage
  , _storageV1'walletName          :: Text
  }

instance SafeCopy WalletStorage_V1 where
...
```

The suffix is V1 since the initial version is 1. Set the suffix according to the version of the previous datatype.

### Stage 2. New schema.

Write the new data type. The name should be suffix-less
``` haskell
data WalletStorage = WalletStorage {
    _storage'encryptedPrvStorage :: EncryptedPrvStorage
  , _storage'pubStorage          :: PubStorage
  , _storage'walletName          :: Text
  , _storage'someBool            :: Bool
}
```
Write SafeCopy instance:

``` haskell
instance SafeCopy WalletStorage where
  version = 2
  putCopy (WalletStorage e p n b) = contain $ do
    safePut e >> safePut p >> put n >> put b
  getCopy = contain $ WalletStorage <$> safeGet <*> safeGet <*> get <*> get
  kind = extension
```

* Use ``safePut`` and ``safeGet`` for fields which have migration capability and regular ``put`` and ``get`` for other fields.

Since the new type is an extension we also have to write the Migration instance where you describe how to transform one data type to another.

``` haskell
instance Migrate WalletStorage where
  type MigrateFrom WalletStorage = WalletStorage_V1
  migrate (WalletStorage_V1 e p n) = WalletStorage e p n $ (someCondition n)
```

### Stage 3. Finalizing

* If changes to the schema require you to add utility functions, do it in the "root" module, i.e. ``Ergvein.Types.Keys`` or ``Ergvein.Types.Storage`` modules.
* You also have to explicitly export all new lenses. If you are not sure how they are spelled, turn on ``-Wunused-top-binds``
* Lenses are required only for the newest version
