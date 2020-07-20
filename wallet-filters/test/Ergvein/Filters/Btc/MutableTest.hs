{-# LANGUAGE TypeApplications #-}
module Ergvein.Filters.Btc.MutableTest where

import           Test.Tasty.Hspec
import           Data.Maybe
import           Network.Haskoin.Address
import           Network.Haskoin.Block
import           Network.Haskoin.Constants
import           Network.Haskoin.Transaction
import           Control.Monad
import qualified Data.Serialize                as S
import           Data.Text                      ( Text
                                                , unpack
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Encoding            as TE
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base16        as BS16
import           Ergvein.Filters.Btc.Address
import           Ergvein.Filters.Btc.Mutable
import           Ergvein.Text
import           Ergvein.Types.Address          (btcAddrToString')
import           Data.Foldable
import           System.IO.Unsafe (unsafePerformIO)

import Debug.Trace

spec_filterPositive :: Spec
spec_filterPositive = forM_ samples $ \(block, txs, as) -> do
  let bhash = headerHash . blockHeader $ block
      bid   = blockHashToHex bhash
  describe ("block " ++ show bid) $ do
    it "block filter encodes-decods to same" $ do
      bfilter <- makeBtcFilter btcTest txs block
      hx1 <- bs2Hex <$> encodeBtcAddrFilter bfilter
      bfilter2 <- either (fail "decode error") pure =<< decodeBtcAddrFilter (hex2bs hx1)
      hx2 <- bs2Hex <$> encodeBtcAddrFilter bfilter2
      hx1 `shouldBe` hx2
    forM_ as $ \a -> do
      let at    = unpack $ btcAddrToString' btcTest $ fromSegWit a
      it ("block filter contains address " ++ at) $ do
        bfilter <- makeBtcFilter btcTest txs block
        -- traceShowM $ bs2Hex $ encodeBtcAddrFilter bfilter
        res <- applyBtcFilter btcTest bhash bfilter a
        res `shouldBe` True
  where samples = zip3 testBlocks testInputTxs testAddresses

spec_filterNegative :: Spec
spec_filterNegative = forM_ samples $ \(block, txs) -> do
  let bhash   = headerHash . blockHeader $ block
      bid     = blockHashToHex bhash
      at      = unpack $ btcAddrToString' btcTest $ fromSegWit testAddress
  describe ("block " ++ show bid) $ it ("block filter should not contain address " ++ at) $ do
    bfilter <- makeBtcFilter btcTest txs block
    res <- applyBtcFilter btcTest bhash bfilter testAddress
    res `shouldBe` False
  where samples = zip testBlocks testInputTxs

spec_specificFilter1 :: Spec
spec_specificFilter1 = do
  describe "filter encode-decode" $ it "idepontent" $ do
    let filterHex = "0000000000000015024a80000098066800004cd26bc68db000000000030cdd70db52ff43c6000000fa9ea8d5fac400001dc73c000000000137e4b8d5e647dc38"
    hx <- fmap bs2Hex $ encodeBtcAddrFilter =<< getFilter filterHex
    hx `shouldBe` filterHex
  describe "block 000000000000017c36b1c7c70f467244009c552e1732604a0f779fc6ff2d6112 generate filters" $ do
    let bhash   = headerHash . blockHeader $ block1
        bid     = blockHashToHex bhash
        filterHex = "0000000000000015024a80000098066800004cd26bc68db000000000030cdd70db52ff43c6000000fa9ea8d5fac400001dc73c000000000137e4b8d5e647dc38"
    it "generates right filter" $ do
      bfilter <- makeBtcFilter btcTest block1Txs block1
      fstr <- bs2Hex <$> encodeBtcAddrFilter bfilter
      fstr' <- fmap bs2Hex $ encodeBtcAddrFilter =<< getFilter filterHex
      fstr `shouldBe` fstr'
  describe "block 000000000000017c36b1c7c70f467244009c552e1732604a0f779fc6ff2d6112 filter tests" $ do
    let bhash = "000000000000017c36b1c7c70f467244009c552e1732604a0f779fc6ff2d6112"
        addrs :: [SegWitAddress]
        addrs = fmap loadAddress [
            "tb1qjx8u3dz6dnxcnwmpwdcd2c8hzugzt8jap9enpu"
          , "tb1q0fql9yduelq8lxyrlrajlxewj09zg2st3ufyf7"
          , "tb1qms2h0994zyywf2jvr6gez4nwtqg8xc0fz70260"
          ]
        negaddrs :: [SegWitAddress]
        negaddrs = fmap loadAddress [
            "tb1q8tkcrwr0ejssk4xhchmge0dmpuqvd3rdu93srh"
          , "tb1q2z49tgch3fdjs7ye9swx5t6n824zqh2zaazw94"
          ]
        filterHex = "0000000000000015024a80000098066800004cd26bc68db000000000030cdd70db52ff43c6000000fa9ea8d5fac400001dc73c000000000137e4b8d5e647dc38"

    forM_ addrs $ \addr -> do
      let addrstr = unpack . TE.decodeUtf8 $ encodeSegWitAddress btcTest addr
      it ("has address " <> addrstr) $ void $ replicateM 1000 $ do
        bfilter <- getFilter filterHex
        res <- applyBtcFilter btcTest bhash bfilter addr
        res `shouldBe` True
    it ("has any of prev addresses") $ void $ replicateM 1000 $ do
      bfilter <- getFilter filterHex
      res <- applyBtcFilterMany btcTest bhash bfilter addrs
      res `shouldBe` True
    it ("works when not all match filter first") $ do
      bfilter <- getFilter filterHex
      res <- applyBtcFilterMany btcTest bhash bfilter $ [head addrs] ++ negaddrs
      res `shouldBe` True
    it ("works when not all match filter middle") $ do
      bfilter <- getFilter filterHex
      res <- applyBtcFilterMany btcTest bhash bfilter $ [head negaddrs] ++ [head addrs] ++ [last negaddrs]
      res `shouldBe` True
    it ("works when not all match filter last") $ do
      bfilter <- getFilter filterHex
      res <- applyBtcFilterMany btcTest bhash bfilter $ negaddrs ++ [head addrs]
      res `shouldBe` True
  describe "block 00000000a23765a02274f860841fef247233bd92b3cd98f6ee40f513937129c4 filter tests" $ do
    let bhash = "00000000a23765a02274f860841fef247233bd92b3cd98f6ee40f513937129c4"
        addrs = fmap loadAddress
            ["tb1q6np2kl9j7w5lrhw4wccqzh8gdgxapvwtdu46eg","tb1qjx8u3dz6dnxcnwmpwdcd2c8hzugzt8jap9enpu","tb1qg0etkg3447p0d6vce3dxg8czr2zpnt203n67ap","tb1q4r9qsfugzz8zvfw00a9ez9mhhpal20lvnz06aw","tb1qelkhsjgm4y6mlfn9tcc80nsuj9sjavp27v4sv2","tb1q8cv0j5mhcc2mv9ugqacqpmq6f7w8749fsh75te","tb1qrr8ztdep2zg9pz8udlfvtjqr6zura9uanxhr3h","tb1qtaufhn6ryjeq37epkfn03crmsjuds7kuhs7tqv","tb1q2zlt086sqps28740gekn3pejcr47e3hcau3qgw","tb1qtd5d7evhhjumm9vv04e690pdm8xrhxac2cu9md","tb1qthrcedzd34r5mz0ycwqtp0y4yk4s4y4p4a6d4n","tb1q4q457pyf9g5ngfpxm4tftlg06vckkdcrmg3gtl","tb1q9yyvzhek2mzdqv4j7za7pawrn2vqruqn2f0xwq","tb1q68r5uk5e45vnqecc5ayf92drcdy0d4fmrxjrrv","tb1qhuvzhvjmtspqxd69vl9y27k9clp5yrqufnywgq","tb1qkp0qdcwz533d397pa402vkrvqupa75m2ukadwu","tb1qt5p89dkhxq050uld0te4m23yu9nsp9dk0raewx","tb1qrudkcdae3jgww8xd6ydcrum4e7d83lv7nat0nd","tb1qjq3cyuzwmngzn8r85w87skxz2c9zyl32lvjjks","tb1q5n8vuaq8f0yrxa7vkfsqctfgqlfp6c83exkgge","tb1qwn2u4pkwsuu9gffv33c9gwepdz2m33rlvn3vcm","tb1q8vz4xa8gtxw55ct6ze6h85e8w5y3u9puccydus","tb1qxwaqacv7acjfrjjernxrtkgvgekxsuzz80cmak","tb1qs8vj2jp8a9astkt8qqq5g5yj4q25z2gn36mdey","tb1q700ay3f0pqjfsnv3ypyfpr7ghvx0jhr0x90njz","tb1qz7a4lqtnrr6dm8ztglq0acafmmf5f4pncwk2kk","tb1qj35g4gs8gtremtqz2t60am6xexm8p0qpelyz83","tb1qp8ckmhd9sjf8fky90560va77q9gacdkrwd98ts","tb1qm4gexe3q9sw48fazmfcqmxfv6fqwfe5tj9cp4r","tb1ql4xdr2jr3zzjn9lxsdwzjvg3t9q2s8l24zg0j6","tb1qxfpxygp7vrrczpw98fgmykdme504nzw9teshlx","tb1qgfqpymsqt8pqgr329xx8xdsj5vlcezh28hcxhu","tb1qjr4e3u3x9tuw35zsy5yqyc8nvktxwmzxsfnejl","tb1qyx8se3ne5l3hfkahss9lzcpt47nr2nfxlv06dt","tb1q8046gny6vnhr6sdyta2cww48zsm55cjh8yzt9n","tb1q8pajpyux77mzc3ru68ck0eazrn7nrqmjcht8l6","tb1qaq7erxv6ph9e7gzkdt23zplahxrnuxkcgsunpl","tb1qauw2gcees2kn3tzrwj3shz43pzzjqdnun7n9kr","tb1q0pgzdc4dsmkl8e9ctrdguma3xtyy6lqlre2ytr","tb1qqa2y7ynak7qcn9g55ras4qcv2xd4p7zxxsp6x0","tb1qjv5725suqy0untvsgtjyf35c9d9p498yqcghy0","tb1qxrqyw7ycs5w8u2fyzgycmmdurv4snwrlxrwyw4","tb1qsyr9zlk97tpruyr65pt4necmyezpp34u0gttdc","tb1qe7dn7607py20hhy04msqjhc7jc25zyrhfzq5kz","tb1qm46s63fa3xf7m3lf998duahp8fqseyj2v4hx7d","tb1qyfqetcg94g8lwm96h6y2h8tn9s5r3rl94hz6rj","tb1qaaerc67qamc2g4pplz0ex969p87lutjy39lne6","tb1q4srewz3u3hrmleywxcguzjwxkdytwtkwm4qhk4","tb1q7s9zta79hrzrwhcuk2t6krgys9ey3uedcya0hu","tb1qyjyq2d3mvzm8y07gydsredlsr65dfkdge4z7ed","tb1q2uk8vv54sfhkjt6m7zxvdu9y7sthqn5w8lshh4","tb1qwgz5ff07vhs25yx7jny5wsldrmus8u6at85cle","tb1qc037ylsk37q95ym6lrc5w2h2cmugxg7jwavttf","tb1qgmc5syxe49w696qgjq2azmfr00q5wn8c9qlkft","tb1q8ra5rznzws85yh3a42vegpx9wfccu6v3ucae68","tb1qs60d9lzrypwtrcuc9uh4h96utlw7p727snphtv","tb1qjejrqpkgv6kum54snjlwe3hj9tug705fx5ru3j","tb1q67ctqwz4kvrd3xah5pty9r9d3fgyjxl0a8tw49","tb1qxsnjrtkjegtygc9wznawxrq70rkyex0ace4q9c","tb1q65pk5ssknc2ut67y3jp749u4fsuy395sawa2vz","tb1qmdrzrmsyn4dvsket0sezj7dsqltm62tts609t2","tb1qrm5xfau08kd5wnmh2yu9lfmfzcgcvpkkq7lxfh","tb1qclz8nnuf3cw9caxt709c8mkrqa6j049gm03evh","tb1q3j0cfgv57slmh6gu9xckwt7fwxayv567j7ctvn","tb1qd9s324p7cg8ssdh70q3mzskp50qayjfu68uy6d","tb1qgchg72f7c5uvklp4jgskhhunvlkj6aq8jlt4xg"]
        filterHex =
          "00000000000000e32c544012d9dcda9018b23997122067e1ae2d01b19c37d6b1c08300000eeb34a1f65cd034f3cc4fac2f90000060278a42e4a3309a000005756c621e000001367b1013ece3589dfe2951f25551d6ff70ff240df66da9f17f17989b211a000000167080000334ab0000057c0e25f00000000f22c627b78d6620388d2a30c000001162cae28ffa8c3d909567d0ac5fc12cfde960747631f64684d84a17bbf5800000701bf0f343f465f50ba18ad1dfdf242b50f7f75ce4b244cf25397094be01aed815e9f6d2d2bbad4fd09a10fa3fdf2a9400000c15a49f706293e27dba318d3c9eee78000015427be5d2afba6aa9b3800000ac318000071bdfea66bd85d82086cd80000774058efae43fb06e9e8825c01f193379f88a8e8000007b9931da10da14d316680bfc44374ba800d410205e5d8743d6e00000e2a0f78358295292e3352fb06ca9550c00000501a18d5f67a2acf917e00000063e61ef6607e13aa7b1f055f8112942ee4f1e4000000000000001d99664000006ac683ddb40000229d626bdc87cd560000066ef28bd1a9f717ee5c2b917bc6009f0a70e2495c5c4a87c00002ece81ffe01767ea5d4400001c4e7e40000288bdf3108399650800007eb1c257f758fa0fdc62e6b643380db079c06f6c00000683174ff6cb91c09285c9e7a468b5bc45e2bce648fe21ca2bacbc11c26db87c141aa8797600000b36cd467d3000009b38f037e9972755482b295c89a88717d8b074e806bfa4f8b48a8733f47a8005e41a454b6de09a4937d108839522cfb9fd36e9b929b2d264549a5d2d5d0e0de1200001f640f5da8c160e58c700b383fdecc1077c300840"

    it ("filters given addresses") $ do
      bfilter <- getFilter filterHex
      res <- applyBtcFilterMany btcTest bhash bfilter addrs
      res `shouldBe` True
  where
    getFilter :: Text -> IO BtcAddrFilter
    getFilter v = either (fail "Failed to decode filter1!") pure =<< decodeBtcAddrFilter (hex2bs v)


testBlocks :: [Block]
testBlocks = fmap
  loadBlock
  [ "00000020ddbaf75407886c86a4cae8d6daa6ef0d1102a99d7bc17d4b100c00000000000003c1dc70732b7c6d91128276479e3e0375573d4a3f2173b721c5c035e81e0e83a2ee005ef0ff0f1a998a1b0305010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff4c035ef11804a2ee005e08fabe6d6d000000000000000000000000000000000000000000000000000000000000000001000000000000001800000172a502000d2f6e6f64655374726174756d2f00000000020000000000000000266a24aa21a9ed1b13a82aa3ab4e6d6ac153d82d8c7d8f1c532ef01b3ae542c8d64c8d6b325681e62d5402000000001976a914bd3400d71504033fb1e7c947f2e0f55775899f2288ac012000000000000000000000000000000000000000000000000000000000000000000000000001000000019636f66114de423258593c8d41ab2762d95e22c9993c240474d0ba7cde2ef452010000006a47304402200c68ce930269ea92f0829a6a6d167a76b18c9fd8fb8ee5133b2242c7eb63ca7302203854c58342583e6f4d04ef453f10f85a2a4d0c8706090741dff97a49e0e9f57701210345d09b2853f985eda2766a8847fb671fdce8b48a07d8fbb2aa48a2f4f1aeef6bffffffff0240420f00000000001976a9144ec06d007995a2bbaf0251c100eaa083417ee6d388ac96185501000000001976a91475008b6030eda94db83f1d2fb28cf6577d87c2bc88ac0000000001000000000101c81efcbeadb14f515c8fb56913decd896cea458eb6e36369dc80858d4c492ca90000000023220020a344d5ae22e2e041d2269dce688a2f00574722cab0876afc68e1487c7350f64affffffff01b2501e000000000017a9145f360b7ee84d314c2e567b7f64ce28d4cdbfc79b870400483045022100b3ff5890042d586aeaad17f12b5909868e270fc94d2ea40e43506acf20f480080220784966f22bad10732ebc777cb4e0cfaf1386e7ac9c08e1463168735ad91f5bb60147304402200a79ea5c2dac97041e672b44c1cc239af6065982563d69dc84e1b473b115f72b02202a4355029cc8291266cf39e85ac98aa4dedb3b897162ea146a674b4bcfa581c501695221029c3b8946a69f9c5b82b93cf89544d5b56bdde6f663c31dd777111474d50d87af21026e476676e9c8092009e551067e7dac8c057cd34b6b552b56078d76d927a3130d2103eb27f329224f32cb7837ea87a32947affbff7068e133a5b682934a7cbd04848553ae000000000100000000010158215c4790c0952809f0f07a5b01457d3384998079f7e7d32baf418bb1ba64400100000000ffffffff02ae5a19000000000016001498c980338eedc2938c0f47c7af536138866dc39d0000000000000000536a4c500002280700029ee6ec7bed5faf88db46796474ac668de79ec5c018c7e0ae230a4ac3afcd2ebae7b6f26189842b8ac8a41d895e6c5e00ee930405f5e102844bda4f75662be062df035a8aa8c6f56dba3002483045022100fc3153decc81de90e290657dbb2f269bbb3d7485961bd6bb27f69844a8eb9bde02202e0c703ae9cc6367914a7ba04f95414de017a45b51eea63e9c2659c9ba8b5d1b012102916c6da5139821053bd5145a052d5bb803da5b588425882ddce9c9194afb722a0000000001000000000101adb24d3b82f21c9a974c7c5c6216d28d681d6632d7418196187e2b05543392050000000000ffffffff02ebf734000000000016001454e850ff81fdc8ff2ae88c95d5e3d73f611ac6bc0000000000000000536a4c500002280700029ee6ec7bed5faf88db46796474ac668de79ec5c018c7e0ae230a4ac3afcd2ebae7b6f26189842b8ac8a41d895e6c5e00ee930405f5e102844bda4f75662be062df035a8aa8c6f56dba3002483045022100ef5f2d3f01f90210cf9e9e65ca75ca1264c5c6f76e6f9d3537dbd84399c9946302206a6fff623253a8708a446100fb8c1b6dddb3683eba2996647d8998c57feba5f201210259c87de12afef1bee19ed3e1380a73236a9d774a9c107fdd35e04ae346b7a57600000000"
  , "00000020eb07a47f176ffbd56d84b4e2578c3c26d4edd74c81c6704535010000000000008c9145166e8d4f2068ba3a80dcd12da6c0f5c2ccf0cb625a8d44b28b50a6cd8415ee005ef0ff0f1a12f8991c10010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff4c035bf1180415ee005e08fabe6d6d0000000000000000000000000000000000000000000000000000000000000000010000000000000018000001f46700000d2f6e6f64655374726174756d2f00000000020000000000000000266a24aa21a9edea23d92d9e6eeef92066c7681a8b0d0ec6cdb940a6f39ab608ee8450bbdd6fbd532a5502000000001976a914bd3400d71504033fb1e7c947f2e0f55775899f2288ac01200000000000000000000000000000000000000000000000000000000000000000000000000200000001de935db2565d16d7ea82494aaee743e607bf3ab1ce80b88d0db48ab69b76bd80000000006a47304402203e532fa0ea419fb976e2e9f380f52aaf0b3be895e464c9954beee4ff7991661d02203900cb738b8a1c78faec11e39105ac2a59dc1f7d110202ba8224a454d64afadc012103c9f4243d6edd131d9e7178bcf77d35d64b7400714f7fa520631d5e7e23231a59ffffffff01a08601000000000017a914bfee073b17632933992f145f865c1d7e6c278bc687000000000100000001ffead9923c76a364819af0c39150dc80461b4a2aed15588070854e02ebcde03a000000006b483045022100b86b17a1a74768d9bc3a47317eae1c247e0fd9fd4f1f53c281c0d868413d23c502200ee1df6faa41a6698be3d05206927d4ee03a672d7c0e8942fccec9da3a86ad80012103fc910717da9fa2e50a85c2b219de10f24614c8abc00dada5582caddd14b81464ffffffff010cd90000000000001976a914cd77154097c250a02e6880fc60440319cc241f2f88ac000000000100000001986d2f2c27c0dfe067f69ec52847a508fd7e6db3d135412722d83e6570be1cf1010000006a47304402204fae1d2ae995ce90402f709cb3f4291072df42a129b570c4e4d38381252fac1f02206053c574bb0888f1c3c1b0d979f86bb3825b4123f75302ca24f8fe306ec3fc2c01210339ec342e96b80604b2d4da2d2379ae55e5b1bff7e9fd578d9c2978365e3988bbffffffff02e8030000000000001976a91479d56dafa61cc0a476130a2e7963cd900230fe8c88acc4020100000000001976a91422bfed2b3336ee1a15426fbaa794d583ddd7351288ac000000000100000001dfd804c09168f34d0cd9656d610fba5c0efa749f6d1ee3c6a5692dca829c4e99000000006b483045022100e6a756f24b3f7de206908e77ecd9458b5cc20d494de8f5816ad419f016f049a402201b71b023dbf6d625b0748bb356ab4fa71732cb4117a6edd6951575f95a63fb8a0121035c06f446a7fda2ec010ce6ac9d5654b7f2e652eb98ecc115806ec8cb0512195affffffff01e3010000000000000e6e9302f825877b7b9402a61b879a000000000200000000010123b91dcac9e166a9c6c49d66cd837b47b511b1fa4599035859505806af65ecb2000000001716001439082f6ea75e33c7ecaa7af07aebb876b255c608ffffffff018f7606000000000017a914591d59329645a4ef7ea9dbbdc74c1f10eb5a7ec98702483045022100f914d6c2c10e8bf9182981dd04ec0f837b688d78cf111ac25ec3f5176ab0861102200364f378daecef8f8f4532c12dcd6a19b151a4c9fe8279c3a8fb19f43bce4acb012102194d94846e42412344b131f0d57c5364b99923d4a4b4fc35cd19ce8330a6e0960000000001000000013c30d66580144ea18f0aa0ab471ff3273bd04732612df37bfaccc19d45a787c800000000fc004730440220474f2d26f206d731b817325a17d8d61805264757b7131baf7a749d1630c09bbe022025997d7307d0a5665d406dc0637c1081364d62d3d8aa730c1fbcfb1881bb9405014730440220508f4ea3381f06c53213923020d099eaf0f0e7dec4f341fdd565865f34e3216202201dde145bad50dfe033673d0358946acebd1abe1e3d4d269e33ab1f12db3c9f7b014c695221026beda6acab07cc00f09da4ffb92dfac85fe02bc808ed4ec5ffa9f28b90ad8a5221024d9cb4a4db43561ddb5f6b0f53f5c87ec6342c95e74aef8123ec790c5320079721029f148f10a33df9932e76c90f3ee771300452e43f65f79be2613f81095c02d6f853aeffffffff02e4e600000000000017a9144560e724fae3f0f8c5cc7e68fd716df3c8d2312a87204e00000000000017a91480b575753f8aaf5d1e0a23817fc1ac92d0ca6054870000000001000000018c9c699f9cf67a24ca0dc2b54320218efbc39a7733b451ec935f3e23316d6e5e01000000fc0047304402206ab90a1184aa22f1b5072798e1b61974f36ea63408af7916f39e03cfe1cc90b6022056d0dafab07ebe485af876b2430e46bac018338ae0ac538c956e43d2f0003c500147304402204706409407abf99f429d894e791962d5a95278b79f4182f545c514dc2d9a46a902203eabd5a09d8d3eaca156800c6ac77fd2cb76ab4bcabcd99cdb5662c07be83eb1014c695221026beda6acab07cc00f09da4ffb92dfac85fe02bc808ed4ec5ffa9f28b90ad8a52210207555fa6fba4f5dbbd7b9f07f353b50f7d7276cecdacba968252530a1b1b28c121029f148f10a33df9932e76c90f3ee771300452e43f65f79be2613f81095c02d6f853aeffffffff02c43601000000000017a9142777140c6119a7ee88a625ad03bfdf23a61e1ad787204e00000000000017a91460f5880eb0adba97f4c6972e65ca56b8630e2cab870000000001000000011ecd3ec7648244a0929138c9f4080e8bd7174f3013823be2eb6b87b1a56ce38e00000000fdfd0000473044022063380efc70d28332b1800a26a5be2a1f927dee61b83b88c6643d6b888d408b750220569899941e6cad4353eb8cf7f656e21e471d489cf3ad6e395c5bf254e9d7c70d01483045022100ed2f6950912ec153aa8d37c8c786bdf8838c6ee5becabf5b7b83eaadd9af00a2022047e05f591a1a96ad4c08bff5be7df12f055ca0fed04df706113ea32ee0379525014c695221026beda6acab07cc00f09da4ffb92dfac85fe02bc808ed4ec5ffa9f28b90ad8a52210256cfff35a5a034ce5567f94c3d0404a69541d294c61dcad76a56178a4f87f8ec21029f148f10a33df9932e76c90f3ee771300452e43f65f79be2613f81095c02d6f853aeffffffff02e4e600000000000017a914072891bec1a67e0c6653189bb998c58a403140fb87204e00000000000017a9140c14e0248d5aa4fc3f361049b95b9e74ff66a9f88700000000020000000001044504668dd6d4a5eb3f1def7a2c5ca7698d7526bcdf8fb5ca6b24c61553b7a4b40000000017160014b89f295422215178be46ceb1b3cf195c2d1aa5e6feffffff32f50d8892166f3284f6062d6cf5bb6f12f30d97443941addc32fa215e05f89b0000000017160014bbd10443276169ff5026c0ba31f05c9296f6be37feffffffd4012c1baa4d3ffa8b577ccd730292056860c864d199d102595f20c132572f780100000017160014fb183d5794e4af0bcdb0df3617ecf5213f787ceafeffffff472d79ce48c6b043de65688fe1dc728485ab6a9d9aac822b67b945e891ebe08200000000171600140a4eb3c1d49d078a1a345d4bb40531d5b667eb43feffffff01892614000000000017a9147da55b537305b44e843e575e1d107e59e104fc5b8702473044022051a62fde7bfa537388671d2996761a24dae91f01e1b13dd239c68422d9718ae60220628689122f2a5ca15fb3bb39b9d5bd324d5a94132afa41d028b9784c7c11acde012102787a9786c6ff05949fc61562da23864f221c987618b5cfc90390e91dc644716b024730440220482a6a2337fb7b8a461140250a66ca374130c7dc26593c9c4baabb719d4c4f5b022043fcdf35c0390f69516996c412e84463452e1238239e458801886948308bd42c012103d85a97cdeb4b68663ded2eb896c3d00606d21eca0f3b07913e47340af4d5308902473044022049e4a8734ce467edbd97175afae59c3f7d3389d2390fb3e20ec7b720ea36754902204bc7ae03d74661f9b8b6c2982144577b38e0343a1b04ae06507c07fc08c6d1b5012102e3c6b76c3600a0a87720b0c7f875303f9b7a983fe6503891993aa720bb5bd83002473044022050afa4686a33a06413c097ab9f57a8e0577d8dca7765d11a3bda20916e40510702205b0f8fe67424426b5fbe8c8be0cd6247b6222d5cccf0c995415f10f7c624dfe6012102e1c40947e6d9d71e646722fa3f431da46704d557c214dacc276f0430c092dc4a0000000002000000000101105d1a09506d6075dffcef083561b0c0dc84df46f72b9037061d29f223995b320e00000017160014c9da660fd4e1e8e5d65a1f57090e12eca383d4b3feffffff18a08f3e000000000017a914e4827d37aea3d5e95654699ae1464336dc7e2f9287a08f3e000000000017a9140b6de5735af02a9db31a4bf63ca2d3c0388e2e3587a08f3e000000000017a9149af0ea693340f38c93118d74f80c38dbecb0a4dc87a08f3e000000000017a914b4b93aca8c6c1b77cead0e38fc4aa5843d477ee387a08f3e000000000017a91415b208443656e57a4aa17cb7102dcfc9aebbd55287401640000000000017a9148a08adddc282fce1c932c8d97fe001c0d1384a2c8757c384a4a900000017a9148251a5ea79264a2148a48f7ee18259cc5846109a87401640000000000017a9148c47abfbaa14192032ace186b17a5c741bd4b56087401640000000000017a914348a69313f757f46e549d90f63b991b1f96bca4287a08f3e000000000017a91406b52daf41d556bc0dc6a069203f45b4b58e321d87a08f3e000000000017a914b6ed3aaf30d227300943fd32ac28c34c352ab6ff87401640000000000017a914c6f585286aeb00caab2ca1e03d7a3cdeaf2fc02487401640000000000017a9149256171dd1daec5e1db3b7c5a5a0a059f2dd149087401640000000000017a914988766a98fd3e28299752fd1e7685fc0965f61dd87401640000000000017a91417ddd07d4122e58cda44265aa303c6833d1687f287401640000000000017a91416685feb262b633d46af1536cecb7f7c39132be887401640000000000017a91442f3fdaa8aee7be775883247d8bee11bc6a87b1a87401640000000000017a914da99690484b72299cf1f8c5970538c473e37618a87401640000000000017a9148ff45b216cda5f60310526d76f647ebb7fe635bc87e09c41000000000017a914102438a06791553c01774c041c61cc2db099dcf687e09c41000000000017a914d8ed8a145ce0fccbe847650abcb5d2a1d0516dc887e09c41000000000017a914508628c5c7dd3c752989e03828f768682158fbc787e09c41000000000017a9140719af0e0cfb8c8f3cfe7634e8f2503daa428b5b87e09c41000000000017a914b5ed644cb29594a1715de4efb7acb566e1e140dc8702473044022020f24eef9bbba78e1c89e557d9a0f685d511016d41b48e4fcdb5686cc6615562022013f4870f2da0ed648e1c1bf9ff5632300ad58f3b89f1fb3b0d3f17d1e58626f9012102c0590d01ad07ee4fc708867d51b3aa07b7d2859fec25091f5b7af43428dd29905af1180002000000000101de935db2565d16d7ea82494aaee743e607bf3ab1ce80b88d0db48ab69b76bd800100000017160014687763bfd061a6490ee5e7437c14f6a53d7e4a9afeffffff02f049020000000000160014eff59981ea90768679af3e25eebe6839bf3ce443a66ca30000000000160014fd4c757ba1de077a4ffbf50369f172276a95408f024730440220596caca110909c020c98ce556b0bd96b8c2ae72306e99c276b4ae0802b69a1690220609e33b39bc81a71731bfb5821494e455111e6bfe03896b41f5dd0e751c4199d012103fef4f74c8abf9383f8891db2d92fe0ff0edfe8d4976c9a454fda62de76a768a9f8f018000200000000010154e8f69ca4ab40fb7c06666bc360a222a9b8d77dae8b96db1784d39f52693ad50000000017160014cd1640974c7a763056b4ed3d7d36328c01ead3c9feffffff023035ff010000000017a914378d12458fe149a8b8d2fbd838e2cd97d71dfb0987102700000000000017a914e76397043df60b2f9728209be28a5592049ce69487024730440220467c3764ab6cb798265ba3693272e46b605a2b0777ec3c63bf45103bc5e66e7f022058d58d6254fbb16e35849ffd0e0e20ad2330699712ab1d4b443d3de144c65c2601210206404a43c3c1f75322d05b69ad89c058f6a68222b18d5517642c3209cbaf7eae5af1180002000000000101e096ace143db5c1a6b0144c3f55e027dffa28840eb3b3ee37bced7acd0bbf0c30100000017160014f98e50091deed92f5c83f51157fea95296a9c336feffffff027d2061010000000017a914c19c461776dfe8585ba0a0536509680726e30f3b87102700000000000017a914c695341928e6e887447f006345be943cf368cb58870247304402201972361fa103e81df70e85a39a62e86c7f3019684250e201c4e4edbac6cf9b1e0220690b2f4b0549e86ffd6494bfae331cde920f46c15f64f2ee197fc5e095bbb9760121035058fdc67be79f003451b1627dded2d09512564843fa89500718d00b28c872da5af118000200000000010151d925bcb17a8e2f10d9b9cb49e2e114931a46b663a1b1a369c3b345dbb0f7950100000000feffffff02102700000000000017a914c695341928e6e887447f006345be943cf368cb5887f9747c000000000017a9144660975e7a3cdf9d59776f101119a3d0eb0b09c7870247304402206b55143ca18cafb7cad445d7edbe3fb0be78fc4a264ca0b777b8ff8f702d49f00220020a3e60b5134fa7407198528d50dd4a0613af95bfbe797d4cb21e8a56fef55c0121024971d3abb3bda9aec650a39b06de2eee131392a8ada7676ca034247630a177075af1180002000000000101d21846f4940f6c3c384c28c5534df668705e7f0615a6a2e1b5a7b1eab943aa660100000017160014f06ba492252dd589912502031e4ee55ca5dc5c37feffffff02306f01000000000017a9143caec215067f2772525a290f2b4a3e372f84b06f87dd92c4020000000017a9148d0e3dcb7f6ada3cf156e0521523822f24ce5fbc870247304402207f89fbe2f37cdaf33fcf53394d2b84b28c6fca832c4e3e86634d643d818e4ed602201091871f1f65e5250823d7354440fc5e54e8eff4ad34a996ab8f11a38b3eac8d012103c2352378518d333c910d23413da7d8b1949dc5fb6df141b76e7d2d11c90cfcf159f11800"
  ]

loadBlock :: Text -> Block
loadBlock = either error id . S.decode @Block . hex2bs . T.filter (/= '\n')

testInputTxs :: [[Tx]]
testInputTxs = (fmap . fmap)
  loadTx
  [ [ "01000000000101cb6d6ca7e36725d98592c142bc8e54b53e81d1079d0a45fca91ae9640f4faf2f0000000000ffffffff020000000000000000536a4c50000228120002ca4f86db7d73e73ba71e587a6b46f7ec375c2fecc374ac668de79ec5c018b1cfc9a811cf32c7ddfd8f1c31bfb5db5e00f2230405f5e153929d764f75662be062df035a8aa8c6f56dba3094591900000000001600148765bf25275f6e034e5c61cfadf7a76a7e5dbca90247304402207f1cdcf37a5f7fb04a2e989f390bad38dd31536899388fcb39ea877ac662a39a02205835f15cc37d89a3f92cf3a0a006c27bb662f2eda93791a66fd31b97069cdd24012102916c6da5139821053bd5145a052d5bb803da5b588425882ddce9c9194afb722a00000000"
  , "01000000000101f54d73a0eb37ac94f4d630d11dd5665bb7762affec377de13a55bc0445e182350000000000ffffffff02d1f6340000000000160014728227dfd4dfe62eb788fac48917df9ff235fccf0000000000000000536a4c50000228120002ca4f86db7d73e73ba71e587a6b46f7ec375c2fecc374ac668de79ec5c018b1cfc9a811cf32c7ddfd8f1c31bfb5db5e00f2230405f5e153929d764f75662be062df035a8aa8c6f56dba300247304402206d747cb0da86f6a140c5e685f4c926932ced93bcd3726b68b0d9f50fbe87682502207cadaabd39882109c76302d6eab003b87ca90d81ed15f2231b7c17e9bf13e36b01210259c87de12afef1bee19ed3e1380a73236a9d774a9c107fdd35e04ae346b7a57600000000"
    ]
  , [ "02000000000101cb928b3d568186d4a167167ba8959e253a320b92499ae926962d8ffcb985dc0001000000171600143332a0772b4b9ea8f84a5fd0a62df51f6dfacf7cfeffffff020852000000000000160014cf6d0a6f8b1dd2a1fc53147fd3ae59edad0099f8989c7c0000000000160014161a71c89d7baca9420212081b5ed5e3fedd349f0247304402207a2805836f7446c434b883015e88a4fa6f1dd6bf67f7f3f7a797f64aecc45b1002204d1b245194d9c0f670210a79e412957452f9ce5fd4a0585c4d6c3d2c1dfeec8401210315ecc79737cc7a484615e8e287d7d53d5166e019f52dbc6387a7c4c2cb27ca1557f11800"
  ]]

testAddresses :: [[SegWitAddress]]
testAddresses = (fmap . fmap)
  loadAddress
  [ [ "tb1qnrycqvuwahpf8rq0glr675mp8zrxmsua32u482"
    , "tb1q2n59pluplhy072hg3j2atc7h8as3434uw4wv02"
    , "tb1qsajm7ff8tahqxnjuv886maa8dfl9m09fm674pe"
    ]
  , [ "tb1qal6enq02jpmgv7d08cj7a0ng8xlneezrz5g98q"
    , "tb1ql4x827apmcrh5nlm75pknutjya4f2sy05l23yc"
    , "tb1qzcd8rjya0wk2jsszzgypkhk4u0ld6dylga9ypz"
    ]
  ]

testAddress :: SegWitAddress -- that isn't containted in test blocks
testAddress = loadAddress "tb1qw508d6qejxtdg4y5r3zarvary0c5xw7kxpjzsx"

loadAddress :: Text -> SegWitAddress
loadAddress t =
  fromMaybe (error "Failed to parse address")
    $   guardSegWit
    =<< stringToAddr btcTest t

loadTx :: Text -> Tx
loadTx = either error id . S.decode @Tx . hex2bs

block1 :: Block
block1 = loadBlock $ unsafePerformIO (T.readFile "block1")

block1Txs :: [Tx]
block1Txs = fmap loadTx . T.lines $ unsafePerformIO (T.readFile "block1-txs")
